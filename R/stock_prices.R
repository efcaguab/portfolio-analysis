calculate_stock_daterange <- function(trades, today){
  suppressPackageStartupMessages(library(tidyverse))

  trades %>%
    group_by(symbol) %>%
    summarise(current = round(sum(quantity), 10) != 0, .groups = "keep") %>%
    left_join(trades, by = "symbol") %>%
    summarise(date_start = min(date),
              date_end = if_else(current, today, max(date)),
              .groups = "drop") %>%
    distinct()
}

calculate_currency_daterange <- function(trades, securities, today){
  suppressPackageStartupMessages(library(tidyverse))

  securities %>%
    filter(currency != "USD") %>%
    group_by(currency) %>%
    summarise(.groups = "drop") %>%
    mutate(date_start = min(trades$date),
           date_end = today) %>%
    mutate(symbol = paste0(currency, "=X")) %>%
    select(symbol, everything(), -currency)
}

get_closing_price <- function(x){

  suppressPackageStartupMessages(library(quantmod))
  suppressPackageStartupMessages(library(tidyverse))

  if(str_ends(x$symbol[1], coll(".CSTM"))) {
    df <- get_closing_price_simplicity(x)
  } else {
    df <- get_closing_price_quantmod(x)
  }

  df |>
    complete(date = full_seq(c(x$date_start, x$date_end), 1)) |>
    filter(!lubridate::wday(date, week_start = 1) %in% c(6,7)) |>
    arrange(date) |>
    fill(closing_price, symbol)
}

get_closing_price_simplicity <- function(x){

  if (x$symbol[1] == "SIUGS.CSTM") {
    df <- "https://simplicity.kiwi/api/download_prices?fund_name=INVUnhedged%20Global%20Share" |>
      read_curl_csv()
  } else {
    stop("Unknown simplicity symbol")
  }

  df |>
    rename(date = Date, closing_price = Price) |>
    mutate(date = lubridate::dmy(date)) |>
    mutate(symbol = x$symbol[1])

}

read_curl_csv <- function(x){
  response <- curl::curl_fetch_memory(x)

  if (response$status == 200) {
    response$content %>%
      rawToChar() %>%
      readr::read_csv(col_types = "cd")
  } else {
    stop("Failed to download csv")
  }
}

get_closing_price_quantmod <- function(x){
  suppressPackageStartupMessages(library(quantmod))
  suppressPackageStartupMessages(library(tidyverse))

  suppressMessages({
    symbol_data <- getSymbols(
      Symbols = x$symbol[1],
      env = NULL,
      from = as.character(x$date_start[1]),
      to = as.character(x$date_end[1]),
      periodicity = "daily",
      auto.assign = FALSE
    )
  })

  dates <- xts::.index(symbol_data) %>%
    as.POSIXct(origin = "1970-01-01", tz = "UTC") %>%
    as.Date()

  symbol_data %>%
    as_tibble(rownames = NULL) %>%
    mutate(date = dates) %>%
    select(date, everything()) %>%
    # Sometimes the API returns several values for the same day, keep only the
    # latest one
    group_by(date) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(date, ends_with("Close")) %>%
    pivot_longer(ends_with("Close"), names_to = "symbol", values_to = "closing_price") %>%
    mutate(symbol = x$symbol[1])
}

calculate_currency_conversions <- function(currency_prices){
  suppressPackageStartupMessages(library(tidyverse))

  from_usd_rates <- currency_prices %>%
    mutate(to = str_extract(symbol, "^[A-Z]+"),
           from = "USD") %>%
    select(-symbol, rate = closing_price)

  to_usd_rates <- from_usd_rates %>%
    rename(from = to, to = from) %>%
    mutate(rate = 1 / rate)

  bind_rows(from_usd_rates, to_usd_rates)
}
