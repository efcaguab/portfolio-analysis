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

  symbol_data %>%
    as_tibble(rownames = "date") %>%
    select(date, ends_with("Close")) %>%
    pivot_longer(ends_with("Close"), names_to = "symbol", values_to = "closing_price") %>%
    mutate(symbol = x$symbol[1]) %>%
    mutate(date = as.Date(date))
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
