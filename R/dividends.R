get_stock_dividends <- function(x){
  suppressPackageStartupMessages(library(quantmod))
  suppressPackageStartupMessages(library(tidyverse))

  suppressMessages({
    symbol_data <- getDividends(
      Symbol = x$symbol[1],
      from = as.character(x$date_start[1] - lubridate::years(2)),
      to = as.character(x$date_end[1]),
      auto.assign = FALSE
    )
  })

  symbol_data %>%
    as_tibble(rownames = "date") %>%
    select(date, ends_with("div")) %>%
    pivot_longer(ends_with("div"), names_to = "symbol", values_to = "dividend") %>%
    mutate(symbol = str_remove(symbol, "\\..+$")) %>%
    mutate(date = as.Date(date))
}

calc_portfolio_dividends <- function(dividends_stock, unrated_dividends, trades){

  rated_dividends <- dividends_stock

  suppressPackageStartupMessages(library(tidyverse))

  rated <- trades %>%
    select(date, symbol, quantity) %>%
    filter(symbol %in% rated_dividends$symbol) %>%
    group_by(symbol) %>%
    mutate(quantity = cumsum(quantity)) %>%
    complete(date = full_seq(rated_dividends$date, 1)) %>%
    fill(quantity) %>%
    replace_na(list(quantity = 0)) %>%
    mutate(dividend_period = cut(
      x = date,
      breaks = rated_dividends$date[rated_dividends$symbol == first(.data$symbol)],
      labels =  na.omit(lead(rated_dividends$date[rated_dividends$symbol == first(.data$symbol)])),
      right = FALSE)) %>%
    filter(!is.na(dividend_period)) %>%
    mutate(date = as.Date(dividend_period)) %>%
    group_by(symbol, date) %>%
    summarise(quantity = mean(quantity), .groups = "drop") %>%
    left_join(rated_dividends, by = c("symbol", "date")) %>%
    mutate(dividend = dividend * quantity) %>%
    filter(dividend != 0) %>%
    select(date, symbol, dividend)

    unrated <- trades %>%
      select(date, symbol, quantity) %>%
      filter(symbol %in% unrated_dividends$symbol) %>%
      group_by(symbol) %>%
      mutate(quantity = cumsum(quantity)) %>%
      complete(date = full_seq(unrated_dividends$date, 1)) %>%
      fill(quantity) %>%
      replace_na(list(quantity = 0)) %>%
      left_join(unrated_dividends, by = c("symbol", "date")) %>%
      filter(!is.na(dividend)) %>%
      mutate(dividend = dividend * quantity) %>%
      filter(dividend != 0) %>%
      select(date, symbol, dividend)

    rated %>% bind_rows(unrated) %>% arrange(symbol, date)
}
