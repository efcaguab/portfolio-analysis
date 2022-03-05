calc_returns <- function(trades, stock_prices, securities, currency_conversions, dividends_portfolio, base_currency = "NZD"){

  suppressPackageStartupMessages({library(tidyverse)})

  date_list <- seq(min(trades$date), max(stock_prices$date), by = "day")
  # date_list <- seq(Sys.Date() - lubridate::years(1), max(stock_prices$date), by = "day")

  end_values <-  date_list %>%
    map_dfr(get_value, trades, securities, stock_prices) %>%
    group_by(currency) %>%
    fill(end_value, .direction = "down") %>%
    ungroup()

  # cash_flow <- date_list %>%
    # map_dfr(get_cash_flow, trades, securities)

  # returns_base <- calc_roi(end_values, cash_flow)

  cash_flow_dividends <- date_list %>%
    map_dfr(get_cash_flow, trades, securities, dividends_portfolio)

  # returns_dividends <- calc_roi(end_values, cash_flow_dividends)

  to_base_currency <- currency_conversions %>%
    filter(to == base_currency)

  end_values_basec <- end_values %>%
    left_join(to_base_currency, by = c("end_period" = "date", "currency" = "from")) %>%
    mutate(
      to = ifelse(currency == base_currency, currency, to),
      rate = ifelse(currency == base_currency, 1, rate)) %>%
    mutate(end_value = end_value * rate, currency = to) %>%
    select(-rate, -to) %>%
    filter(!is.na(currency)) %>%
    group_by(currency, end_period) %>%
    summarise(end_value = sum(end_value), .groups = "drop")

  cash_flow_basec <- cash_flow_dividends %>%
    left_join(to_base_currency, by = c("end_period" = "date")) %>%
    mutate(
      to = ifelse(currency == base_currency, currency, to),
      rate = ifelse(currency == base_currency, 1, rate)) %>%
    mutate(cash_flow = cash_flow * rate, currency = to) %>%
    select(-rate, -to, -from) %>%
    filter(!is.na(currency)) %>%
    group_by(currency, end_period) %>%
    summarise(cash_flow = sum(cash_flow), .groups = "drop")

  returns_basec <- calc_roi(end_values_basec, cash_flow_basec)

  returns_basec
}

get_value <- function(this_date, trades, securities, stock_prices){

  date_prices <- stock_prices %>%
    filter(date == this_date)

  currencies <- securities %>%
    group_by(symbol) %>%
    summarise(currency = first(currency))

  trades %>%
    filter(date <= this_date) %>%
    group_by(broker, symbol) %>%
    summarise(quantity = round(sum(quantity), 10), .groups = "drop") %>%
    filter(quantity != 0) %>%
    left_join(currencies, by = "symbol") %>%
    left_join(date_prices, by = "symbol") %>%
    mutate(end_value = closing_price * quantity) %>%
    group_by(currency) %>%
    summarise(end_value = sum(end_value)) %>%
    mutate(end_period = this_date)

}

get_cash_flow <- function(this_date, trades, securities, dividends_portfolio = NULL) {

  currencies <- securities %>%
    group_by(symbol) %>%
    summarise(currency = first(currency))

  if (is.null(dividends_portfolio)){
    from_dividends <- tibble(symbol = character(), value = numeric())
  } else {
    from_dividends <- dividends_portfolio %>%
      filter(date == this_date) %>%
      mutate(value = dividend * (-1)) %>%
      select(symbol, value)

  }


  trades %>%
    filter(date == this_date) %>%
    mutate(value = price * quantity + brokerage) %>%
    bind_rows(from_dividends) %>%
    group_by(symbol) %>%
    summarise(value = round(sum(value), 10), .groups = "drop") %>%
    left_join(currencies, by = "symbol") %>%
    group_by(currency) %>%
    summarise(cash_flow = sum(value)) %>%
    mutate(end_period = this_date)
}

calc_roi <- function(end_values, cash_flow){

  end_values %>%
    filter(!is.na(end_value)) %>%
    mutate(initial_value = lag(end_value)) %>%
    left_join(cash_flow) %>%
    mutate(
      initial_value = replace_na(initial_value, 0),
      cash_flow = replace_na(cash_flow, 0),
      hp = (end_value - (initial_value + cash_flow )) / (end_value + cash_flow),
      twr = cumprod(1 + hp) - 1,
      add_sub = cumsum(cash_flow),
      roi = (end_value  - add_sub) / add_sub )
}

