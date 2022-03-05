
get_trades_cost_basis <- function(trades, securities, currency_conversions, dates, base_currency = "NZD"){

  suppressPackageStartupMessages(require(tidyverse, quietly = TRUE))

  symbol_currencies <- securities %>%
    group_by(symbol) %>%
    summarise(currency = first(currency))

  to_nzd_conversions <- currency_conversions %>%
    filter(to == base_currency)

  trades %>%
    filter(brokerage_currency != base_currency) %>%
    arrange(date) %>%
    left_join(symbol_currencies, by = "symbol") %>%
    left_join(to_nzd_conversions, by = c("date" = "date", "currency" = "from")) %>%
    mutate(price = price * rate) %>%
    group_by(owner, symbol) %>%
    summarise(value = list(tibble(dates, value = map_dbl(dates, ~ get_stock_purchase_value(date, type, quantity, price, .)))), .groups = "drop") %>%
    unnest(value) %>%
    rename(date = dates) %>%
    mutate(currency = "NZD")
}


get_stock_purchase_value <- function(trade_date, trade_type, quantity, price, cutoff_date = Sys.Date()){


  stock_trades <- tibble(date = trade_date, type = trade_type, quantity, price) %>%
    ungroup() %>%
    filter(date <= cutoff_date) %>%
    arrange(date)

  if (nrow(stock_trades) == 0) return(0)

  balances <- list()
  for (i in 1:nrow(stock_trades)) {
    if (stock_trades$type[i] == "buy") {
      balances[[i]] <- stock_trades %>%
        filter(date <= stock_trades$date[i]) %>%
        mutate(eval_date = stock_trades$date[i])
    }
    if (stock_trades$type[i] == "sell") {
      balances[[i]] <- balances[[i - 1]] %>%
        mutate(q2 = cumsum(quantity) + stock_trades$quantity[i]) %>%
        mutate(quantity = case_when(q2 <= 0 ~ 0,
                                    q2 > 0 & lag(q2 <= 0) ~ q2,
                                    TRUE ~ quantity)) %>%
        # filter(quantity > 0) %>%
        mutate(eval_date = stock_trades$date[i]) %>%
        select(-q2)
    }
  }

  last_balance <- balances[[length(balances)]] %>%
    mutate(value = price * quantity)

  sum(last_balance$value)
}


