get_current_portfolio <- function(trades, securities, stock_prices){
  suppressPackageStartupMessages(library(tidyverse))

  latest_prices <- stock_prices %>%
    filter(date == max(date))

  trades %>%
    group_by(owner, broker, symbol) %>%
    summarise(quantity = round(sum(quantity), 10), .groups = "drop") %>%
    filter(quantity != 0) %>%
    left_join(securities, by = "symbol") %>%
    mutate(quantity = quantity * allocation_proportion) %>%
    left_join(latest_prices, by = "symbol") %>%
    mutate(closing_value = closing_price * quantity)
}
