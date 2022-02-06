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

process_target_allocation <- function(allocation_target){

  list(
    type = tribble(
      ~ type, ~ allocation_target,
      "bond", allocation_target$bond_prop,
      "stock", 1 - allocation_target$bond_prop
    ),
    allocation = tribble(
      ~ allocation, ~ allocation_target,
      "core", allocation_target$core_prop,
      "satellite", 1 - allocation_target$core_prop
    ),
    geography = tribble(
      ~ geography, ~ allocation_target,
      "nz", allocation_target$nz_prop,
      "us", (1 - allocation_target$nz_prop) * allocation_target$us_prop,
      "ex-us", (1 - allocation_target$nz_prop) * (1 - allocation_target$us_prop)
    )
  )
}

calc_asset_allocation <- function(current_portfolio, allocation_target, currency_conversions, base_currency = "NZD"){
  suppressPackageStartupMessages(library(tidyverse))

  to_nzd_conversions <- currency_conversions %>%
    filter(to == base_currency)

  current_portfolio <- current_portfolio %>%
    arrange(date) %>%
    left_join(to_nzd_conversions, by = c("date" = "date", "currency" = "from")) %>%
    mutate(across(starts_with("closing"), ~ . * rate)) %>%
    mutate(currency = base_currency)

  allocation_target %>%
    imap(get_allocation, portfolio = current_portfolio)
}


get_allocation <- function(allocation_target, allocation_type, portfolio){

  if (n_distinct(portfolio$currency) != 1)
    stop("All holdings must be in the same currency")

  suppressMessages({
    portfolio %>%
      group_by(.data[[allocation_type]], currency) %>%
      summarise(closing_value = sum(closing_value)) %>%
      ungroup() %>%
      mutate(allocation_actual = closing_value / sum(closing_value)) %>%
      full_join(allocation_target) %>%
      mutate(
        closing_value = replace_na(closing_value, 0),
        allocation_actual = replace_na(allocation_actual, 0),
        currency = replace_na(currency, first(na.omit(currency)))) %>%
      mutate(
        allocation_diff = allocation_target - allocation_actual
      )
  })
}
