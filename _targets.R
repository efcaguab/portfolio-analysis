# Prepare workspace -------------------------------------------------------

library(targets)
library(tarchetypes)

# load functions
f <- lapply(list.files(path = here::here("R"), full.names = TRUE,
                       include.dirs = TRUE, pattern = "*.R"), source)

# Plan analysis ------------------------------------------------------------

data_input_plan <- list(
  tar_target(
    name = sheet_last_modified,
    command = get_date_last_modified(),
    cue = tar_cue(mode = "always")
  ),
  tar_target(
    name = trades,
    command = {sheet_last_modified; get_gs_sheet(range = "trades", col_types = "ccccDddcd")}),
  tar_target(
    name = securities,
    command = {sheet_last_modified; get_gs_sheet(range = "securities", col_types = "cdccccc")}),
  tar_target(
    name = today,
    command = Sys.Date(),
    cue = tar_cue(mode = "always")),
  tar_group_size(
    name = stock_dates,
    command = calculate_stock_daterange(trades, today),
    size = 1),
  tar_target(
    name = stock_prices,
    command = get_closing_price(stock_dates),
    pattern = map(stock_dates),
    iteration = "group"),
  tar_target(
    name = dividends_stock,
    command = get_stock_dividends(stock_dates),
    pattern = map(stock_dates),
    iteration = "group"),
  tar_group_size(
    name = currency_dates,
    command = calculate_currency_daterange(trades, securities, today),
    size = 1),
  tar_target(
    name = currency_prices,
    command = get_closing_price(currency_dates),
    pattern = map(currency_dates),
    iteration = "group"),

  ## ANALYSIS
  tar_target(
    name = currency_conversions,
    command = calculate_currency_conversions(currency_prices)),
  tar_target(
    name = trades_cost_basis,
    command = get_trades_cost_basis(trades, securities, currency_conversions, dates = unique(c(trades$date, today)))),
  tar_target(
    name = current_portfolio,
    command = get_current_portfolio(trades, securities, stock_prices)),

  tar_target(
    name = allocation_target,
    command = process_target_allocation(config::get("target_allocation"))),
  tar_target(
    name = allocation_current,
    command = calc_asset_allocation(current_portfolio, allocation_target, currency_conversions)),

  tar_target(
    name = dividends_portfolio,
    command = calc_portfolio_dividends(dividends_stock, trades)
  ),
  tar_target(
    name = returns,
    command = calc_returns(trades, stock_prices, securities, currency_conversions, dividends_portfolio, base_currency = "NZD")
  )

)


reporting_plan <- list(
  tar_target(
    name = email,
    command = generate_email(trades_cost_basis, returns)
  ),
  tar_target(
    name = email_sent,
    command = send_email(
      email,
      subject = glue::glue("Weekly investment update ({lubridate::year(lubridate::today())} - {lubridate::week(lubridate::today())})")),
    cue = tar_cue_skip(condition = config::get("skip_email"))
  )
)

list(
  data_input_plan,
  reporting_plan
)
