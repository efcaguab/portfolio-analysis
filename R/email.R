generate_email <- function(trades_cost_basis, returns){

  suppressPackageStartupMessages({
    library(blastula)
  })

  tax_and_compliance_section <- get_tax_and_compliance(trades_cost_basis)
  # performance_section <- get_performance(returns)
  compose_email(
    title = "Your weekly investment update",
    # header = "Your weekly investment update",
    body = blocks(
      block_text(md(get_intro(trades_cost_basis, returns))),
      block_spacer(),
      block_text(md(paste(tax_and_compliance_section))),
      # block_text(md(performance_section))
    ),
    footer = md(get_footer())
  )

}

send_email <- function(email, to = config::get("email_to"), from = Sys.getenv("SMTP_USER"),  subject, host = Sys.getenv("SMTP_HOST")){

  suppressPackageStartupMessages({
    library(blastula)
  })

  smtp_send(
    email,
    to = to,
    from = from,
    subject = subject,
    credentials = creds_envvar(
      user = from,
      host = host,
      port = 465,
      use_ssl = TRUE
    )
  )
}

get_intro <- function(trades_cost_basis, returns){

  peak_values <- trades_cost_basis %>%
    group_by(owner, date) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(quarter = lubridate::quarter(date, with_year = TRUE, fiscal_start = 4),
           fiscal_year = str_sub(quarter, 1, 4)) %>%
    group_by(owner, fiscal_year) %>%
    summarise(date_of_peak = last(date[value == max(value)]),
              peak_cost = max(value), .groups = "drop") %>%
    group_by(owner) %>%
    filter(as.numeric(fiscal_year) == max(as.numeric(fiscal_year))) %>%
    ungroup()

  mf <- scales::number_format(accuracy = 10, prefix = "NZD ", big.mark = ",")
  pf <- scales::percent_format(accuracy = 0.1)

  peak_cost <- glue_collapse(scales::number(peak_values$peak_cost, accuracy = 1, big.mark = ","), sep = ", ", last = " and ")
  peak_owners <- glue_collapse(peak_values$owner, sep = ", ", last = " and ")

  current_size <- returns$end_value[returns$end_period == max(returns$end_period)]
  current_return <- returns$roi[returns$end_period == max(returns$end_period)]

  returns_plot <- blastula::add_ggplot(
    plot_object = visualise_returns(returns),
    height = 2.5)

  last_period <- slice(returns, which.min(abs(returns$end_period - (max(returns$end_period) - lubridate::days(7)))))
  size_diff <- current_size - last_period$end_value

  glue(
    "
    Hi, here is a summary of Fernando and Peter investments.

    The current size of the portfolio is **{mf(current_size)}** compared to {mf(last_period$end_value)} one week ago ({mf(size_diff)}).

    This makes for an overall return of investment of **{pf(current_return)}** since the inception of the portfolio.

    {returns_plot}

    The peak cost basis for foreign investment funds for the current finantial year are NZD {peak_cost} for {peak_owners} portfolios.

    Have a look below for more details.

    Cheers,

    Fer

    "
  )
}

get_outro <- function(){
  glue(
    "
    Regards,

    Fer
    "
  )
}

get_footer <- function(){
  glue(
    "
    This report was automatically generated on {add_readable_time()}
    "
  )
}

get_tax_and_compliance <- function(trades_cost_basis){
  suppressPackageStartupMessages({
    library(tidyverse)
    library(gt)
    library(glue)
  })

  cost_basis_table <- generate_cost_basis_table(trades_cost_basis)
  cost_basis_table_html <- as_raw_html(cost_basis_table)

  # cost_basis_plot <- generate_cost_basis_plot(trades_cost_basis)
  # cost_basis_plot_html <- add_ggplot(cost_basis_plot, width = 8, height = 5)

  glue(
    "
    ## Tax and compliance

    ### Cost basis of foreign investment funds

    The cost basis (purchase price) of investments foreign (to New Zealand) is used to determine whether an individual is except or not of foreign investment funds (FIF) tax.

    FIF tax is payable if the cost of foreign investment funds surpasses an NZD 50,000 value at any time during the financial year.

    Costs are calculated using the first-in-first-out method.

    <br />

    {cost_basis_table_html}

    "
  )

}

generate_cost_basis_table <- function(trades_cost_basis){

  trades_cost_basis %>%
    group_by(owner, date) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(quarter = lubridate::quarter(date, with_year = TRUE, fiscal_start = 4),
           fiscal_year = str_sub(quarter, 1, 4)) %>%
    group_by(owner, fiscal_year) %>%
    summarise(date_of_peak = last(date[value == max(value)]),
              peak_cost = max(value), .groups = "drop") %>%
    arrange(owner, desc(fiscal_year)) %>%
    # group_by(owner) %>%
    # pivot_wider(fiscal_year, )
    `names<-`(str_to_sentence(str_replace_all(names(.), pattern = "_", replacement = " "))) %>%
    gt() %>%
    tab_header(
      title = "Cost basis of Foreign Investment Funds",
      subtitle = "Balance across finantial years") %>%
    fmt_currency(
      columns = ends_with("cost"),
      currency = "NZD",
      decimals = 0) %>%
    fmt(columns = `Fiscal year`, fns = format_fiscal_year) %>%
    fmt_date(columns = `Date of peak`, date_style = 5) %>%
    cols_align(
      align = "right",
      columns = ends_with("cost")) %>%
    tab_source_note("* All values in NZD") %>%
    # tab_footnote(
    #   footnote = md(
    #     glue::glue('Values in NZD')),
    #   locations = cells_column_labels(columns = ends_with("cost"))) %>%
    tab_options(table.font.size = "14px",
                heading.title.font.weight = "bold",
                heading.title.font.size = "15px")
}


generate_cost_basis_plot <- function(trades_cost_basis){

  cost_basis_plot_data <- trades_cost_basis %>%
    mutate(symbol = fct_lump_n(symbol, n = 4, w = value)) %>%
    group_by(owner, date, symbol) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    complete(owner, nesting(date, symbol), fill = list(value = 0)) %>%
    group_by(owner, date) %>%
    filter(!all(value == 0)) %>%
    ungroup() %>%
    mutate(symbol = fct_reorder(symbol, value, mean, .desc = T)) %>%
    mutate(symbol = fct_relevel(symbol, "Other", after = Inf))

  cost_basis_plot_data %>%
    ggplot(aes(x = date, y = value, fill = symbol)) +
    geom_area(alpha = 0.9, colour = "white", size = 0.25) +
    scale_y_continuous(labels = scales::dollar_format()) +
    scale_x_date(date_minor_breaks = "1 month", expand = c(0, 0)) +
    scale_fill_manual(values = viridis::viridis(5)) +
    theme_minimal() +
    facet_wrap("owner", ncol = 1) +
    labs(
      title = "Cost basis of Foreign Investment Funds",
      subtitle = "Contribution of multiple secuirities",
      x = "Date",
      y = "Value (NZD)") +
    theme(legend.title = element_blank())
}

## Performance section

get_performance <- function(returns){

  suppressPackageStartupMessages({
    library(tidyverse)
    library(gt)
    library(glue)
  })

  returns_plot <- blastula::add_ggplot(
    plot_object = visualise_returns(returns),
    height = 2.5)

  glue(
    "
    ## Performance

    The overal return since the inception of the portfolio are

    {returns_plot}
    "

  )

}


visualise_returns <- function(returns){

  latest_date <- max(returns$end_period)
  latest_return <- returns$roi[returns$end_period == latest_date]

  returns %>%
    # filter(end_period >= Sys.Date() - lubridate::days(30)) %>%
    ggplot(aes(y = roi, x = end_period)) +
    # geom_line(aes(y = twr)) +
    geom_line(aes(colour = as.character(last(roi) > first(roi)), group = 1), size = 0.35) +
    geom_area(aes(fill = as.character(last(roi) > first(roi))), alpha = 0.25) +
    geom_hline(yintercept = 0, linetype = 2, size = 0.25) +
    geom_vline(xintercept = as.Date("2021-04-01"), linetype = 2, size = 0.25) +
    geom_vline(xintercept = as.Date("2022-04-01"), linetype = 2, size = 0.25) +
    scale_colour_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"), aesthetics = c("colour", "fill")) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
    scale_x_date(date_breaks = "4 months", date_minor_breaks = "1 month", date_labels = "%b %Y", expand = c(0,0)) +
    # scale_x_date(expand = c(0,0)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_blank()) +
    labs(
      # title = paste(
      #   "Overall Return of Investment:",
      #   scales::percent(latest_return, accuracy = 0.1)),
      # subtitle = paste(
      #   "From inception until",
      #   latest_date),
      caption = "Includes capital, currency, and dividend gains.")


}
