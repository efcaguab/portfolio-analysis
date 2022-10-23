generate_email <- function(trades_cost_basis, returns, allocation_current){

  suppressPackageStartupMessages({
    library(blastula)
    library(patchwork)
  })

  tax_and_compliance_section <- get_tax_and_compliance(trades_cost_basis)
  performance_section <- get_performance(returns)
  contributions_section <- get_contributions(returns)
  balancing_section <- get_balancing(allocation_current)

  compose_email(
    title = "Your weekly investment update",
    # header = "Your weekly investment update",
    body = blocks(
      block_text(md(get_intro(trades_cost_basis, returns, allocation_current))),
      block_spacer(),
      block_text(md(performance_section)),
      block_text(md(contributions_section)),
      block_text(md(balancing_section)),
      block_text(md(paste(tax_and_compliance_section)))

    ),
    footer = md(get_footer())
  )

}

send_email <- function(email, to = config::get("email_to"), from = Sys.getenv("SMTP_USER"),  subject, host = Sys.getenv("SMTP_HOST"), port = Sys.getenv("SMTP_PORT", "465")){

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
      port = port,
      use_ssl = TRUE
    )
  )
}

get_intro <- function(trades_cost_basis, returns, allocation_current){

  mf <- scales::number_format(accuracy = 1, prefix = "NZD ", big.mark = ",")
  pf <- scales::percent_format(accuracy = 0.1)

  current_size <- returns$end_value[returns$end_period == max(returns$end_period)]
  current_return <- returns$roi[returns$end_period == max(returns$end_period)]

  needs_rebalance <- max(abs(map_dfr(allocation_current, ~ .)$allocation_diff)) > 0.01

  # last_period <- slice(returns, which.min(abs(returns$end_period - (max(returns$end_period) - lubridate::days(7)))))
  # size_diff <- current_size - last_period$end_value

  glue(
    "
    Hi,

    As of today ({format(lubridate::today(), '%d %B %Y')}) the size of Fer and Peter's portfolio is **{mf(current_size)}**. Excluding taxes, this makes for an overall return of investment of **{pf(current_return)}** since inception.
    {ifelse(needs_rebalance, 'The portfolio might need to be rebalanced to match the target allocation.', '') }

    More details below.

    Cheers,

    Fer

    "
  )
}

visualise_allocation <- function(allocation_current){

  x <- allocation_current %>%
    imap_dfr(function(x,y){
      x <- x %>%
        arrange(desc(allocation_target))
      x |>
        mutate(label = paste(x[[y]], collapse = " ")) %>%
        slice(1) %>%
        select(label, starts_with("allocation"))
    }, .id = "allocation_type") %>%
    mutate(label = factor(label))

  max_diff <- max(abs(x$allocation_diff))

  pl <- x %>%
    split(.$allocation_type) %>%
    map(function(x){
      x %>%
        mutate(label = factor(label)) %>%
        ggplot(aes(x = allocation_actual, y = as.numeric(label))) +
        geom_segment(aes(yend = as.numeric(label)), x = -Inf, xend = Inf, colour =  "#d7191c", size = 0.5) +
        geom_segment(aes(yend = as.numeric(label), x = allocation_target-0.025, xend = allocation_target + 0.025), colour =  "#fdae61", size = 0.5) +
        geom_segment(aes(yend = as.numeric(label), x = allocation_target-0.01, xend = allocation_target+0.01), colour =  "#1a9641", size = 0.5) +
        geom_vline(aes(xintercept = allocation_target), size = 0.5, linetype = 2) +
        # geom_vline(aes(xintercept = allocation_target + max(allocation_diff)), size = 0.5, linetype = 2, alpha = 0) +
        # geom_vline(aes(xintercept = allocation_target - max(allocation_diff)), size = 0.5, linetype = 2, alpha = 0) +
        geom_point(size = 2, shape = 21, fill = "white") +
        scale_y_continuous(breaks = 1, labels = word(x$label), sec.axis = sec_axis(trans = ~. , breaks = 1, label = word(x$label, 2))) +
        scale_x_continuous(labels = scales::percent) +
        coord_cartesian(xlim = c(x$allocation_target - max_diff, x$allocation_target+ max_diff)) +
        # scale_fill_stepsn(limits = c(-1, 1), values = scales::rescale(c(-1, 0.025, -0.01, -0.01, 0.025, 1)), colors = c("#d7191c", "#d7191c","#fdae61", "#1a9641","#1a9641", "#fdae61", "#d7191c")) +
        # facet_wrap("allocation_type", scales = "free", ncol = 1) +
        theme_minimal() +
        theme(axis.title = element_blank(), strip.text = element_blank(), axis.text.y = element_text(size = 10, colour = "black"))
    })

  pl[[1]] / pl[[2]] / pl[[3]]


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

  peak_cost <- glue_collapse(scales::number(peak_values$peak_cost, accuracy = 1, big.mark = ","), sep = ", ", last = " and ")

  peak_owners <- glue_collapse(peak_values$owner, sep = ", ", last = " and ")

  # cost_basis_plot <- generate_cost_basis_plot(trades_cost_basis)
  # cost_basis_plot_html <- add_ggplot(cost_basis_plot, width = 8, height = 5)

  glue(
    "
    ## Tax and compliance

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
    height = 3)

  glue(
    "
    ## Performance

    {returns_plot}


    <br />

    {as_raw_html(summary_returns_table(returns))}

    "

  )

}


visualise_returns <- function(returns){

  latest_date <- max(returns$end_period)
  latest_return <- returns$roi[returns$end_period == latest_date]

  returns %>%
    # group_by(cut(end_period, "week")) %>%
    # slice_tail(n = 1) %>%
    # filter(end_period >= Sys.Date() - lubridate::days(30)) %>%
    ggplot(aes(y = roi, x = end_period)) +
    # geom_line(aes(y = twr)) +
    geom_line(aes(colour = as.character(last(roi) > first(roi)), group = 1), size = 0.25) +
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
      axis.title = element_text(size = 10),
      axis.title.x = element_blank(),
      plot.margin = unit(c(1,17,1,1), "pt")) +
    labs(
      # title = paste(
      #   "Overall Return of Investment:",
      #   scales::percent(latest_return, accuracy = 0.1)),
      # subtitle = paste(
      #   "From inception until",
      #   latest_date),
      # title = "Performance",
      y = "Return",
      caption = "Includes capital, currency (NZD), and dividend gains.\nExcludes taxes and fees.")


}


summary_returns_table <- function(returns){
  library(gt)

  get_summary_returns(returns) %>%
    # select(-current_balance) %>%
    mutate(
      growth = contributions + dividends + gains,
      # previous_balance = if_else(previous_balance == 0, NA_real_, previous_balance),
      time_diff = case_when(
        time_diff == 7 ~ "Last week",
        time_diff == 30 ~ "Last month",
        time_diff == 365 ~ "Last year",
        TRUE ~ "Since inception"
      )) %>%
    rename(
      "Range" = time_diff,
      "Total growth" = growth,
      "Contributions" = contributions,
      "Dividends" = dividends,
      "Capital" = gains
    ) %>%
    select(-ends_with("balance")) %>%
    relocate(`Total growth`, .after = last_col()) %>%
    gt(rowname_col = "Range") %>%
    tab_spanner(
      label = "Gains",
      columns = c(`Dividends`, `Capital`)) %>%
    # tab_header(
      # title = "Summary performance") %>%
    fmt_currency(
      columns = c("Contributions", "Total growth", "Capital", "Dividends"),
      accounting = FALSE,
      decimals = 0,
      currency = "NZD") %>%
    fmt(
      columns = c("Contributions", "Capital", "Dividends"),
      fns = format_gains) %>%
    cols_align(
      align = "right",
      columns = -starts_with("Range")) %>%
    tab_source_note("* All values in NZD") %>%
    tab_options(table.font.size = "14px",
                heading.title.font.weight = "bold",
                heading.title.font.size = "15px")
}

format_gains <- function(x){
  x_sign <- sign(x)
  d <- scales::dollar(round(x))
  case_when(
    x_sign == 0 ~ d,
    x_sign == 1 ~ as.character(glue("<span style='color:#006837'>{d}</span>")),
    x_sign == -1 ~ as.character(glue("<span style='color:#a50026'>{d}</span>"))
  )
}

get_contributions <- function(returns){
  suppressPackageStartupMessages({
    library(tidyverse)
    library(gt)
    library(glue)
  })

  contributions_plot <- blastula::add_ggplot(
    plot_object = visualise_contributions(returns),
    height = 2
  )

  glue(
    "
    ## Contributions

    {contributions_plot}

    "
  )
}

get_balancing <- function(allocation_current){
  suppressPackageStartupMessages({
    library(tidyverse)
    library(gt)
    library(glue)
  })

  allocation_plot <- blastula::add_ggplot(
    plot_object = visualise_allocation(allocation_current),
    height = 2
  )

  glue(
    "
    ## Asset allocation

    {allocation_plot}

    "
  )
}

visualise_contributions <- function(returns){

  returns |>
    mutate(semester = lubridate::semester(end_period,with_year = F),
           year = lubridate::year(end_period),
           semester = paste(year, semester)) |>
    group_by(year) |>
    summarise(contributions = sum(cash_flow),
              complete = all(range(lubridate::month(end_period)) == c(1,12))) |>
    ggplot(aes(x = year, y = contributions)) +
    geom_col(aes(alpha = complete, fill = contributions >= 0)) +
    scale_fill_manual(values = c("#1a9641", "#d7191c")) +
    scale_alpha_manual(values = c(0.75, 1)) +
    scale_y_continuous(labels = scales::dollar) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 10),
      axis.title.x = element_blank(),
      plot.margin = unit(c(1,17,1,1), "pt")) +
    labs(
      # title = paste(
      #   "Overall Return of Investment:",
      #   scales::percent(latest_return, accuracy = 0.1)),
      # subtitle = paste(
      #   "From inception until",
      #   latest_date),
      y = "Contributions (NZD)\n")
      # caption = "Includes capital, currency (NZD), and dividend gains.\nExcludes taxes and fees.")

}
