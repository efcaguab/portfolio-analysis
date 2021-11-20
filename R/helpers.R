format_fiscal_year <- function(x, sep = "\U2012") {
  current_year <- as.numeric(x)
  paste(current_year - 1, current_year, sep = sep)
}
