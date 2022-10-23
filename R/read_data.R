
get_gs_sheet <- function(range, spreadsheet = config::get()$google_sheets_id, auth_path = config::get()$sa_key_path, ...){
  googlesheets4::gs4_auth(path = auth_path)
  suppressMessages(googlesheets4::read_sheet(ss = spreadsheet, range = range, ...))
}

get_date_last_modified <- function(spreadsheet = config::get("google_sheets_id"), auth_path = config::get("sa_key_path")){
  suppressPackageStartupMessages(library(tidyverse))
  logger::log_info("Loding google service key credentials from ", auth_path)
  googledrive::drive_auth(path = auth_path)
  logger::log_info("Finding last modified date for ", spreadsheet)
  x <- googledrive::drive_find() %>%
    filter(id == spreadsheet) %>%
    mutate(modified = map_chr(drive_resource, "modifiedTime"))
  x$modified[[1]]
}

