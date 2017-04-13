#' Number if days in given year
#' 
#' Accounts for leap years.
#' 
#' @author Alexey Shiklomanov
#' @param year Calendar year
#' @export
days_in_year <- function(year) {
  ifelse(lubridate::leap_year(year), 366, 365)
}
