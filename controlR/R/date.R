#' Returns daynumber of year
#'
#' @param date a character or numeric vector of suspected dates.
#' @return daynumber of year of \code{date}
#' @importFrom lubridate ymd floor_date
#' @export dayyear
#' @examples
#' date <- c("2016-01-01", "2016-12-31", "2019-01-01", "2019-12-31")
#' dayyear(date)
dayyear <- function(date) {

  # ---- initial part of procedure ----

  if (!all(class(date) %in% c("Date", "POSIXct", "POSIXt"))) date <- ymd(date)

  # main part of procedure
  #-------------------------------------------

  date_dayyear <- as.numeric(difftime(time1 = date, time2 = floor_date(date, unit = "year"), units = "days")) + 1

  # ---- return of procedure ----

  return(date_dayyear)
}

#' Returns hydrologic year
#'
#' @param date a character or numeric vector of suspected dates.
#' @return hydrologic year of \code{date}
#' @importFrom lubridate ymd year month
#' @export hydryear
#' @examples
#' date <- c("2016-01-01", "2016-12-31", "2019-01-01", "2019-12-31")
#' hydryear(date)
hydryear <- function(date) {


  # ---- initial part of procedure ----

  if (!all(class(date) %in% c("Date", "POSIXct", "POSIXt"))) date <- ymd(date)

  # ---- main part of procedure ----

  date_year <- year(date)
  date_month <- month(date)
  date_hydryear <- ifelse(date_month <= 3, paste(date_year - 1, date_year, sep = "-"), paste(date_year, date_year + 1, sep = "-"))

  # ---- return of procedure ----

  return(date_hydryear)
}
