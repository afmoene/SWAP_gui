#' Check if string can be converted to numerical value
#'
#' @param text vector of character strings.
#' @return Logical
#' @export is_numeric
#' @examples
#' is_numeric(text = "abc")
#' is_numeric(text = "2.0")
#' is_numeric(text = "2.0f")
is_numeric <- function(text) {

  # ---- main part of procedure ----

  # set warn option (temporariry)
  opt_warn <- getOption("warn")
  options(warn=-1)

  # check if numeric
  check <- !is.na(as.numeric(text))

  # reset warn option (stored)
  options(warn = opt_warn)

  # ---- return of procedure ----

  return(check)
}

#' Check if string can be converted to date
#'
#' @param text vector of character strings.
#' @importFrom lubridate guess_formats as_date
#' @return Logical
#' @export is_date
#' @examples
#' is_date(text = "2019-08-13")
#' is_date(text = "2019-08-13 15:47:00")
#' is_date(text = "13-08-2019")
#' is_date(text = "13-Aug-2019")
is_date <- function(text) {

  # ---- initial part of procedure ----

  orders <- c("ymd" , "dmy")

  # ---- main part of procedure ----

  # check format
  formats <- guess_formats(x = text, orders = orders)
  check <- !is.null(formats)
  if (check) {
    format <- formats[names(formats) %in% orders]
    if (!all(format %in% c("%Y-%m-%d", "%d-%m-%Y", "%Y-%b-%d", "%d-%b-%Y"))) check <- FALSE
  }

  if (check) {

    # set warn option (temporariry)
    opt_warn <- getOption("warn")
    options(warn=-1)

    # check if numeric
    check <- !is.na(as_date(text))

    # reset warn option (stored)
    options(warn = opt_warn)

  } else {

    check <- rep(x = FALSE, times = length(text))

  }

  # ---- return of procedure ----

  return(check)
}

#' Check if R is running on a windows machine
#'
#' @return Logical
#' @export is_windows
#' @examples
#' is_windows()
is_windows <- function() {

  # ---- main part of procedure ----

  check <- Sys.info()[['sysname']] == "Windows"

  # ---- return of procedure ----

  return(check)
}
#' Get number of digits of value
#'
#' @param value value
#' @return Number of digits of \code{val}
#' @importFrom stringr str_split str_replace
#' @export get_digits
#' @examples
#' get_digits(value = 12)
#' get_digits(value = 12.)
#' get_digits(value = 12.123)
#' get_digits(value = c(0.0, 12, 1.23, 1))
get_digits <- function(value) {

  # ---- initial part of procedure ----

  value <- round(x = value, digits = 12)

  # ---- main part of procedure ----

  value_split <- str_split(string = str_replace(string = as.character(format(x = value, scientific = FALSE)), pattern = "0+$", replacement = ""), pattern = "\\.", simplify = TRUE)
  if (ncol(value_split) == 2) {
    digits <- nchar(value_split[, 2])
  } else {
    digits <- rep(x = 0, times = length(value))
  }

  # ---- return of procedure ----

  return(digits)
}
