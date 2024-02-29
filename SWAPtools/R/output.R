#' Read SWAP-output
#'
#' @param file character string, name of SWAP output-file.
#' @param variable character string, name of SWAP variable to extract.
#' @param timestamp logical, TRUE if multiple timesteps within a day.
#' @param quiet logical.
#' @importFrom readr read_rds read_csv
#' @importFrom fs path_package path_dir path_ext path_file
#' @importFrom stringr str_c
#' @importFrom lubridate ymd_hms
#' @keywords internal
read_SWAP <- function(file, variable, timestamp = FALSE, quiet = TRUE) {

  # ---- initial part of procedure ----

  # load SWAP setup
  setup_SWAP <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_setup.rds"))

  # ---- main part of procedure ----

  # set extension
  ext <- path_ext(file)

  # set index of data
  index <- "Date"
  if (timestamp & ext %in% c("inc", "vap", "str")) index <- c("Date", "Time")
  if (ext == "vap") index <- c(index, "Top", "Bottom")

  # set setup of output-file
  s_setup_SWAP <- setup_SWAP[[ext]]
  if (!timestamp & ext %in% c("inc", "vap", "str")) {
    rec <- match(x = "Time", table = s_setup_SWAP$column)
    n_rec <- length(s_setup_SWAP$column)
    s_setup_SWAP$column <- s_setup_SWAP$column[c(1:(rec - 1), (rec + 1):n_rec)]
    s_setup_SWAP$format <- s_setup_SWAP$format[c(1:(rec - 1), (rec + 1):n_rec)]
    s_setup_SWAP$unit <- s_setup_SWAP$unit[c(1:(rec - 1), (rec + 1):n_rec)]
    s_setup_SWAP$type <- s_setup_SWAP$type[c(1:(rec - 1), (rec + 1):n_rec)]
  }

  # check variables to extract
  s_column <- s_setup_SWAP$column[s_setup_SWAP$type %in% c("rate", "state")]
  s_variable <- variable[variable %in% s_column]
  s_variable <- c(index, s_variable)
  clm <- match(x = s_variable, table = s_setup_SWAP$column)

  # set format of columns
  format <- rep(x = "-", times = length(s_setup_SWAP$column))
  format[clm] <- s_setup_SWAP$format[clm]
  format <- str_c(format, collapse = "")

  # set names of columns
  column <- s_setup_SWAP$column[sort(clm)]
  skip <- s_setup_SWAP$skip

  # extract requested variables from file
  if (!quiet) message(str_c("reading file: ", path_file(file)))
  dat <- read_csv(file = file, col_names = column, n_max = -1, skip = skip, comment = "", col_types = format, progress = FALSE, lazy = FALSE)

  # delete NA Bottom records
  if (ext == "vap") dat <- dat[!is.na(dat$Bottom),]

  # create class date
  if (!timestamp) {
    dat$datetime <- ymd_hms(str_c(dat$Date, "00:00:00", sep = " "))
  } else {
    dat$datetime <- ymd_hms(str_c(dat$Date, dat$Time, sep = " "))
  }

  # ---- return of procedure ----

  if (ext == "vap") {
    column <- c("datetime", "Top", "Bottom", variable)
  } else {
    column <- c("datetime", variable)
  }
  dat <- dat[, column]
  return(dat)
}

#' Scan SWAP-csv-output
#'
#' @param file character string, name of SWAP csv-output-file.
#' @param filter logical, filter variables only.
#' @importFrom readr read_csv
#' @importFrom stringr str_c str_to_upper
#' @export scan_csv_SWAP
#' @keywords internal
#' @examples
#' # extract example
#' dir_out <- paste0(tempfile(pattern = "swap_"))
#' file_csv <- paste0(dir_out, "/result_output.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, files = basename(file_csv), exdir = dir_out)
#'
#' # get list of variables from output file of SWAP
#' scan_csv_SWAP(file = file_csv)
#'
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
scan_csv_SWAP <- function(file, filter = TRUE) {

  # ---- main part of procedure ----

  # read header
  variables <- str_to_upper(as.character(read_csv(file = file, comment = "*", col_names = FALSE, n_max = 1, progress = FALSE, show_col_types = FALSE, lazy = FALSE)))
  if (filter) variables <- variables[!variables %in% c("DATETIME", "DATE", "DEPTH")]

  # ---- return of procedure ----

  return(variables)
}

#' Read SWAP-csv-output
#'
#' @param file character string, name of SWAP csv-output-file.
#' @param variable character string, name of SWAP variable to extract.
#' @param timestamp logical, TRUE if multiple timesteps within a day.
#' @param quiet logical.
#' @importFrom readr read_csv
#' @importFrom fs path_file
#' @importFrom stringr str_c
#' @importFrom lubridate ymd_hms
#' @importFrom dplyr %>% rename select all_of
#' @export read_csv_SWAP
#' @examples
#' # extract example
#' dir_out <- paste0(tempfile(pattern = "swap_"))
#' file_csv <- paste0(dir_out, "/result_output.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, files = basename(file_csv), exdir = dir_out)
#'
#' # plot timeseries of groundwater level
#' read_csv_SWAP(file = file_csv, variable = c("GWL", "WC[-15.0]"))
#'
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
read_csv_SWAP <- function(file, variable, timestamp = FALSE, quiet = TRUE) {

  # ---- initial part of procedure ----

  datetime <- DATE <- DEPTH <- NULL

  # ---- main part of procedure ----

  if (!quiet) message(str_c("reading file: ", path_file(file)))

  # scan available variables
  header <- scan_csv_SWAP(file = file, filter = FALSE)

  # set format of columns
  format <- rep(x = "-", times = length(header))
  clm <- match(x = c("DATE", "DATETIME"), table = header)
  format[clm] <- ifelse(timestamp, "T", "D")
  clm <- match(x = "DEPTH", table = header)
  if (!is.na(clm)) format[clm] <- "d"
  clm <- match(x = toupper(variable), table = header)
  format[clm] <- "d"
  format <- str_c(format, collapse = "")

  # extract requested variables from file
  db_swp <- read_csv(file = file, comment = "*", n_max = -1, col_types = format, progress = FALSE, lazy = FALSE)

  # change column DATE into DATETIME (before version SWAP 4.1.81)
  if ("DATE" %in% names(db_swp)) db_swp <- db_swp %>% rename(DATETIME = DATE)

  # set index
  index <- "datetime"
  if ("DEPTH" %in% names(db_swp)) {
    db_swp <- db_swp %>% rename(depth = DEPTH)
    index <- c("datetime", "depth")
  }

  # create class date
  if (!timestamp) {
    db_swp$datetime <- ymd_hms(str_c(db_swp$DATETIME, "00:00:00", sep = " "))
  } else {
    db_swp$datetime <- db_swp$DATETIME
  }


  # ---- return of procedure ----

  db_swp <- db_swp %>% select(all_of(index), all_of(variable))
  return(db_swp)
}

#' Get SWAP version
#'
#' @param file_swp character string, name of swp-file.
#' @importFrom stringr str_replace
#' @importFrom fs path_dir
#' @importFrom readr read_lines
#' @details Extracts SWAP version number from csv-output
#' @export get_version_SWAP
get_version_SWAP <- function(file_swp) {

  # ---- main part of procedure ----

  # load dynamic variables
  OUTFIL <- get_value_SWAP(file = file_swp, variable = "OUTFIL")
  file <- str_c(path_dir(file_swp), "/", OUTFIL, "_output.csv")
  string <- read_lines(file = file, skip = 3, n_max = 1, lazy = FALSE)
  version <- str_replace(string = string, pattern = "\\* Model version: Swap ", replacement = "")

  # ---- return of procedure ----

  return(version)
}
