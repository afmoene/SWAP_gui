#' Get possible variables of meteo stations from \href{https://www.knmi.nl/home}{KNMI}
#'
#' @param topt frequency of observations, options: 'day' or 'hour'
#' @importFrom stringr str_c
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @export opt_variable_meteo_KNMI
#' @examples
#' # select all variables of meteo stations from KNMI
#' opt_variable_meteo_KNMI(topt = "day")
opt_variable_meteo_KNMI <- function(topt) {

  # ---- initial part of procedure ----

  # load settings KNMI
  if (topt == "day") {
    opt_variable <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_variable_meteo_day.rds"))
  } else {
    opt_variable <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_variable_meteo_hour.rds"))
  }

  # ---- return part of procedure ----

  return(names(opt_variable))
}

#' Get possible variables of precipitation stations from \href{https://www.knmi.nl/home}{KNMI}
#'
#' @importFrom stringr str_c
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @export opt_variable_precipitation_KNMI
#' @examples
#' # select all variables of meteo stations from KNMI
#' opt_variable_precipitation_KNMI()
opt_variable_precipitation_KNMI <- function() {

  # ---- initial part of procedure ----

  # load settings KNMI
  opt_variable <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_variable_precipitation.rds"))

  # ---- return part of procedure ----

  return(names(opt_variable))
}

#' Get possible meteo stations from \href{https://www.knmi.nl/home}{KNMI}
#'
#' @importFrom stringr str_c
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @export opt_station_meteo_KNMI
#' @examples
#' # select all meteo stations from KNMI
#' opt_station_meteo_KNMI()
opt_station_meteo_KNMI <- function() {

  # ---- initial part of procedure ----

  # load settings KNMI
  opt_station <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_station_meteo.rds"))

  # ---- return part of procedure ----

  return(names(opt_station))
}

#' Get info of meteo stations from \href{https://www.knmi.nl/home}{KNMI}
#'
#' @importFrom stringr str_c
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @export get_station_meteo_KNMI
#' @examples
#' # select all meteo stations from KNMI
#' get_station_meteo_KNMI()
get_station_meteo_KNMI <- function() {

  # ---- initial part of procedure ----

  # load settings KNMI
  db <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_station_info_meteo.rds"))

  # ---- return part of procedure ----

  return(db)
}

#' Get possible precipitation stations from \href{https://www.knmi.nl/home}{KNMI}
#'
#' @importFrom stringr str_c
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @export opt_station_precipitation_KNMI
#' @examples
#' # select all precipitation stations from KNMI
#' opt_station_precipitation_KNMI()
opt_station_precipitation_KNMI <- function() {

  # ---- initial part of procedure ----

  # load settings KNMI
  opt_station <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_station_precipitation.rds"))

  # ---- return part of procedure ----

  return(names(opt_station))
}

#' Get info of precipitation stations from \href{https://www.knmi.nl/home}{KNMI}
#'
#' @importFrom stringr str_c
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @export get_station_precipitation_KNMI
#' @examples
#' # select all precipitation stations from KNMI
#' get_station_precipitation_KNMI()
get_station_precipitation_KNMI <- function() {

  # ---- initial part of procedure ----

  # load settings KNMI
  db <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_station_info_precipitation.rds"))

  # ---- return part of procedure ----

  return(db)
}

#' Download observations from \href{https://www.knmi.nl/home}{KNMI} meteo station
#'
#' @param dir_out character string, name of directory
#' @param station character string, name(s) of meteo stations
#' @param variable character string, name(s) of observed variables
#' @param tstart date, start of period
#' @param tend date, end of period
#' @param ... further arguments passed to or from other methods
#' @importFrom utils unzip
#' @importFrom stringr str_c str_sub str_replace_all str_replace str_to_upper
#' @importFrom fs path_package dir_create dir_delete file_temp
#' @importFrom readr read_rds read_csv write_csv
#' @importFrom rlang :=
#' @importFrom dplyr %>% left_join mutate rename select filter if_else all_of
#' @importFrom tibble as_tibble tibble deframe add_column
#' @importFrom curl new_handle handle_setform
#' @importFrom lubridate as_date
#' @importFrom controlR download_data
#' @export download_meteo_KNMI
#' @examples
#' # download meteo from KNMI (do not run!)
#' #download_meteo_KNMI(dir_out = "../temp_knmi",
#' # station = "de Bilt", variable = "RH", tstart = "1980-01-01", tend = "1980-12-31")
download_meteo_KNMI <- function(dir_out, station, variable, tstart, tend, ...) {

  # ---- initial part of procedure ----

  STN <- YYYYMMDD <- HH <- value <- time <- NULL

  # set optional arguments
  opt_param <- c("topt", "replace", "url", "opt_station", "opt_variable")
  for (name in opt_param) assign(name, NULL)

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(topt)) topt <- "day"
  if (is.null(opt_station)) opt_station <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_station_meteo.rds"))
  if (is.null(opt_variable)) opt_variable <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_variable_meteo_", topt,".rds"))
  if (is.null(url)) {
    if (topt == "day") {
      url <- "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/daggegevens/etmgeg_{{stn_id}}.zip"
    } else {
      url <- "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/uurgegevens/uurgeg_{{stn_id}}_{{start_id}}-{{end_id}}.zip"
    }
  }

  # ---- main part of procedure ----

  # set stns
  stn_id <- replace_id <- NULL
  for (s_station in station) {
    rec <- match(x = str_to_upper(s_station), table = names(opt_station))
    if (is.na(rec)) stop(str_c("unknown station selected: '", s_station, "'\n check 'opt_station_meteo_KNMI'"))
    stn_id <- c(stn_id, opt_station[[rec]]$STN)
  }
  if (!is.null(replace)) {
    rec <- match(x = str_to_upper(replace), table = names(opt_station))
    if (is.na(rec)) stop(str_c("unknown station selected: '", replace, "'\n check 'opt_station_meteo_KNMI'"))
    replace_id <- opt_station[[rec]]$STN
  }

  # set period
  if (topt == "day") {
    start_id <- str_replace_all(string = tstart, pattern = "-", replacement = "")
    end_id <- str_replace_all(string = tend, pattern = "-", replacement = "")
  } else {
    start_id <- floor((year(tstart) - 1) / 10) * 10 + 1
    end_id <- ceiling(year(tend) / 10) * 10
    first_id <- start_id + 10
    last_id <- end_id
    while (first_id < last_id) {
      start_id <- c(start_id, first_id)
      end_id <- sort(c(first_id - 1, end_id))
      first_id <- first_id + 10
    }
  }

  # create empty data frame
  if (topt == "day") {
    db_empty <- tibble(date = seq(from = as_date(tstart), to = as_date(tend), by = 1), time = "00:00:00")
  } else {
    db_empty <- as_tibble(expand.grid(time = str_c(formatC(x = 0:23, format = "d", width = 2, flag = "0"),":00:00"), date = seq(from = as_date(tstart), to = as_date(tend), by = 1)))
  }

  # create directory
  dir_tmp <- str_c(file_temp(),"/zipmeteo")
  dir_create(path = dir_tmp)

  # download data from KNMI
  handle <- new_handle()
  for (s_stn_id in unique(c(stn_id, replace_id))) {
    for (rec in 1:length(start_id)) {
      file_zip <- str_c(dir_tmp, "/KNMI_", s_stn_id, "_", start_id[rec],"-", end_id[rec],".zip")
      url_spec <- url
      url_spec <- str_replace(string = url_spec, pattern = "\\{\\{stn_id\\}\\}", replacement = as.character(s_stn_id))
      url_spec <- str_replace(string = url_spec, pattern = "\\{\\{start_id\\}\\}", replacement = as.character(start_id[rec]))
      url_spec <- str_replace(string = url_spec, pattern = "\\{\\{end_id\\}\\}", replacement = as.character(end_id[rec]))
      download_data(file = file_zip, url = url_spec, handle = handle)
      unzip(zipfile = file_zip, overwrite = TRUE, exdir = dir_tmp)
    }
  }

  # set format data
  if (topt == "day") {
    column <- c("STN", "YYYYMMDD", "DDVEC", "FHVEC", "FG", "FHX", "FHXH", "FHN", "FHNH", "FXX", "FXXH", "TG", "TN", "TNH", "TX", "TXH", "T10N", "T10NH", "SQ", "SP", "Q", "DR", "RH", "RHX", "RHXH", "PG", "PX", "PXH", "PN", "PNH", "VVN", "VVNH", "VVX", "VVXH", "NG", "UG", "UX", "UXH", "UN", "UNH", "EV24")
    col_names <- column[column %in% c("STN", "YYYYMMDD", variable)]
    type <- rep(x = "-", times = length(column))
    type[column %in% c("STN", "YYYYMMDD", variable)] <- c("i", "c", rep(x = "d", times = length(variable)))
    col_types <- str_c(type, collapse = "")
  } else {
    column <- c("STN", "YYYYMMDD", "HH", "DD", "FH", "FF", "FX", "T", "T10N", "TD", "SQ", "Q", "DR", "RH", "P", "VV", "N", "U", "WW", "IX", "M", "R", "S", "O", "Y")
    col_names <- column[column %in% c("STN", "YYYYMMDD", "HH", variable)]
    type <- rep(x = "-", times = length(column))
    type[column %in% c("STN", "YYYYMMDD", "HH", variable)] <- c("i", "c", "i", rep(x = "d", times = length(variable)))
    col_types <- str_c(type, collapse = "")
  }


  # load data
  db <- NULL
  for (s_stn_id in unique(c(stn_id, replace_id))) {
    for (rec in 1:length(start_id)) {
      if (topt == "day") {
        file_txt <- str_c(dir_tmp, "/etmgeg_", s_stn_id, ".txt")
        skip <- 53
      } else {
        file_txt <- str_c(dir_tmp, "/uurgeg_", s_stn_id, "_", start_id[rec], "-", end_id[rec],".txt")
        skip <- 33
      }

      db_tmp <- read_csv(file = file_txt, col_names = col_names, col_types = col_types, skip = skip, lazy = FALSE) %>%
        rename(stn_id = STN) %>%
        mutate(date = as_date(YYYYMMDD)) %>%
        filter(date >= tstart & date <= tend)
      db <- rbind(db, db_tmp)
    }
  }

  # replace minimum observation
  for (s_variable in variable) {
    if ("Replace" %in% names(opt_variable[[s_variable]])) {
      value_old <- opt_variable[[s_variable]]$Replace[1]
      value_new <- opt_variable[[s_variable]]$Replace[2]
      db <- db %>%
        rename(value = all_of(s_variable)) %>%
        mutate(value = if_else(value == value_old, value_new, value)) %>%
        rename(!!s_variable := value)
    }
  }

  # convert unit
  for (s_variable in variable) {
    if ("Factor" %in% names(opt_variable[[s_variable]])) {
      multiplier <- opt_variable[[s_variable]]$Factor
      db <- db %>%
        rename(value = all_of(s_variable)) %>%
        mutate(value = value * multiplier) %>%
        rename(!!all_of(s_variable) := value)
    }
  }

  # add date information
  if (topt == "day") {
    db <- db %>%
      mutate(time = "00:00:00") %>%
      select(date, time, stn_id, all_of(variable))
  } else {
    db <- db %>%
      mutate(time = str_c(formatC(x = HH - 1, format = "d", width = 2, flag = "0"), ":00:00")) %>%
      select(date, time, stn_id, all_of(variable))

    start_id <- str_replace_all(string = tstart, pattern = "-", replacement = "")
    end_id <- str_replace_all(string = tend, pattern = "-", replacement = "")
  }

  # set data for replacement
  if (!is.null(replace)) {
    db_replace <- db_empty %>% add_column(stn_id = replace_id)
    db_replace <- left_join(x = db_replace, y = db[db$stn_id == replace_id, ], by = c("date", "time", "stn_id")) %>%
      select(date, time, stn_id, all_of(variable))
  }

  # loop over stations
  for (s_stn_id in stn_id) {

    # create data with all available observations
    s_db <- db_empty %>% add_column(stn_id = s_stn_id)
    s_db <- left_join(x = s_db, y = db[db$stn_id == s_stn_id, ], by = c("date", "time", "stn_id")) %>%
      select(date, time, stn_id, all_of(variable))

    # check missing values
    found <- FALSE
    for (s_variable in variable) {
      index <- is.na(s_db[, c(s_variable)])
      n_row <- nrow(s_db[index, ])
      if (n_row != 0) {
        stn_name <- station[match(s_stn_id, stn_id)]
        if (!found) message(str_c("\nmissing observations found for ", stn_name, " (", s_stn_id,"):"))
        found <- TRUE
        if (is.null(replace)) {
          message(str_c(" - ", n_row, " observation(s) missing for variable '", s_variable, "'"))
        } else {
          if (replace_id != s_stn_id) s_db[index, s_variable] <- db_replace[index, s_variable]
          n_fill <- n_row - nrow(s_db[is.na(s_db[, c(s_variable)]), ])
          message(str_c(" - ", n_row, " observation(s) missing for variable '", s_variable, "'; ", n_fill, " observation(s) replaced"))
        }
      }
    }
    if (found) message("")

    # write file
    dir_create(dir_out)
    file <- str_c(dir_out, "/KNMI_", s_stn_id, "_", start_id, "-", end_id, ".csv")
    write_csv(x = s_db, file = file, progress = FALSE, quote = "none")
  }

  # clean directory
  dir_delete(path = dir_tmp)
}

#' Download observations from \href{https://www.knmi.nl/home}{KNMI} precipitation station
#'
#' @param dir_out character string, name of directory
#' @param station character string, name(s) of precipitation stations
#' @param tstart date, start of period
#' @param tend date, end of period
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_sub str_replace_all str_to_upper
#' @importFrom fs path_package dir_create dir_delete
#' @importFrom readr read_rds read_csv write_csv
#' @importFrom curl new_handle handle_setform
#' @importFrom dplyr %>% left_join mutate rename select filter if_else
#' @importFrom lubridate as_date
#' @importFrom controlR download_data
#' @export download_precipitation_KNMI
#' @examples
#' # download precipitation from KNMI (do not run!)
#' #download_precipitation_KNMI(dir_out = "../temp_knmi",
#' # station = "de-Bilt", tstart = "1980-01-01", tend = "1980-12-31")
download_precipitation_KNMI <- function(dir_out, station, tstart, tend, ...) {

  # ---- initial part of procedure ----

  YYYYMMDD <- RD <- REPLACE <- time <- NULL

  # set optional arguments
  opt_param <- c("replace", "url", "opt_station", "opt_variable")
  for (name in opt_param) assign(name, NULL)

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(opt_station)) opt_station <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_station_precipitation.rds"))
  if (is.null(opt_variable)) opt_variable <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_variable_precipitation.rds"))
  if (is.null(url)) url <- "https://cdn.knmi.nl/knmi/map/page/klimatologie/gegevens/monv_reeksen/neerslaggeg_{{station}}_{{stn_id}}.zip"

  # ---- main part of procedure ----

  # set stns
  stn_id <- replace_id <- NULL
  for (s_station in station) {
    rec <- match(x = str_to_upper(s_station), table = names(opt_station))
    if (is.na(rec)) stop(str_c("unknown station selected: '", s_station, "'\n check 'opt_station_precipitation_KNMI'"))
    stn_id <- c(stn_id, opt_station[[rec]]$STN)
  }
  if (!is.null(replace)) {
    rec <- match(x = str_to_upper(replace), table = names(opt_station))
    if (is.na(rec)) stop(str_c("unknown station selected: '", replace, "'\n check 'opt_station_precipitation_KNMI'"))
    replace_id <- opt_station[[rec]]$STN
  }

  # set period
  start_id <- str_replace_all(string = tstart, pattern = "-", replacement = "")
  end_id <- str_replace_all(string = tend, pattern = "-", replacement = "")

  # create directory
  dir_tmp <- str_c(file_temp(),"/zipmeteo")
  dir_create(path = dir_tmp)

  # download data from KNMI
  handle <- new_handle()
  for (s_station in unique(c(station, replace))) {
    s_stn_id <- opt_station[[match(x = str_to_upper(s_station), table = names(opt_station))]]$STN
    file_zip <- str_c(dir_tmp, "/KNMI_", formatC(x = s_stn_id, format = "d", width = 3, flag = "0"), "_", start_id,"-", end_id,".zip")
    url_spec <- url
    url_spec <- str_replace(string = url_spec, pattern = "\\{\\{stn_id\\}\\}", replacement = formatC(x = s_stn_id, format = "d", width = 3, flag = "0"))
    url_spec <- str_replace(string = url_spec, pattern = "\\{\\{station\\}\\}", replacement = str_to_upper(s_station))
    download_data(file = file_zip, url = url_spec, handle = handle)
    unzip(zipfile = file_zip, overwrite = TRUE, exdir = dir_tmp)
  }

  # load data
  db <- NULL
  for (s_station in unique(c(station, replace))) {
    s_stn_id <- opt_station[[match(x = str_to_upper(s_station), table = names(opt_station))]]$STN
    file_txt <- str_c(dir_tmp, "/neerslaggeg_", str_to_upper(s_station), "_", formatC(x = s_stn_id, format = "d", width = 3, flag = "0"), ".txt")
    col_names <- c("stn_id", "YYYYMMDD", "RD")
    col_types <- "icd--"
    skip <- 24

    db_tmp <- read_csv(file = file_txt, col_names = col_names, col_types = col_types, skip = skip, lazy = FALSE) %>%
      mutate(
        date = as_date(YYYYMMDD),
        time = "08:00:00"
      ) %>%
      filter(date >= tstart & date <= tend) %>%
      select(date, time, stn_id, RD)
    db <- rbind(db, db_tmp)
  }

  # convert unit
  multiplier <- opt_variable$RD$Factor
  db <- db %>%
    mutate(RD = RD * multiplier)

  # set data for replacement
  if (!is.null(replace)) {
    db_replace <- db %>%
      filter(stn_id == replace_id) %>%
      rename(REPLACE = RD) %>%
      select(-stn_id)
  }

  # loop over stations
  for (s_stn_id in stn_id) {

    s_db <- db %>%
      filter(stn_id == s_stn_id)

    # check missing values
    found <- FALSE
    n_row <- nrow(s_db %>% filter(is.na(RD)))
    if (n_row != 0) {
      if (!found) message("\nmissing observations found:")
      found <- TRUE
      if (is.null(replace)) {
        message(str_c(" - ", n_row, " observation(s) missing for variable 'RD'"))
      } else {
        if (replace_id != s_stn_id) {
          s_db <- left_join(x = s_db, y = db_replace, by = c("date", "time")) %>%
            mutate(RD = if_else(is.na(RD), REPLACE, RD))
        }
        n_fill <- n_row -  nrow(s_db %>% filter(is.na(RD)))
        message(str_c(" - ", n_row, " observation(s) missing for variable 'RD'; ", n_fill, " observation(s) replaced"))
      }
    }

    # write file
    dir_create(dir_out)
    file <- str_c(dir_out, "/KNMI_", formatC(x = s_stn_id, format = "d", width = 3, flag = "0"), "_", start_id, "-", end_id, "_prec.csv")
    write_csv(x = s_db, file = file, progress = FALSE, quote = "none")
  }

  # clean directory
  dir_delete(path = dir_tmp)
}
