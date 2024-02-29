#' Create meteo files for SWAP (based on KNMI data)
#'
#' Based on observations from \href{https://www.knmi.nl/home}{KNMI}
#'
#' @param dir_out character string, name of directory
#' @param station character string, name(s) of meteo stations
#' @param tstart date, start of period
#' @param tend date, end of period
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_sub str_replace_all str_to_upper
#' @importFrom fs path_package dir_delete dir_create file_temp
#' @importFrom readr read_rds read_csv write_lines write_csv
#' @importFrom lubridate year month day
#' @importFrom dplyr %>% mutate filter select if_else
#' @importFrom KNMItools download_meteo_KNMI actual_vapour_pressure
#' @details Download of meteorological data.
#' Optionally the data is split in yearly files by specifying \code{split} = TRUE.
#' Missing data can be replaced by specifying a second station with the option \code{replace}.
#' @export create_meteo_SWAP
#' @examples
#' # create SWAP meteo files from KNMI (do not run!)
#' #create_meteo_SWAP(dir_out = "../temp_swap",
#' # station = "de Bilt", tstart = "1980-01-01", tend = "1980-12-31")
create_meteo_SWAP <- function(dir_out, station, tstart, tend, ...) {

  YYYY <- MM <- DD <- NULL
  STN <- Q <- TN <- TX <- HUM <- FG <- RH <- EV24 <- DR <- UN <- UX <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("split", "replace")
  split <- replace <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(split)) split <- FALSE

  # load settings KNMI
  opt_station <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_station_meteo.rds"))

  # check period
  tmax <- as_date(Sys.time())
  if (tend >= tmax) stop(str_c("tend should be earlier than ", tmax))

  variable <- c("FG","TN","TX","Q","DR","RH","UX","UN","EV24")

  # ---- main part of procedure ----

  # create directory
  dir_tmp <- file_temp()
  dir_create(path = dir_tmp)

  # download data from KNMI
  download_meteo_KNMI(dir_out = dir_tmp, station = station, variable = variable, tstart = tstart, tend = tend, topt = "day", replace = replace)

  # set info replace (optional)
  if (!is.null(replace)) replace_info <- opt_station[[match(x = str_to_upper(replace), table = names(opt_station))]]

  # loop over station
  for (s_station in station) {

    # set info station
    station_info <- opt_station[[match(x = str_to_upper(s_station), table = names(opt_station))]]

    # set file
    stn_id <- station_info$STN
    start_id <- str_replace_all(string = tstart, pattern = "-", replacement = "")
    end_id <- str_replace_all(string = tend, pattern = "-", replacement = "")
    file_knmi <- str_c(dir_tmp, "/KNMI_", stn_id, "_", start_id, "-", end_id, ".csv")

    # load data
    db <- read_csv(file = file_knmi, col_names = TRUE, col_types = "Dtiddddddddd", lazy = FALSE, progress = FALSE)

    # extract date info
    db <- db %>%
      mutate(
        YYYY = year(date),
        MM = formatC(x = month(date), format = "d", width = 2, flag = "0"),
        DD = formatC(x = day(date), format = "d", width = 2, flag = "0")
      )

    # convert rainfall duration [hour] -> [day]
    db <- db %>% mutate(DR = DR / 24.0)

    # check consistence between rainfall and duration
    # - Set rainfall to zero in case duration is less than 1.e-6
    # - Set duration to zero in case rainfall is less than 1.e-6
    db <- db %>%
      mutate(
        RH = if_else(DR < 1.e-6, 0, RH),
        DR = if_else(RH < 1.e-6, 0, DR)
      )

    # convert relative humidity UG [%] to actual vapour pressure HUM [kPa]
    db <- db %>% mutate(HUM = actual_vapour_pressure(TN = TN, TX = TX, UN = UN, UX = UX))

    # prepare output
    db <- db %>%
      mutate(
        STN = str_c("'", stn_id, "'"),
        Q = if_else(is.na(Q), NA_character_, formatC(x = Q, format = "f", digits = 1)),
        TN = if_else(is.na(TN), NA_character_, formatC(x = TN, format = "f", digits = 1)),
        TX = if_else(is.na(TX), NA_character_, formatC(x = TX, format = "f", digits = 1)),
        HUM = if_else(is.na(HUM), NA_character_, formatC(x = HUM, format = "f", digits = 6)),
        FG = if_else(is.na(FG), NA_character_, formatC(x = FG, format = "f", digits = 1)),
        RH = if_else(is.na(RH), NA_character_, formatC(x = RH, format = "f", digits = 3)),
        EV24 = if_else(is.na(EV24), NA_character_, formatC(x = EV24, format = "f", digits = 1)),
        DR = if_else(is.na(DR), NA_character_, formatC(x = DR, format = "f", digits = 3))
      ) %>%
      select(STN, DD, MM, YYYY, Q, TN, TX, HUM, FG, RH, EV24, DR)

    # create header
    header <- c(
      "*------------------------------------------------------------------------------------",
      "* Source of data    : Royal Netherlands Meterological Institute (https://www.knmi.nl)",
      "* File content      : Meteo from KNMI; input file for SWAP 4.2 (https://swap.wur.nl)",
      "*",
      str_c("* Station           : ", str_to_upper(s_station), " (", station_info$STN,")"),
      str_c("* Longitude         : ", ifelse(is.na(station_info$LON), "unknown", station_info$LON)),
      str_c("* Latitude          : ", ifelse(is.na(station_info$LAT), "unknown", station_info$LAT)),
      str_c("* Altitude          : ", ifelse(is.na(station_info$ALT), "unknown", station_info$ALT)),
      if (!is.null(replace)) str_c("* Missing values are replaced by station ", str_to_upper(replace), " (", replace_info$STN,")"),
      "*",
      "* File generated by : Wageningen Environmental Research",
      str_c("* File generated at : ",str_sub(string = Sys.time(), start = 1, end = 20)),
      "*------------------------------------------------------------------------------------",
      "Station,DD,MM,YYYY,Rad,Tmin,Tmax,Hum,Wind,Rain,ETref,Wet"
    )

    # create output directory
    dir_create(dir_out)

    # create meteo files
    if (!split) {

      # write data
      file <- str_c(dir_out, "/", stn_id, ".met")
      write_lines(x = header, file = file)
      write_csv(x = db, file = file, na = "-99.9", append = TRUE, progress = FALSE, quote = "none")

      if(any(is.na(db))) warning("incomplete data for file: ", file, "\n")

    } else {

      for (year in unique(db$YYYY)) {

        s_db <- db %>% filter(YYYY == year)

        # write data
        file <- str_c(dir_out, "/", stn_id, ".", str_sub(string = year, start = 2, end = 4))
        write_lines(x = header, file = file)
        write_csv(x = s_db, file = file, na = "-99.9", append = TRUE, progress = FALSE, quote = "none")

        if(any(is.na(s_db))) warning("incomplete data for file: ", file, "\n")

      }
    }

    # clean directory
    file_delete(path = file_knmi)
  }
}

#' Create precipitation files for SWAP (based on KNMI data)
#'
#' Based on observations from \href{https://www.knmi.nl/home}{KNMI}
#'
#' @param dir_out character string, name of directory
#' @param station character string, name(s) of meteo stations
#' @param tstart date, start of period
#' @param tend date, end of period
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_sub str_replace_all str_to_upper
#' @importFrom fs path_package dir_create dir_delete file_temp
#' @importFrom readr read_rds read_csv write_lines write_csv
#' @importFrom lubridate year month day hour
#' @importFrom dplyr %>% mutate filter select if_else
#' @importFrom KNMItools download_precipitation_KNMI download_meteo_KNMI
#' @details Download of daily precipitation \code{topt} = 'day' or hourly precipitation \code{topt} = 'hour'.
#' Optionally the data is split in yearly files by specifying \code{split} = TRUE.
#' Missing data can be replaced by specifying a second station with the option \code{replace}.
#' @export create_precipitation_SWAP
#' @examples
#' # create SWAP precipitation files from KNMI (do not run!)
#' #create_precipitation_SWAP(dir_out = "../temp_swap",
#' # station = "de Bilt", tstart = "1980-01-01", tend = "1980-12-31")
create_precipitation_SWAP <- function(dir_out, station, tstart, tend, ...) {

  YYYY <- MM <- DD <- HH <- time <- NULL
  RH <- FacDay <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("topt", "split", "replace")
  topt <- split <- replace <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(topt)) topt <- "day"
  if (is.null(split)) split <- FALSE

  # load settings KNMI
  if (topt == "day") {
    opt_station <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_station_precipitation.rds"))
  } else {
    opt_station <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/knmi_station_meteo.rds"))
  }

  # check period
  tmax <- as_date(Sys.time())
  if (tend >= tmax) stop(str_c("tend should be earlier than ", tmax))

  # ---- main part of procedure ----

  # create directory
  dir_tmp <- file_temp()
  dir_create(path = dir_tmp)

  # download data from KNMI
  if (topt == "day") {
    download_precipitation_KNMI(dir_out = dir_tmp, station = station, tstart = tstart, tend = tend, replace = replace)
  } else {
    download_meteo_KNMI(dir_out = dir_tmp, station = station, variable = "RH", tstart = tstart, tend = tend, topt = topt, replace = replace)
  }

  # set info replace (optional)
  if (!is.null(replace)) replace_info <- opt_station[[match(x = str_to_upper(replace), table = names(opt_station))]]

  # loop over station
  for (s_station in station) {

    # set info station
    station_info <- opt_station[[match(x = str_to_upper(s_station), table = names(opt_station))]]

    # set file
    stn_id <- station_info$STN
    start_id <- str_replace_all(string = tstart, pattern = "-", replacement = "")
    end_id <- str_replace_all(string = tend, pattern = "-", replacement = "")
    if (topt == "day") {
      file_knmi <- str_c(dir_tmp, "/KNMI_", formatC(x = stn_id, format = "d", width = 3, flag = "0"), "_", start_id, "-", end_id, "_prec.csv")
    } else {
      file_knmi <- str_c(dir_tmp, "/KNMI_", formatC(x = stn_id, format = "d", width = 3, flag = "0"), "_", start_id, "-", end_id, ".csv")
    }

    # load data
    db <- read_csv(file = file_knmi, col_names = TRUE, col_types = "Dtid", lazy = FALSE, progress = FALSE)
    if (topt == "day") names(db)[4] <- "RH"

    # extract date info
    db <- db %>%
      mutate(
        YYYY = year(date),
        MM = formatC(x = month(date), format = "d", width = 2, flag = "0"),
        DD = formatC(x = day(date), format = "d", width = 2, flag = "0"),
        HH = hour(time),
        FacDay = HH / 24.0,
        FacDay = if_else(is.na(FacDay), NA_character_, formatC(x = FacDay, format = "f", digits = 2)),
        RH = if_else(is.na(RH), NA_character_, formatC(x = RH, format = "f", digits = 1))
      ) %>%
      select(stn_id, DD, MM, YYYY, FacDay, RH)

    # create header
    header <- c(
      "*------------------------------------------------------------------------------------",
      "* Source of data    : Royal Netherlands Meterological Institute (https://www.knmi.nl)",
      "* File content      : Meteo from KNMI; input file for SWAP 4.2 (https://swap.wur.nl)",
      "*",
      str_c("* Station           : ", str_to_upper(s_station), " (", station_info$STN,")"),
      str_c("* Longitude         : ", ifelse(is.na(station_info$LON), "unknown", station_info$LON)),
      str_c("* Latitude          : ", ifelse(is.na(station_info$LAT), "unknown", station_info$LAT)),
      str_c("* Altitude          : ", ifelse(is.na(station_info$ALT), "unknown", station_info$ALT)),
      if (!is.null(replace)) str_c("* Missing values are replaced by ", str_to_upper(replace), " (", replace_info$STN,")"),
      "*",
      "* File generated by : Wageningen Environmental Research",
      str_c("* File generated at : ",str_sub(string = Sys.time(), start = 1, end = 20)),
      "*------------------------------------------------------------------------------------",
      "Station,Day,Month,Year,Time,Amount"
    )

    # create output directory
    dir_create(dir_out)

    # create precipitation files
    if (!split) {

      # write data
      file <- str_c(dir_out, "/", formatC(x = stn_id, format = "d", width = 3, flag = "0"), ".rain")
      write_lines(x = header, file = file)
      write_csv(x = db, file = file, na = "-99.9", append = TRUE, progress = FALSE, quote = "none")

      if(any(is.na(db))) warning("incomplete data for file: ", file, "\n")

    } else {

      for (year in unique(db$YYYY)) {

        s_db <- db %>% filter(YYYY == year)

        # write data
        file <- str_c(dir_out, "/", formatC(x = stn_id, format = "d", width = 3, flag = "0"), "_prec.", str_sub(string = year, start = 2, end = 4))
        write_lines(x = header, file = file)
        write_csv(x = s_db, file = file, na = "-99.9", append = TRUE, progress = FALSE, quote = "none")

        if(any(is.na(s_db))) warning("incomplete data for file: ", file, "\n")
      }
    }

    # clean directory
    dir_delete(path = dir_tmp)
  }
}

#' Create meteo files for SWAP (based on AgERA5 data)
#'
#' Based on observations from \href{https://cds.climate.copernicus.eu}{AgERA5}
#'
#' @param dir_out character string, name of directory
#' @param latitude numeric, the latitude of the site in decimal degrees
#' @param longitude numeric, the longitude of the site in decimal degrees
#' @param tstart date, start of period
#' @param tend date, end of period
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_sub str_replace_all str_to_upper
#' @importFrom fs path_package dir_delete dir_create file_temp
#' @importFrom readr read_rds read_csv write_lines write_csv
#' @importFrom lubridate year month day
#' @importFrom dplyr %>% mutate filter select if_else
#' @importFrom KNMItools download_meteo_AgERA5
#' @details Download of meteorological data.
#' Optionally the data is split in yearly files by specifying \code{split} = TRUE.
#' Missing data can be replaced by specifying a second station with the option \code{replace}.
#'
#' It is known that the precipitation predicted by AgERA5 is biased in certain areas, particularly in the tropics.
#' Therefore this procedure provides the opportunity to replace the precipitation estimates from AgERA5 with precipitation
#' estimates from CHIRPS by specifying \code{opt_rain}.
#' @export create_meteo_AgERA5
#' @examples
#' # create SWAP meteo files from AgERA5 (do not run!)
#' #create_meteo_AgERA5(dir_out = "../temp_swap",
#' # latitude = 52.1, longitude = 5.18, tstart = "2000-01-01", tend = "2001-12-31")
create_meteo_AgERA5 <- function(dir_out, latitude, longitude, tstart, tend, ...) {

  YYYY <- MM <- DD <- NULL
  STN <- radiation <- temperature_min <- temperature_max <- vapourpressure <- windspeed <- precipitation <- etref <- duration <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("opt_rain", "split")
  opt_rain <- split <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(opt_rain)) opt_rain <- "AgERA5"
  if (!opt_rain %in% c("AgERA5", "CHIRPS")) stop("rain should be specified with 'AgERA5' (default) or 'CHIRPS'")
  if (is.null(split)) split <- FALSE

  # check period
  tmax <- as_date(Sys.time())
  if (tend >= tmax) stop(str_c("tend should be earlier than ", tmax))

  variable <- c("windspeed", "temperature_min", "temperature_max", "radiation", "precipitation", "vapourpressure")

  # ---- main part of procedure ----

  # create directory
  dir_tmp <- file_temp()
  dir_create(path = dir_tmp)

  # download data from KNMI
  download_meteo_AgERA5(dir_out = dir_tmp, latitude = latitude, longitude = longitude, variable = variable, tstart = tstart, tend = tend, opt_rain = opt_rain)

  # set file
  start_id <- str_replace_all(string = tstart, pattern = "-", replacement = "")
  end_id <- str_replace_all(string = tend, pattern = "-", replacement = "")
  file_AgERA <- str_c(dir_tmp, "/AgERA5", ifelse(opt_rain == "CHIRPS", "-CHIRPS", ""), "_", start_id, "-", end_id, ".csv")

  # load header
  header <- read_lines(file = file_AgERA, n_max = 10)
  long <- str_trim(string = str_split(string = header[str_detect(string = header, pattern = "Longitude")], pattern = ":", simplify = TRUE)[2])
  lat <- str_trim(string = str_split(string = header[str_detect(string = header, pattern = "Latitude")], pattern = ":", simplify = TRUE)[2])
  alt <- str_trim(string = str_split(string = header[str_detect(string = header, pattern = "Altitude")], pattern = ":", simplify = TRUE)[2])

  long_id <- formatC(x = as.numeric(long) * 100, format = "d", width = 4, flag = "0")
  lat_id <- formatC(x = as.numeric(lat) * 100, format = "d", width = 4, flag = "0")

  # load data
  db <- read_csv(file = file_AgERA, col_names = TRUE, comment = "*", col_types = "D-dddddd", lazy = FALSE, progress = FALSE)

  # extract date info
  db <- db %>%
    mutate(
      YYYY = year(date),
      MM = formatC(x = month(date), format = "d", width = 2, flag = "0"),
      DD = formatC(x = day(date), format = "d", width = 2, flag = "0")
    )

  # prepare output
  db <- db %>%
    mutate(
      STN = opt_rain,
      radiation = if_else(is.na(radiation), NA_character_, formatC(x = radiation, format = "f", digits = 1)),
      temperature_min = if_else(is.na(temperature_min), NA_character_, formatC(x = temperature_min, format = "f", digits = 1)),
      temperature_max = if_else(is.na(temperature_max), NA_character_, formatC(x = temperature_max, format = "f", digits = 1)),
      vapourpressure = if_else(is.na(vapourpressure), NA_character_, formatC(x = vapourpressure, format = "f", digits = 6)),
      windspeed = if_else(is.na(windspeed), NA_character_, formatC(x = windspeed, format = "f", digits = 1)),
      precipitation = if_else(is.na(precipitation), NA_character_, formatC(x = precipitation, format = "f", digits = 3)),
      etref = NA_character_,
      duration = NA_character_
    ) %>%
    select(STN, DD, MM, YYYY, radiation, temperature_min, temperature_max, vapourpressure, windspeed, precipitation, etref, duration)

  # create header
  header <- c(
    "*------------------------------------------------------------------------------------",
    str_c("* Source of data    : AgERA5", ifelse(opt_rain == "CHIRPS", "-CHIRPS", ""), " (https://cds.climate.copernicus.eu)"),
    "* File content      : Meteo from KNMI; input file for SWAP 4.2 (https://swap.wur.nl)",
    "*",
    str_c("* Longitude         : ", long),
    str_c("* Latitude          : ", lat),
    str_c("* Altitude          : ", alt),
    "*",
    "* File generated by : Wageningen Environmental Research",
    str_c("* File generated at : ",str_sub(string = Sys.time(), start = 1, end = 20)),
    "*------------------------------------------------------------------------------------",
    "Station,DD,MM,YYYY,Rad,Tmin,Tmax,Hum,Wind,Rain,ETref,Wet"
  )

  # create output directory
  dir_create(dir_out)

  # create meteo files
  if (!split) {

    # write data
    file <- str_c(dir_out, "/", long_id, "_", lat_id, ".met")
    write_lines(x = header, file = file)
    write_csv(x = db, file = file, na = "-99.9", append = TRUE, progress = FALSE, quote = "none")

    db_tmp <- db %>%
      select(-etref, -duration)
    if (any(is.na(db_tmp))) warning("incomplete data for file: ", file, "\n")

  } else {

    for (year in unique(db$YYYY)) {

      s_db <- db %>% filter(YYYY == year)

      # write data
      file <- str_c(dir_out, "/", long_id, "_", lat_id, ".", str_sub(string = year, start = 2, end = 4))
      write_lines(x = header, file = file)
      write_csv(x = s_db, file = file, na = "-99.9", append = TRUE, progress = FALSE, quote = "none")

      db_tmp <- s_db %>%
        select(-etref, -duration)

      if (any(is.na(db_tmp))) warning("incomplete data for file: ", file, "\n")

    }
  }

  # clean directory
  file_delete(path = file_AgERA)
}
