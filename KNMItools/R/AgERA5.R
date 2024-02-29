#' Download observations from \href{https://cds.climate.copernicus.eu}{AgERA5}
#'
#' @param dir_out character string, name of directory
#' @param latitude numeric, the latitude of the site in decimal degrees
#' @param longitude numeric, the longitude of the site in decimal degrees
#' @param tstart date, start of period
#' @param tend date, end of period
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c
#' @importFrom fs path_package dir_create dir_delete file_temp
#' @importFrom readr read_rds write_csv write_lines
#' @importFrom tibble tibble
#' @importFrom dplyr %>% select
#' @importFrom curl new_handle
#' @importFrom lubridate as_date
#' @importFrom jsonlite read_json
#' @importFrom progress progress_bar
#' @importFrom controlR download_data
#' @export download_meteo_AgERA5
#' @examples
#' # download meteo from AgERA5 (do not run!)
#' #download_meteo_AgERA5(dir_out = "../temp_agera5",
#' # latitude = 52.1, longitude = 5.18, tstart = "2000-01-01", tend = "2001-12-31")
download_meteo_AgERA5 <- function(dir_out, latitude, longitude, tstart, tend, ...) {

  # ---- initial part of procedure ----

  time <- value <- NULL

  # set optional arguments
  opt_param <- c("opt_rain", "url", "variable", "opt_variable", "quiet")
  for (name in opt_param) assign(name, NULL)

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(opt_rain)) opt_rain <- "AgERA5"
  if (!opt_rain %in% c("AgERA5", "CHIRPS")) stop("rain should be specified with 'AgERA5' (default) or 'CHIRPS'")
  if (is.null(url)) {
    if (opt_rain == "AgERA5") url <- "https://agera5.containers.wurnet.nl/api/v1/get_agera5?latitude={{lat_id}}&longitude={{long_id}}&startdate={{start_id}}&enddate={{end_id}}"
    if (opt_rain == "CHIRPS") url <- "https://agera5.containers.wurnet.nl/api/v1/get_agera5_chirps?latitude={{lat_id}}&longitude={{long_id}}&startdate={{start_id}}&enddate={{end_id}}"
  }
  if (is.null(variable)) variable <- c("windspeed", "temperature_avg", "temperature_min", "temperature_max", "radiation", "precipitation", "vapourpressure", "snowdepth")
  if (is.null(opt_variable)) opt_variable <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/variable_meteo.rds"))
  if (is.null(quiet)) quiet <- FALSE

  # ---- main part of procedure ----

  # check selected variables
  s_variable <- variable[!variable %in% names(opt_variable)]
  if (length(s_variable) > 0) stop(str_c("unknown variable selected: '", str_c(s_variable, collapse = "' '"), "'"))

  # check selected location
  if (opt_rain == "AgERA5" & (latitude < -66.5 | latitude > 66.5)) stop("selected location is outside range; latitude only available from -66.5 to 66.5")
  if (opt_rain == "CHIRPS" & (latitude < -50.0 | latitude > 50.0)) stop("selected location is outside range; latitude only available from -50.0 to 50.0")

  # set period
  start_id <- str_replace_all(string = tstart, pattern = "-", replacement = "")
  end_id <- str_replace_all(string = tend, pattern = "-", replacement = "")

  # create directory
  dir_tmp <- str_c(file_temp(),"/meteo")
  dir_create(path = dir_tmp)

  # download data from AgERA5 (located at wurnet)
  handle <- new_handle()
  file_json <- str_c(dir_tmp, "/AgERA5_", start_id,"-", end_id,".json")
  url_spec <- url
  url_spec <- str_replace(string = url_spec, pattern = "\\{\\{lat_id\\}\\}", replacement = as.character(latitude))
  url_spec <- str_replace(string = url_spec, pattern = "\\{\\{long_id\\}\\}", replacement = as.character(longitude))
  url_spec <- str_replace(string = url_spec, pattern = "\\{\\{start_id\\}\\}", replacement = start_id)
  url_spec <- str_replace(string = url_spec, pattern = "\\{\\{end_id\\}\\}", replacement = end_id)
  download_data(file = file_json, url = url_spec, handle = handle)

  # load data
  db_json <- read_json(path = file_json)
  if (!db_json$success) stop(str_c("unable to download weather data from AgERA5", ifelse(opt_rain == "CHIRPS", "-CHIRPS", "")))
  db_raw <- db_json$data

  # create empty data frame
  db <- tibble(
    date = seq(from = as_date(tstart), to = as_date(tend), by = 1), time = "00:00:00",
    windspeed = NA_real_, temperature_avg = NA_real_, temperature_min = NA_real_, temperature_max = NA_real_,
    radiation = NA_real_, precipitation = NA_real_, vapourpressure = NA_real_, snowdepth = NA_real_
    ) %>%
    select(date, time, all_of(variable))

  # fill empty data frame
  n_rec <- length(db_raw$weather_variables)
  if (!quiet) pb <- progress_bar$new(total = n_rec, format = str_c("process AgERA5", ifelse(opt_rain == "CHIRPS", "-CHIRPS", ""), " data: :percent"), clear = TRUE)
  for (i_rec in 1:n_rec) {

    db_tmp <- db_raw$weather_variables[[i_rec]]

    date <- as_date(db_tmp$day)
    rec <- match(x = date, table = db$date)

    for (s_variable in variable) {
      db[[s_variable]][rec] <- db_tmp[[s_variable]]
    }

    # set progress
    if (!quiet) pb$tick()
  }

  # convert unit
  for (s_variable in variable) {
    if (!is.null(opt_variable[[s_variable]]$Factor)) {
      multiplier <- opt_variable[[s_variable]]$Factor
      db <- db %>%
        rename(value = all_of(s_variable)) %>%
        mutate(value = value * multiplier) %>%
        rename(!!all_of(s_variable) := value)
    }
  }

  # create header
  header <- c(
    "*------------------------------------------------------------------------------------",
    str_c("* Source of data    : AgERA5", ifelse(opt_rain == "CHIRPS", "-CHIRPS", ""), " (https://cds.climate.copernicus.eu)"),
    "*",
    str_c("* Longitude         : ", db_raw$location_info$grid_agera5_latitude),
    str_c("* Latitude          : ", db_raw$location_info$grid_agera5_longitude),
    str_c("* Altitude          : ", db_raw$location_info$grid_agera5_elevation),
    "*",
    "* File generated by : Wageningen Environmental Research",
    str_c("* File generated at : ",str_sub(string = Sys.time(), start = 1, end = 20)),
    "*------------------------------------------------------------------------------------"
  )

  # write file
  dir_create(dir_out)
  file <- str_c(dir_out, "/AgERA5", ifelse(opt_rain == "CHIRPS", "-CHIRPS", ""), "_", start_id, "-", end_id, ".csv")
  write_lines(x = header, file = file)
  write_csv(x = db, file = file, append = TRUE, col_names = TRUE, progress = FALSE, quote = "none")

  # clean directory
  dir_delete(path = dir_tmp)
}
