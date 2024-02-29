#' Double-mass curve for precipitation from \href{https://www.knmi.nl/home}{KNMI} meteo station
#'
#' @param dir_out character string, name of directory
#' @param station character string, name(s) of meteo stations
#' @param tstart date, start of period
#' @param tend date, end of period
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_to_upper
#' @importFrom fs path_package path_file
#' @importFrom readr read_rds read_csv
#' @importFrom dplyr %>% mutate select group_by summarise
#' @importFrom controlR create_label get_my_theme
#' @importFrom grDevices png dev.off
#' @importFrom ggplot2 ggplot aes_string geom_line geom_abline coord_equal labs scale_color_manual
#' @importFrom pals viridis
#' @export analyse_precipitation_KNMI
#' @examples
#' # analyse meteo from KNMI (do not run!)
#' #analyse_precipitation_KNMI(dir_out = "../temp_knmi",
#' # station = c("De Bilt", "Maastricht"), tstart = "1990-01-01", tend = "2010-12-31")
analyse_precipitation_KNMI <- function(dir_out, station, tstart, tend, ...) {

  # ---- initial part of procedure ----

  RH_ref <- RH <- NULL

  # set optional arguments
  opt_param <- c("topt", "replace", "opt_station", "opt_variable", "width", "height", "language", "quiet")
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
  if (is.null(width)) width <- 8
  if (is.null(height)) height <- 8
  if (!is.null(language)) {
    if (!tolower(language) %in% c("dutch", "english")) stop("language options are 'English' or 'Dutch'")
  } else {
    language <- "english"
  }
  if (is.null(quiet)) quiet <- TRUE

  variable <- "RH"


  # ---- main part of procedure ----

  download_meteo_KNMI(dir_out = dir_out, station = station, replace = replace, variable = variable, tstart = tstart, tend = tend, topt = topt)

  # loop over station
  db_fig <- db_ref <- NULL
  for (s_station in station) {

    # set file
    stn_id <- opt_station[[match(x = str_to_upper(s_station), table = names(opt_station))]]$STN
    start_id <- str_replace_all(string = tstart, pattern = "-", replacement = "")
    end_id <- str_replace_all(string = tend, pattern = "-", replacement = "")
    file_knmi <- str_c(dir_out, "/KNMI_", stn_id, "_", start_id, "-", end_id, ".csv")

    # load data
    db_stn <- read_csv(file = file_knmi, col_names = TRUE, col_types = "Dtid", lazy = FALSE, progress = FALSE) %>%
      mutate(station = all_of(s_station)) %>%
      select(date, station, RH)

    # combine data
    if (is.null(db_ref)) {
      stn_ref <- stn_id
      db_ref <- db_stn %>%
        rename(RH_ref = RH) %>%
        select(date, RH_ref)
    } else {
      stn <- stn_id
      db_tmp <- left_join(x = db_ref, y = db_stn, by = "date")
      db_fig <- rbind(db_fig, db_tmp)
    }
  }

  # remove date with missing value
  missing <- db_fig %>%
    filter(is.na(RH_ref) | is.na(RH)) %>%
    select(date) %>%
    deframe()
  db_fig <- db_fig %>%
    filter(!date %in% missing)

  db_fig <- db_fig %>%
    group_by(station) %>%
    summarise(
      RH_ref = cumsum(RH_ref),
      RH = cumsum(RH),
      .groups = "drop"
    )

  # set y-limit
  value_max <- ceiling(max(db_fig$RH_ref, db_fig$RH) / 1000) * 1000

  # sel labels
  if (length(station) == 2) {
    namfig <- str_c(dir_out, "/prec_", stn_ref, "-vs-", stn, ".png")
    if (tolower(language) == "english") label_y <- str_c("cum. precipitation ", station[2])
    if (tolower(language) == "dutch") label_y <- str_c("cum. neerslag ", station[2])
    color <- "blue"
  } else {
    namfig <- str_c(dir_out, "/prec_", stn_ref, "-vs.png")
    if (tolower(language) == "english") {
      label_x <- str_c("cum. precipitation ", station[1])
      label_y <- "cum. precipitation"
    }
    if (tolower(language) == "dutch") {
      label_x <- str_c("cum. neerslag ", station[1])
      label_y <- "cum. neerslag"
    }
    color <- viridis(length(station) - 1)
  }

  # open device
  png(filename = namfig, width = width*150, height = height*150, res = 144)

  # create plot
  G = ggplot(data = db_fig)
  G = G + geom_abline(slope = 1, colour = "grey65")
  G = G + geom_line(aes_string(x = "RH_ref", y = "RH", colour = "station"), size = 1)
  G = G + scale_color_manual(name = create_label(label = "Station"), values = color)
  G = G + coord_equal(xlim = c(0, value_max), ylim = c(0, value_max))
  G = G + labs(x = create_label(label = label_x, unit = "mm"), y = create_label(label = label_y, unit = "mm"))
  G = G + get_my_theme("figure")
  print(G)

  # close device
  suppressMessages(dev.off())
  if (!quiet) message(str_c("create plot: ", path_file(namfig)))
}

#' Create overview of available observations from \href{https://www.knmi.nl/home}{KNMI} or \href{https://cds.climate.copernicus.eu}{AgERA5}
#'
#' @description Based on the data (\code{file}) an overview will be created with available observations present in the data.
#' @param file character string, filename of raw-data created by \code{download_meteo_KNMI} or \code{download_meteo_AgERA5}
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_replace
#' @importFrom fs path_package path_dir path_file path_ext
#' @importFrom readr read_rds read_csv
#' @importFrom lubridate year
#' @importFrom controlR dayyear create_label get_my_theme
#' @importFrom ggplot2 ggplot aes_string geom_raster scale_fill_gradientn scale_y_reverse scale_x_continuous labs ggsave ggplotGrob
#' @importFrom pals viridis
#' @importFrom grid unit.pmax
#' @importFrom rlang := !! parse_expr
#' @details Hourly data will be aggregated to daily values before plotting.
#' Optionally a selection of the data can be analysed by specifying either \code{variable} and/or the period using \code{tstart} and \code{tend}.
#' The size of the plot can be adjusted by specifying \code{width} (default 10.0) and/or \code{height} (default 16.0).
#' @export analyse_meteo
analyse_meteo <- function(file, ...) {

  # ---- initial part of procedure ----

  daynr <- value <- NULL

  # set optional arguments
  opt_param <- c("variable", "tstart", "tend", "width", "height", "opt_variable", "quiet")
  for (name in opt_param) assign(name, NULL)

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(width)) width <- 10.0
  if (is.null(height)) height <- 16.0
  if (is.null(opt_variable)) opt_variable <- read_rds(file = str_c(path_package(package = "KNMItools"), "/rds/variable_meteo.rds"))
  if (is.null(quiet)) quiet <- FALSE

  # settings of x-axes
  labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  first_day_decade_365 <- c(1, 11, 21, 32, 42, 52, 60, 70, 80, 91, 101, 111, 121, 131, 141, 152, 162, 172, 182, 192, 202, 213, 223, 233, 244, 254, 264, 274, 284, 294, 305, 315, 325, 335, 345, 355)
  breaks <- first_day_decade_365[seq(from = 1, to = 36, by = 3)]
  minor_breaks <- first_day_decade_365[sort(c(seq(from = 2, to = 36,by = 3), seq(from = 3, to = 36, by = 3)))]

  # ---- main part of procedure ----

  # check selected variables
  if (!is.null(variable)) {
    s_variable <- variable[!variable %in% names(opt_variable)]
    if (length(s_variable) > 0) stop(str_c("unknown variable selected: '", str_c(s_variable, collapse = "' '"), "'"))
  }

  # load data
  db <- read_csv(file = file, lazy = TRUE, progress = FALSE, show_col_types = FALSE, comment = "*")

  # optionally filter data based on specified date
  if (!is.null(tstart) | !is.null(tend)) {
    if (is.null(tstart)) tstart <- min(db$date)
    if (is.null(tend)) tend <- max(db$date)
    db <- db %>%
      filter(date >= as_date(tstart) & date <= as_date(tend))
    if (nrow(db) == 0) stop("no records left to analyse; adjust 'tstart' or 'tend'")
  }

  # set and check variable if needed
  if (is.null(variable)) {
    # set variable
    variable <- names(db)[!names(db) %in% c("date", "time", "stn_id")]
  } else {
    # check downloaded variables
    s_variable <- variable[!variable %in% names(db)]
    if (length(s_variable) > 0) stop(str_c("selected variable not in download: '", str_c(s_variable, collapse = "' '"), "'"))
  }

  # aggregate data (in case of hourly data)
  if (!all(unique(db$time) == 00:00:00)) {
    db_agg <- tibble(date = sort(unique(db$date)))
    for (s_variable in variable) {

      # set expression
      expression <- opt_variable[[s_variable]]$Aggregate

      db_tmp <- db %>%
        select(date, all_of(s_variable)) %>%
        group_by(date) %>%
        summarise(
          !!all_of(s_variable) := !!parse_expr(expression),
          .groups = "drop"
        )
      db_agg <- left_join(x = db_agg, y = db_tmp, by = "date")
    }
    db <- db_agg
  }

  # extract data
  db <- db %>%
    mutate(
      year = year(date),
      daynr = dayyear(date)
    ) %>%
    select(year, daynr, all_of(variable))

  # create list of figures
  figures <- list()
  for (s_variable in variable) {

    # subset data
    db_fig <- db %>%
      rename(value = all_of(s_variable)) %>%
      select(year, daynr, value)

    # set labels
    label <- opt_variable[[s_variable]]$Label
    unit <- opt_variable[[s_variable]]$Unit

    # create plot
    G = ggplot(data = db_fig) %>%
      + geom_raster(aes_string(x = "daynr", y = "year", fill = "value"), na.rm = TRUE) %>%
      + scale_y_reverse(breaks = unique(db_fig$year)) %>%
      + scale_x_continuous(breaks = breaks, minor_breaks = minor_breaks, labels = labels, limits = c(1,366)) %>%
      + scale_fill_gradientn(name = "", colors = viridis(100), na.value = "red") %>%
      + labs(title = create_label(label = label, unit = unit), x = "", y = "") %>%
      + get_my_theme("figure")

    figures[[s_variable]] <- ggplotGrob(G)
  }

  # arrange figures
  G <- figures[[variable[1]]]
  G$widths <- unit.pmax(figures[[s_variable[1]]]$widths)
  for (s_variable in variable[2:length(variable)]) {
    G <- rbind(G, figures[[s_variable]])
    G$widths <- unit.pmax(G$widths, figures[[s_variable]]$widths)
  }

  # save plot
  file_png <- str_replace(string = file, pattern = path_ext(file), replacement = "png")
  ggsave(filename = file_png, plot = G, width = width, height = height, dpi = "retina")
  if (!quiet) message(str_c("create plot: ", path_file(file_png)))

}
