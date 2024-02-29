#' Create plot with selection of soil profiles of the Dutch soil map
#'
#' @param ... Arguments to pass, either \code{soil_id}, \code{profile_id}, \code{bofek_id} or \code{cluster_id} should be specified. Look at \code{Details} for further arguments to pass.
#' @importFrom readr read_rds read_csv
#' @importFrom fs path_package
#' @importFrom stringr str_c str_detect str_split str_replace_all
#' @importFrom tibble deframe
#' @importFrom dplyr %>% mutate filter select rename arrange left_join group_by ungroup lag
#' @importFrom ggplot2 ggplot aes_string geom_text scale_fill_manual coord_cartesian labs position_stack theme element_blank
#' @importFrom ggpattern geom_col_pattern scale_pattern_manual
#' @importFrom controlR get_my_theme create_label
#' @export plot_profile
#' @description Creating a plot for a selection of soil profiles (either specified by \code{soil_id}, \code{profile_id}, \code{bofek_id} or \code{cluster_id}).
#' @details
#' Base information is the Dutch soil map (1:50,000) distinguishing 370 soil profiles (\href{https://edepot.wur.nl/298371}{de Vries, 1999}).
#' Each soil profile contains a sequence of soil layers that are linked to texture groups of the Staringreeks (\href{https://edepot.wur.nl/512761}{Heinen et al., 2020}).
#' Based on the soil physical properties, i.e. water retention and hydraulic conductivity, clusters of soil profiles can be created.
#'
#' Soil profiles to be included in the plot can be selected by \code{soil_id} (options are 1-370) or \code{profile_id} (vector of soil profile numbers mentioned in \href{https://edepot.wur.nl/541544}{Heinen et al., 2021}).
#' Alternatively the dominant soil profiles within a BOFEK cluster can be selected by specifying \code{bofek_id}. All soil profiles within a BOFEK cluster can be selected by specifying a single \code{cluster_id}.
#' In case of a selection by \code{bofek_id} the type of BOFEK should be specified, options are 'BOFEK2012' (\href{https://edepot.wur.nl/247678}{Wosten et al., 2013}) or 'BOFEK2020' (\href{https://edepot.wur.nl/541544}{Heinen et al., 2021}).
#'
#' The maximum depth of the soil profiles to be plotted is set to -120 cm below soil surface. Optionally this can be adjusted by specifying \code{max_depth}.
#'
#' As default the soil profiles are arranged by area which is presented at the top of the graph, this can be turned of by setting \code{plot_area} to FALSE. In case \code{plot_area} is set to FALSE the soil profiles are arranged by user defined input.
#'
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#' @examples
#' # plot selection of soil profiles
#' print(plot_profile(soil_id = c(36, 61, 79, 253, 367)))
#'
#' # plot selection of bofek_id
#' print(plot_profile(bofek_id = c(1006, 2006, 3015, 4018, 5007), type = "BOFEK2020"))
#'
#' # plot selection within BOFEK cluster
#' print(plot_profile(cluster_id = 3015, type = "BOFEK2020"))
plot_profile <- function(...) {

  BOFEK <- sfu <- area <- dominant <- thickness <- NULL
  y_pos <- label <- NULL

  # ---- initial part of procedure ----

  opt_selection <- c("soil_id", "profile_id", "bofek_id", "cluster_id")
  opt_type <- c("BODEM", "BOFEK2012", "BOFEK2020")

  # set optional arguments
  opt_param <- c("soil_id", "profile_id", "bofek_id", "cluster_id", "type", "file_prof", "file_area", "file_sfu", "max_depth", "plot_area", "language")
  soil_id <- profile_id <- bofek_id <- cluster_id <- type <- file_prof <- file_area <- file_sfu <- max_depth <- plot_area <- language <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # load default soilinfo
  soilinfo <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/soilinfo.rds"))

  # check used arguments
  selection <- opt_selection[!c(is.null(soil_id), is.null(profile_id), is.null(bofek_id), is.null(cluster_id))]
  if (length(selection) != 1) stop("either 'soil_id', 'profile_id', 'bofek_id' or 'cluster_id' should be specified")
  if (is.null(type) & selection %in% c("bofek_id", "cluster_id")) stop("in case of 'bofek_id' or 'cluster_id' the BOFEK version should be specified using 'type' (option: 'BOFEK2012' or 'BOFEK2020')")
  if (!is.null(type) & selection %in% c("bofek_id", "cluster_id")) {
    if (!type %in% c("BOFEK2012", "BOFEK2020")) stop("in case of 'bofek_id' or 'cluster_id' the BOFEK version should be specified with 'BOFEK2012' or 'BOFEK2020'")
  }

  if (is.null(file_prof)) db_prof <- soilinfo$profiles
  if (is.null(file_area)) db_area <- soilinfo$area
  if (is.null(file_sfu)) db_sfu <- soilinfo$sfu
  if (is.null(type)) type <- "BODEM"
  if (type == "BOFEK2012") db_link <- soilinfo$BOFEK2012
  if (type == "BOFEK2020") db_link <- soilinfo$BOFEK2020
  if (is.null(max_depth)) max_depth <- -120.0
  if (is.null(plot_area)) plot_area <- TRUE
  if (is.null(language)) language <- "NL"

  # ---- main part of procedure ----

  # load user-information if specified (profile, area, sfu)
  if (!is.null(file_prof)) db_prof <- read_csv(file = file_prof, col_types = "icdd", progress = FALSE)
  if (!is.null(file_area)) db_area <- read_csv(file = file_area, col_types = "ddd", progress = FALSE)
  if (!is.null(file_sfu)) db_sfu <- read_csv(file = file_sfu, col_types = "ccccc", progress = FALSE)

  # check selected type
  if (!type %in% opt_type) stop(str_c("'type' should have one of the following options: '", str_c(opt_type, collapse = "' '"), "'"))

  # select profiles in case of BOFEK id's
  if (selection == "bofek_id") {

    # change label of profile
    db_area <- left_join(x = db_area, y = db_link, by = "soil_id") %>%
      mutate(profile_id = BOFEK) %>%
      select(-BOFEK)

    # extract BOFEK cluster
    db_bofek <- db_link %>%
      filter(BOFEK %in% bofek_id & dominant == 1)

    # check selected BOFEK id's
    if (nrow(db_bofek) != length(bofek_id)) warning(str_c("unknown BOFEK selected: '", str_c(bofek_id[!bofek_id %in% db_bofek$BOFEK], collapse = "' '"), "'"))

    # select soil id's
    s_soil_id <- db_bofek %>%
      mutate(order = match(x = BOFEK, table = bofek_id)) %>%
      arrange(order) %>%
      select(soil_id) %>%
      deframe

    # check selected SOIL id's
    if (length(s_soil_id) == 0) stop("no profiles to plot")
  }

  # select profiles in case of BOFEK cluster
  if (selection == "cluster_id") {

    if (length(cluster_id) > 1) stop("only a single is cluster allowed")
    if (!cluster_id %in% db_link$BOFEK) stop("unknown cluster selected")

    # extract BOFEK cluster
    s_soil_id <- db_link %>%
      filter(BOFEK == cluster_id) %>%
      select(soil_id) %>%
      deframe

    # check selected SOIL id's
    if (length(s_soil_id) == 0) stop("no profiles to plot")
  }

  # select profiles in case of SOIL id
  if (selection == "soil_id") {

    # extract soil_id
    s_soil_id <- soil_id
    s_soil_id <- db_area %>%
      filter(soil_id %in% s_soil_id) %>%
      mutate(order = match(x = soil_id, table = s_soil_id)) %>%
      arrange(order) %>%
      select(soil_id) %>%
      deframe

    # check selected SOIL id's
    if (length(s_soil_id) != length(soil_id)) warning(str_c("unknown soilprofile selected: '", str_c(soil_id[!soil_id %in% s_soil_id], collapse = "' '"), "'"))
    if (length(s_soil_id) == 0) stop("no profiles to plot")

  }

  # select profiles in case of PROFILE id
  if (selection == "profile_id") {

    # extract soil_id
    s_profile_id <- profile_id
    db_profile <- db_area %>%
      filter(profile_id %in% s_profile_id)

    # check selected BOFEK id's
    if (nrow(db_profile) != length(profile_id)) warning(str_c("unknown PROFILE selected: '", str_c(profile_id[!profile_id %in% db_profile$profile_id], collapse = "' '"), "'"))

    # select soil id's
    s_soil_id <- db_profile %>%
      mutate(order = match(x = profile_id, table = s_profile_id)) %>%
      arrange(order) %>%
      select(soil_id) %>%
      deframe

    # check selected SOIL id's
    if (length(s_soil_id) == 0) stop("no profiles to plot")

  }

  # filter soil
  db_soil <- db_prof %>%
    filter(soil_id %in% s_soil_id) %>%
    group_by(soil_id) %>%
    mutate(
      y_pos = pmin(-max_depth, cumsum(thickness)),
      y_label = 0.5 * y_pos + 0.5 * lag(x = y_pos, default = 0)) %>%
    ungroup() %>%
    select(soil_id, sfu, order, thickness, y_label)

  # area info
  db_area <- db_area %>%
    filter(soil_id %in% s_soil_id) %>%
    mutate(label = str_c(signif(x = area, digits = 3), " ha"))

  # order by area (default)
  if (plot_area | selection == "cluster_id") {
    db_area <- db_area %>%
      arrange(-area)
  } else {
    db_area <- db_area %>%
      mutate(order = match(x = soil_id, table = s_soil_id)) %>%
      arrange(order)
  }

  # set dataframe figure and legend
  db_soil <- left_join(x = db_soil, y = db_sfu, by = "sfu") %>%
    rename(label = ifelse(language == "NL", "sfu_label_NL", "sfu_label_EN")) %>%
    mutate(label = str_c(sfu, "\n", str_replace_all(string = label, pattern = "\\\\n", replacement = "\n"))) %>%
    select(soil_id, sfu, order, thickness, y_label, label)

  db_legend <- db_sfu %>%
    filter(sfu %in% db_soil$sfu)

  # create factors
  db_soil$soil_id <- factor(x = db_soil$soil_id, levels = db_area$soil_id, labels = db_area$profile_id, ordered = TRUE)
  db_soil$order <- factor(x = db_soil$order, levels = 1:max(db_soil$order), ordered = TRUE)
  db_area$soil_id <- factor(x = db_area$soil_id, levels = db_area$soil_id, labels = db_area$profile_id, ordered = TRUE)
  db_legend$sfu <- factor(x = db_legend$sfu, levels = db_legend$sfu, ordered = TRUE)

  # set labels
  if (language == "NL") {
    x_label <- ifelse(selection  == "bofek_id", "BOFEK", "profiel")
    y_label <- "diepte"
    y_unit <- "cm+mv"
  }
  if (language == "EN") {
    x_label <- ifelse(selection  == "bofek_id", "BOFEK", "profile")
    y_label <- "depth"
    y_unit <- "cm+MSL"
  }

  # create plot
  P <- ggplot(data = db_soil) +
    geom_col_pattern(aes_string(x = "soil_id", y = "-thickness", fill = "sfu", pattern = "sfu", group = "order"), colour = "grey75", pattern_fill = "grey75", pattern_angle = 10, pattern_density = 0.08, pattern_alpha = 0.5, pattern_linetype = "blank", position = position_stack(reverse = TRUE), alpha = 0.7) +
    geom_text(aes_string(x = "soil_id", y = "-y_label", label = "label"), colour = "grey15", fontface = "bold", size = 2.5) +
    scale_pattern_manual(values = db_legend$pattern, guide = "none") +
    scale_fill_manual(values = db_legend$fill, guide = "none") +
    coord_cartesian(ylim = c(max_depth, 2)) +
    labs(x = create_label(label = x_label), y = create_label(label = y_label, unit = y_unit)) +
    get_my_theme("figure") +
    theme(panel.grid.major.x = element_blank())

  # add area of each soil profile
  if (plot_area) P <- P + geom_text(data = db_area, aes_string(x = "soil_id", y = 2.0, label = "label"), fontface = "italic", size = 3.0)

  # ---- return of procedure ----

  return(P)
}

#' Create timeseries-plot
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param variable character string of SWAP variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds
#' @importFrom fs path_package
#' @importFrom stringr str_c str_replace
#' @importFrom lubridate as_date
#' @importFrom dplyr %>% mutate filter select rename all_of
#' @importFrom rlang := !! parse_expr
#' @importFrom ggplot2 ggplot aes aes_string geom_line geom_col geom_point geom_text scale_colour_manual scale_fill_manual scale_x_date coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_variable
#' @description Creates timeseries-plot for a specific SWAP variable.
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) and SWAP variable (\code{variable}) a timeseries-plot is created.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' In case observations are available it is possible to add the observations as red dots in the plot.
#' A path to a folder with observations should be specified with \code{dir_obs}.
#' In this folder a csv-file with the name of the SWAP-variable should be present.
#' The file itself should have two columns: 'DATETIME' and 'VALUE'. The unit of the values should be equal to SWAP.
#' Optionally the model performance (\code{performance}) can be added in the plot (see also: \link{get_modelperformance}).
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
#' @examples
#' # extract example
#' dir_out <- tempfile(pattern = "swap_")
#' file_swp <- paste0(dir_out, "/swap.swp")
#' file_csv <- paste0(dir_out, "/result_output.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, exdir = dir_out)
#'
#' # plot timeseries of groundwater level
#' plot_variable(file_swp = file_swp, variable = "GWL")
#'
#' # plot timeseries of groundwater level including observations and model performance
#' plot_variable(file_swp = file_swp, variable = "GWL", dir_obs = paste0(dir_out, "/observed"), performance = "rmse")
#'
#' # plot timeseries of water content at 15 cm depth
#' plot_variable(file_swp = file_swp, variable = "WC[-15.0]")
#'
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
plot_variable <- function(file_swp, variable, ...) {

  datetime <- observed <- level <- value <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "ylim", "language", "dir_obs", "performance", "digits", "output_swap", "error", "quiet")
  tstart <- tend <- ylim <- language <- dir_obs <- performance <- digits <- output_swap <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
  if (is.null(digits)) digists <- 3
  if (is.null(language)) language <- "EN"
  if (is.null(output_swap)) output_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_output.rds"))
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # ---- main part of procedure ----

  # check user-specified input
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")
  if (length(variable) > 1) stop("select only one variable!")

  # extract plot settings
  plot_settings <- get_plot_settings(variable = variable, output_swap = output_swap)

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND"), case_sensitive = FALSE, quiet = quiet)
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract
  extract_variable <- str_replace(string = plot_settings$variable, pattern = "\\[depth\\]", replacement = str_c("\\[", plot_settings$depth, "\\]"))
  if (!all(extract_variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(extract_variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = extract_variable) %>%
    rename_all(.funs = str_replace, pattern = str_c("\\[", plot_settings$depth, "\\]"), replacement = "") %>%
    mutate(date = as_date(datetime)) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, all_of(delete_depth(extract_variable)))

  # set value
  expression <- plot_settings$expression
  if (is.null(expression)) {
    db_swp <- db_swp %>%
      rename(value = all_of(delete_depth(extract_variable)))
  } else {
    db_swp <- db_swp %>%
      mutate(value := !!parse_expr(expression)) %>%
      select(date, value)
  }

  # add observations
  db_swp <- add_observed(db = db_swp, dir_obs = dir_obs, variable = variable)
  observations <- !all(is.na(db_swp$observed))

  # set model performance (if possible)
  if (observations & !is.null(performance)) {
    modelperformance <- get_modelperformance(actual = db_swp$observed, predicted = db_swp$value, performance = performance, digits = digits)
    text <- str_c(names(modelperformance), ": ", modelperformance, collapse = "  ")
  }

  # convert unit
  if (!is.null(plot_settings$factor)) {
    db_swp <- db_swp %>%
      mutate(
        value = value * plot_settings$factor,
        observed = observed * plot_settings$factor
      )
  }

  # rearrange data
  db_fig <- db_swp %>%
    mutate(level = all_of(variable)) %>%
    select(date, level, value, observed)

  # set labels
  unit <- plot_settings$unit
  if (language == "NL") {
    if (is.null(unit)) unit <- plot_settings$unit_NL
    labels <- plot_settings$labels_NL
    y_label <- plot_settings$y_label_NL
  }
  if (language == "EN") {
    if (is.null(unit)) unit <- plot_settings$unit_EN
    labels <- plot_settings$labels_EN
    y_label <- plot_settings$y_label_EN
  }
  if (!is.null(plot_settings$depth)) labels <- str_replace(string = labels, pattern = "\\[depth\\]", replacement = plot_settings$depth)

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = variable, labels = labels, ordered = TRUE)

  # create ggplot
  P <- ggplot(data = db_fig)

  # type of plot
  if (plot_settings$graph_type == "line") {
    P <- P +
      geom_line(aes_string(x = "date", y = "value", colour = "level"), na.rm = TRUE, alpha = 0.8) +
      scale_colour_manual(name = "", values = plot_settings$colour)
  }
  if (plot_settings$graph_type == "bar") {
    P <- P +
      geom_col(aes_string(x = "date", y = "value", fill = "level"), na.rm = TRUE, alpha = 0.8) +
      scale_fill_manual(name = "", values = plot_settings$colour)
  }

  # set layout
  P <- P +
    scale_x_date(limits = c(tstart, tend)) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # add observations
  if (observations) {
    P <- P +
      geom_point(aes_string(x = "date", y = "observed"), colour = "red", na.rm = TRUE, alpha = 0.8)

    # add performance
    if (!is.null(performance)) {
      P <- P +
        geom_text(aes(x = tstart, y = min(c(value, observed, ylim), na.rm = TRUE), label = text), hjust = -0.2, vjust = -2, colour = "grey45", fontface = "italic")
    }
  }

  # set return of procedure
  output <- list()
  output$plot <- P
  if (observations & !is.null(performance)) output$modelperformance <- modelperformance

  # ---- return of procedure ----

  return(output)
}

#' Create contour-plot
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param variable character string of SWAP variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds read_csv
#' @importFrom fs path_package
#' @importFrom stringr str_c str_to_upper str_split str_trim
#' @importFrom lubridate as_date
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate filter select rename all_of inner_join lag
#' @importFrom ggplot2 ggplot aes_string geom_rect scale_fill_gradientn scale_x_date coord_cartesian labs
#' @importFrom pals viridis coolwarm
#' @importFrom controlR get_my_theme create_label
#' @export plot_contour
#' @description Creates contour-plot for a specific SWAP variable.
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) and SWAP variable (\code{variable}) a contour-plot is created.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
#' @examples
#' # extract example
#' dir_out <- tempfile(pattern = "swap_")
#' file_swp <- paste0(dir_out, "/swap.swp")
#' file_csv <- paste0(dir_out, "/result_output_tz.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, exdir = dir_out)
#'
#' # plot contour of water content
#' plot_contour(file_swp = file_swp, variable = "WC", tstart = "2016-01-01", ylim = c(-200.0, 0))
#'
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
plot_contour <- function(file_swp, variable, ...) {

  datetime <- depth <- value <- . <- hcomp <- bottom <- top <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "ylim", "language", "output_swap", "error", "quiet")
  tstart <- tend <- ylim <- language <- output_swap <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
  if (is.null(language)) language <- "EN"
  if (is.null(output_swap)) output_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_output.rds"))
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # ---- main part of procedure ----

  # check user-specified input
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # extract plot settings
  plot_settings <- get_plot_settings(variable = variable)

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "INLIST_CSV_TZ", "TSTART", "TEND", str_c("SOILPROFILE::", c("HSUBLAY", "NCOMP"))), case_sensitive = FALSE, quiet = quiet)
  if (is.null(settings$INLIST_CSV_TZ)) stop("no csv-output generated; use SW_CSV_TZ = 1")
  inlist <- str_to_upper(string = str_trim(string = str_split(string = settings$INLIST_CSV_TZ, pattern = ",", simplify = TRUE)))

  # extract discretization
  db_cmp <- NULL
  for (rec in 1:length(settings$HSUBLAY)) {
    db_tmp <- tibble(layer = rep(x = rec, times = settings$NCOMP[rec]), hcomp = rep(x = settings$HSUBLAY[rec] / settings$NCOMP[rec], times = settings$NCOMP[rec]))
    db_cmp <- rbind(db_cmp, db_tmp)
  }

  db_cmp <- db_cmp %>%
    mutate(
      node = 1:nrow(.),
      bottom = cumsum(hcomp),
      top = lag(bottom, default = 0),
      depth = as.integer((0.5 * top + 0.5 * bottom) * -10)
    ) %>%
    mutate(
      bottom = -bottom,
      top = -top
    )

  # set variables to extract
  if (!all(variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(variable, collapse = "' '"), "'; check INLIST_CSV_TZ")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output_tz.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = variable) %>%
    rename(value = all_of(variable)) %>%
    mutate(
      date = as_date(datetime),
      depth = as.integer(depth * 10)
    ) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, depth, value)

  # convert unit
  if (!is.null(plot_settings$factor)) {
    db_swp <- db_swp %>%
      mutate(value = value * plot_settings$factor)
  }

  # combine data
  db_fig <- inner_join(x = db_cmp, y = db_swp, by = "depth") %>%
    select(-depth)

  # set labels
  unit <- plot_settings$unit
  if (language == "NL") {
    if (is.null(unit)) unit <- plot_settings$unit_NL
    y_label <- "depth"
    y_unit <- "cm + mv"
    label <- plot_settings$y_label_NL
  }
  if (language == "EN") {
    if (is.null(unit)) unit <- plot_settings$unit_EN
    y_label <- "depth"
    y_unit <- "cm + sl"
    label <- plot_settings$y_label_EN
  }

  # set colours
  if (plot_settings$colours == "coolwarm") colours <- coolwarm(100)
  if (plot_settings$colours == "viridis") colours <- viridis(100)
  if (plot_settings$flip_colours) colours <- colours[100:1]

  # create ggplot
  P <- ggplot(data = db_fig) +
    geom_rect(aes_string(xmin = "date", xmax = "date + 1", ymin = "bottom", ymax = "top", fill = "value"), na.rm = TRUE, alpha = 0.8) +
    scale_fill_gradientn(name = create_label(label = label, unit = unit, linebreak = TRUE), colours = colours) +
    scale_x_date(limits = c(tstart, tend)) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = y_unit)) +
    get_my_theme("figure")

  # set return of procedure
  output <- list()
  output$plot <- P

  # ---- return of procedure ----

  return(output)
}

#' Create timeseries-plot of actual evapotranspiration
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds read_csv
#' @importFrom stringr str_c
#' @importFrom lubridate as_date NA_Date_
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate filter select rename all_of
#' @importFrom ggplot2 ggplot aes_string geom_col scale_fill_manual scale_x_date coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_etact
#' @description Creates timeseries-plot of the actual evapotranspiration.
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) a timeseries-plot is created of the actual evapotranspiration.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' Optionally results can be aggregated in time by \code{aggregate} (options: "year", "month", "decade").
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
plot_etact <- function(file_swp, ...) {

  datetime <- value <- CROPFIL <- TPOT <- TACT <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "ylim", "aggregate", "cropfil", "language", "error", "quiet")
  tstart <- tend <- ylim <- aggregate <- cropfil <- language <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(0,NA_real_)
  if (is.null(language)) language <- "EN"
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # ---- main part of procedure ----

  # check user-specified input
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND", "SWCROP", str_c("CROPROTATION::", c("CROPSTART", "CROPEND", "CROPFIL"))), case_sensitive = FALSE, quiet = quiet)
  settings$SWCROP <- ifelse(is.null(settings$SWCROP), 1, settings$SWCROP)
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract
  variable <- c("EACT", c("TACT", "INTERC")[c("TACT", "INTERC") %in% inlist])
  if (!all(variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # filter crop
  if (!is.null(cropfil) & settings$SWCROP == 1) {
    rotation <- tibble(CROPSTART = settings$CROPSTART, CROPEND = settings$CROPEND, CROPFIL = settings$CROPFIL)
    rotation <- rotation %>%
      filter(CROPFIL %in% all_of(cropfil))
    if (nrow(rotation) == 0) stop("no crop found; check croprotation")
    date_crop <- NA_Date_
    for (rec in 1:nrow(rotation)) date_crop <- c(date_crop, seq.Date(from = rotation$CROPSTART[rec], to = rotation$CROPEND[rec], by = 1))
  }

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = variable) %>%
    mutate(date = as_date(datetime)) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, all_of(variable))

  # filter crop
  if (!is.null(cropfil)) {
    db_swp <- db_swp %>%
      filter(date %in% date_crop)
  }

  # aggregate data (if needed)
  if (!is.null(aggregate)) db_swp <- summarise_db(db = db_swp, level = aggregate, column = variable)

  # rearrange data
  db_fig <- NULL
  levels <- variable
  for (level in levels) {
    db_tmp <- db_swp %>%
      rename(value = all_of(level)) %>%
      mutate(level = all_of(level)) %>%
      select(date, level, value)
    db_fig <- rbind(db_fig, db_tmp)
  }

  # set labels
  if (language == "NL") {
    labels <- c("interceptie", "transpiratie", "evaporatie")
    if (!is.null(aggregate)) unit_time <- ifelse(aggregate == "year", "jaar", ifelse(aggregate == "month", "maand", "decade"))
    y_label <- "evapotranspiratie"
  }
  if (language == "EN") {
    labels <- c("interception", "transpiration", "evaporation")
    if (!is.null(aggregate)) unit_time <- aggregate
    y_label <- "evapotranspiration"
  }
  labels <- labels[c("INTERC", "TACT", "EACT") %in% variable]
  values <- c("lightblue", "darkgreen", "orange")[c("INTERC", "TACT", "EACT") %in% variable]
  unit <- ifelse(is.null(aggregate), str_c("mm d-1"), str_c("mm ", unit_time, "-1"))

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = levels, labels = labels, ordered = TRUE)

  # create plot
  P <- ggplot(data = db_fig) +
    geom_col(aes_string(x = "date", y = "value * 10", fill = "level"), position = "stack", na.rm = TRUE) +
    scale_fill_manual(name = "", values = values) +
    scale_x_date(limits = c(tstart, tend)) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # set return of procedure
  output <- list()
  output$plot <- P
  output$data <- db_swp

  # ---- return of procedure ----

  return(output)
}

#' Create timeseries-plot of simulated transpiration reduction
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds read_csv
#' @importFrom stringr str_c
#' @importFrom lubridate as_date NA_Date_
#' @importFrom tibble tibble add_column
#' @importFrom dplyr %>% mutate filter select rename all_of
#' @importFrom ggplot2 ggplot aes_string geom_col scale_fill_manual scale_x_date coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_tred
#' @description Creates timeseries-plot of transpiration reduction simulated by SWAP.
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) a timeseries-plot is created of the simulated transpiration reduction.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' Transpiration reduction can be caused by too dry ('drought'), too wet ('oxygen'), too saline ('salinity') or too cold ('frost') conditions.
#' By default all stressors are selected to present in the plot.
#' In case an stressor is not active (or not present in the model output) a list of stressors can be specified by \code{stressor}.
#'
#' By default the transpiration reduction is plotted for all crops in the crop-calendar during the simulation period.
#' Specify a list of crops with \code{cropfil} in case only the transpiration reduction of specific crop(s) is preferred.
#'
#' Optionally results can be aggregated in time by \code{aggregate} (options: "year", "month", "decade").
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
#' @examples
#' # extract example
#' dir_out <- tempfile(pattern = "swap_")
#' file_swp <- paste0(dir_out, "/swap.swp")
#' file_csv <- paste0(dir_out, "/result_output.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, files = basename(c(file_swp, file_csv)), exdir = dir_out)
#'
#' # plot timeseries of simulated transpiration reduction by drought stress
#' plot_tred(file_swp = file_swp, stressor = "drought")
#'
#' # plot timeseries of simulated transpiration reduction by drought and oxygen stress
#' plot_tred(file_swp = file_swp, stressor = c("drought", "oxygen"))
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
plot_tred <- function(file_swp, ...) {

  datetime <- value <- CROPFIL <- TPOT <- TREDDRY <- TREDWET <- TREDSOL <- TREDFRS <- . <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "stressor", "ylim", "aggregate", "cropfil", "language", "error", "quiet")
  tstart <- tend <- stressor <- ylim <- aggregate <- cropfil <- language <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(0, NA_real_)
  if (is.null(language)) language <- "EN"
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  ymin_default <- 0

  stressor_default <- c("drought", "oxygen", "salinity", "frost")
  filter_inlist <- FALSE
  if (is.null(stressor)) {stressor <- stressor_default; filter_inlist <- TRUE}

  # ---- main part of procedure ----

  # check user-specified input
  if (!all(stressor %in% stressor_default)) stop("unknown stressor selected; options are 'drought', 'oxygen', 'salinity' and/or 'frost'")
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # check if ymin is set
  if (is.na(ylim[1])) ylim[1] <- ymin_default

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND", "SWCROP", str_c("CROPROTATION::", c("CROPSTART", "CROPEND", "CROPFIL"))), case_sensitive = FALSE, quiet = quiet)
  settings$SWCROP <- ifelse(is.null(settings$SWCROP), 1, settings$SWCROP)
  if (settings$SWCROP == 0) {
    error_message <- "no cropgrowth activated; use SWCROP = 1"
    if (error) stop(error_message) else warning(error_message); return()
  }
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract
  variable <- c("TREDDRY", "TREDWET", "TREDSOL", "TREDFRS")[stressor_default %in% stressor]
  if (filter_inlist) variable <- variable[variable %in% inlist]
  variable <- c("TPOT", variable)
  if (!all(variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # filter crop
  rotation <- tibble(CROPSTART = settings$CROPSTART, CROPEND = settings$CROPEND, CROPFIL = settings$CROPFIL)
  if (!is.null(cropfil)) {
    rotation <- rotation %>%
      filter(CROPFIL %in% all_of(cropfil))
  }
  if (nrow(rotation) == 0) stop("no crop found; check croprotation")
  date_crop <- NA_Date_
  for (rec in 1:nrow(rotation)) date_crop <- c(date_crop, seq.Date(from = rotation$CROPSTART[rec], to = rotation$CROPEND[rec], by = 1))

  # set default values if not exists
  default_column <- c(TREDDRY = 0.0, TREDWET = 0.0, TREDSOL = 0.0, TREDFRS = 0.0)

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = variable) %>%
    add_column(!!!default_column[!names(default_column) %in% names(.)]) %>%
    mutate(
      date = as_date(datetime),
      TPOT = TPOT - (TREDDRY + TREDWET + TREDSOL + TREDFRS)
    ) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, all_of(variable))

  # filter crop
  db_swp <- db_swp %>%
    filter(date %in% date_crop)

  # aggregate data (if needed)
  if (!is.null(aggregate)) db_swp <- summarise_db(db = db_swp, level = aggregate, column = variable)

  # potential transpiration
  db_tpot <- db_swp %>%
    rename(value = TPOT) %>%
    select(date, value)

  # rearrange data
  db_tred <- NULL
  levels <- variable[variable %in% c("TREDDRY", "TREDWET", "TREDSOL", "TREDFRS")]
  for (level in levels) {
    db_tmp <- db_swp %>%
      rename(value = all_of(level)) %>%
      mutate(level = all_of(level)) %>%
      select(date, level, value)
    db_tred <- rbind(db_tred, db_tmp)
  }

  # set labels
  name <- "stress"
  if (language == "NL") {
    labels <- c("droogte", "zuurstof", "zout", "vorst")
    if (!is.null(aggregate)) unit_time <- ifelse(aggregate == "year", "jaar", ifelse(aggregate == "month", "maand", "decade"))
    y_label <- "transpiratie"
  }
  if (language == "EN") {
    labels <- c("drought", "oxygen", "salinity", "frost")
    if (!is.null(aggregate)) unit_time <- aggregate
    y_label <- "transpiration"
  }
  labels <- labels[c("TREDDRY", "TREDWET", "TREDSOL", "TREDFRS") %in% variable]
  values <- c("red", "blue", "orange", "lightblue")[c("TREDDRY", "TREDWET", "TREDSOL", "TREDFRS") %in% variable]
  unit <- ifelse(is.null(aggregate), str_c("mm d-1"), str_c("mm ", unit_time, "-1"))

  # factors
  db_tred$level <- factor(x = db_tred$level, levels = levels, labels = labels, ordered = TRUE)

  # create plot
  P <- ggplot() +
    geom_col(data = db_tpot, aes_string(x = "date", y = "value * 10"), fill = "grey85", na.rm = TRUE, alpha = 0.8) +
    geom_col(data = db_tred, aes_string(x = "date", y = "value * 10", fill = "level"), position = "stack", na.rm = TRUE) +
    scale_fill_manual(name = create_label(label = name), values = values) +
    scale_x_date(limits = c(tstart, tend)) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # set return of procedure
  output <- list()
  output$plot <- P
  output$data <- db_swp

  # ---- return of procedure ----

  return(output)
}

#' Create timeseries-plot of precipitation and irrigation
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds read_csv
#' @importFrom stringr str_c
#' @importFrom lubridate as_date NA_Date_
#' @importFrom dplyr %>% mutate filter select rename all_of
#' @importFrom ggplot2 ggplot aes_string geom_col scale_fill_manual scale_x_date coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_rain_irrig
#' @description Creates timeseries-plot of precipitation and irrigation.
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) a timeseries-plot is created of the precipitation and irrigation.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' Optionally results can be aggregated in time by \code{aggregate} (options: "year", "month", "decade").
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
plot_rain_irrig <- function(file_swp, ...) {

  datetime <- value <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "ylim", "aggregate", "language", "error", "quiet")
  tstart <- tend <- ylim <- aggregate <- language <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(0,NA_real_)
  if (is.null(language)) language <- "EN"
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # ---- main part of procedure ----

  # check user-specified input
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND"), case_sensitive = FALSE, quiet = quiet)
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract
  variable <- "RAIN"
  if ("IRRIG" %in% inlist) variable <- c("IRRIG", variable)
  if (!all(variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = variable) %>%
    mutate(date = as_date(datetime)) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, all_of(variable))

  # aggregate data (if needed)
  if (!is.null(aggregate)) db_swp <- summarise_db(db = db_swp, level = aggregate, column = variable)

  # rearrange data
  db_fig <- NULL
  levels <- variable
  for (level in levels) {
    db_tmp <- db_swp %>%
      rename(value = all_of(level)) %>%
      mutate(level = all_of(level)) %>%
      select(date, level, value)
    db_fig <- rbind(db_fig, db_tmp)
  }

  # set labels
  if (language == "NL") {
    labels <- c("irrigatie", "neerslag")
    if (!is.null(aggregate)) unit_time <- ifelse(aggregate == "year", "jaar", ifelse(aggregate == "month", "maand", "decade"))
    y_label <- "neerslag"
  }
  if (language == "EN") {
    labels <- c("irrigation", "precipitation")
    if (!is.null(aggregate)) unit_time <- aggregate
    y_label <- "precipitation"
  }
  labels <- labels[c("IRRIG", "RAIN") %in% variable]
  values <- c("lightblue", "blue")[c("IRRIG", "RAIN") %in% variable]
  unit <- ifelse(is.null(aggregate), str_c("mm d-1"), str_c("mm ", unit_time, "-1"))

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = levels, labels = labels, ordered = TRUE)

  # create plot
  P <- ggplot(data = db_fig) +
    geom_col(aes_string(x = "date", y = "value * 10", fill = "level"), position = "stack", na.rm = TRUE) +
    scale_fill_manual(name = "", values = values) +
    scale_x_date(limits = c(tstart, tend)) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # set return of procedure
  output <- list()
  output$plot <- P
  output$data <- db_swp

  # ---- return of procedure ----

  return(output)
}

#' Create timeseries-plot of simulated groundwater level
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c
#' @importFrom lubridate as_date
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate filter select rename
#' @importFrom ggplot2 ggplot aes aes_string geom_line geom_hline geom_text geom_point scale_colour_manual scale_x_date coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_gwlevel
#' @description Creates timeseries-plot of groundwater level simulated by SWAP.
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) a timeseries-plot is created of the simulated groundwater level.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' Characteristics of the groundwater level ('GHG' and 'GLG'; see also \link{get_gxg}) can be added to the plot by setting \code{add_gxg} to TRUE.
#'
#' In case observations are available it is possible to add the observations as red dots in the plot.
#' A path to a folder with observations should be specified with \code{dir_obs}.
#' In this folder a csv-file with the name of the SWAP-variable should be present.
#' The file itself should have two columns: 'DATETIME' and 'VALUE'. The unit of the values should be equal to SWAP.
#' Optionally the model performance (\code{performance}) can be added in the plot (see also: \link{get_modelperformance}).
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
#' @examples
#' # extract example
#' dir_out <- tempfile(pattern = "swap_")
#' file_swp <- paste0(dir_out, "/swap.swp")
#' file_csv <- paste0(dir_out, "/result_output.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, files = basename(c(file_swp, file_csv)), exdir = dir_out)
#'
#' # plot timeseries of simulated groundwater level
#' plot_gwlevel(file_swp = file_swp, add_gxg = TRUE)
#'
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
plot_gwlevel <- function(file_swp, ...) {

  datetime <- value <- level <- observed <- GWL <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "ylim", "add_gxg", "language", "dir_obs", "performance", "digits", "error", "quiet")
  tstart <- tend <- ylim <- add_gxg <- language <- dir_obs <- performance <- digits <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(NA_real_, 0)
  if (is.null(digits)) digists <- 3
  if (is.null(add_gxg)) add_gxg <- FALSE
  if (is.null(language)) language <- "EN"
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  ymax_default <- 0

  # ---- main part of procedure ----

  # check user-specified input
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # check if ymax is set
  if (is.na(ylim[2])) ylim[2] <- ymax_default

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND"), case_sensitive = FALSE, quiet = quiet)
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract
  variable <- "GWL"
  if (!all(variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = variable) %>%
    mutate(date = as_date(datetime)) %>%
    filter(date >= tstart & date <= tend) %>%
    rename(value = GWL) %>%
    select(date, value)

  # add observations
  db_swp <- add_observed(db = db_swp, dir_obs = dir_obs, variable = variable)
  observations <- !all(is.na(db_swp$observed))

  # set model performance (if possible)
  if (observations & !is.null(performance)) {
    modelperformance <- get_modelperformance(actual = db_swp$observed, predicted = db_swp$value, performance = performance, digits = digits)
    text <- str_c(names(modelperformance), ": ", modelperformance, collapse = "  ")
  }

  # rearrange data
  db_fig <- db_swp %>%
    mutate(level = "GWL") %>%
    select(date, level, value, observed)

  # groundwater characteristics
  gxg <- c(NA_real_, NA_real_)
  if (add_gxg) {
    gxg <- get_gxg(db = db_fig, quiet = TRUE)
  }

  # set labels
  if (language == "NL") {
    labels <- "freatisch"
    unit <- "cm + mv"
    y_label <- "grondwaterstand"
  }
  if (language == "EN") {
    labels <- "phreatic"
    unit <- "cm + sl"
    y_label <- "groundwater level"
  }

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = "GWL", labels = labels, ordered = TRUE)

  # create basic plot
  P <- ggplot(data = db_fig) +
    geom_line(aes_string(x = "date", y = "value", colour = "level"), na.rm = TRUE, alpha = 0.8) +
    scale_colour_manual(name = "", values = "blue") +
    scale_x_date(limits = c(tstart, tend)) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # add groundwater characteristics
  if (!all(is.na(gxg))) {
    P <- P +
      geom_hline(aes(yintercept = gxg[1]), colour = "darkblue", alpha = 0.8) +
      geom_text(aes(x = tend, y = gxg[1], label = "GHG"), colour = "darkblue", vjust = -0.4, hjust = "left", size = 3) +
      geom_hline(aes(yintercept = gxg[2]), colour = "red", alpha = 0.8) +
      geom_text(aes(x = tend, y = gxg[2], label = "GLG"), colour = "red", vjust = -0.4, hjust = "left", size = 3)
  }

  # add observations
  if (observations) {
    P <- P +
      geom_point(aes_string(x = "date", y = "observed"), colour = "red", na.rm = TRUE, alpha = 0.8)

    # add performance
    if (!is.null(performance)) {
      P <- P +
        geom_text(aes(x = tstart, y = min(c(value, observed, ylim), na.rm = TRUE), label = text), hjust = -0.2, vjust = -2, colour = "grey45", fontface = "italic")
    }
  }

  # set return of procedure
  output <- list()
  output$plot <- P
  if (observations & !is.null(performance)) output$modelperformance <- modelperformance
  if (!all(is.na(gxg))) output$gxg <- tibble(ghg = signif(x = gxg[1], digits = 2), glg = signif(x = gxg[2], digits = 2))

  # ---- return of procedure ----

  return(output)
}

#' Create timeseries-plot of potential and actual crop development
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_csv
#' @importFrom fs path_package file_exists
#' @importFrom stringr str_c str_to_lower
#' @importFrom lubridate as_date NA_Date_
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate filter select rename all_of left_join rename_all
#' @importFrom ggplot2 ggplot aes aes_string geom_line geom_point geom_text scale_colour_manual scale_x_date coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_cropdev
#' @description Creates timeseries-plot of simulated potential and actual crop development
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) a timeseries-plot is created of the potential and actual crop development.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' In case observations are available it is possible to add the observations as red dots in the plot.
#' A path to a folder with observations should be specified with \code{dir_obs}.
#' In this folder a csv-file with the name of the SWAP-variable should be present.
#' The file itself should have two columns: 'DATETIME' and 'VALUE'. The unit of the values should be equal to SWAP.
#' Optionally the model performance (\code{performance}) can be added in the plot (see also: \link{get_modelperformance}).
#'
#' By default the crop development of aboveground biomass is plotted for all crops in the crop-calendar during the simulation period.
#' Specify a list of crops with \code{cropfil} in case only the development of specific crop(s) is preferred.
#' In case the development of a certain part of the plant is preferred specify \code{growth} (options: aboveground biomass ('AGB'), storage organs ('SO'), leaves ('LV'), stems ('ST'), roots ('RT'), Leaf Area Index ('LAI') or rootzone ('RD').
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
#' @examples
#' # extract example
#' dir_out <- tempfile(pattern = "swap_")
#' file_swp <- paste0(dir_out, "/swap.swp")
#' file_csv <- paste0(dir_out, "/result_output.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, exdir = dir_out)
#'
#' # plot timeseries of crop development
#' plot_cropdev(file_swp = file_swp, growth = "AGB")
#'
#' # plot timeseries of crop development including observations and model performance
#' plot_cropdev(file_swp = file_swp, growth = "AGB", dir_obs = paste0(dir_out, "/observed"), performance = "rmse")
#'
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
plot_cropdev <- function(file_swp, ...) {

  CROPFIL <- CROPTYPE <- CPWDM <- CPWSO <- PWLV<- PWST <- PWRT <- CWDM <- CWSO <- WLV <- WST <- WRT <- NULL
  LAIPOT <- RDPOT <- LAI <- RD <- NULL
  datetime <- value <- observed <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "growth", "ylim", "cropfil", "language", "dir_obs", "performance", "digits", "error", "quiet")
  tstart <- tend <- growth <- ylim <- cropfil <- language <- dir_obs <- performance <- digits <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
  if (is.null(digits)) digits <- 3
  if (is.null(growth)) growth <- "AGB"
  if (is.null(language)) language <- "EN"
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  ymin_default <- 0

  # ---- main part of procedure ----

  # check user-specified input
  if (!growth %in% c("AGB", "SO", "LV", "ST", "RT", "LAI", "RD")) stop("unknown growth type selected; options are 'AGB' (aboveground), 'SO' (organs), 'LV' (leaves), 'ST' (stems), 'RT' (roots), 'LAI' (Leaf Area Index) or 'RD' (rootzone)")
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # check if ymin is set
  if (is.na(ylim[1]) & growth != "RD") ylim[1] <- ymin_default
  if (is.na(ylim[2]) & growth == "RD") ylim[2] <- ymin_default

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND", "SWCROP", str_c("CROPROTATION::", c("CROPSTART", "CROPEND", "CROPFIL", "CROPTYPE"))), case_sensitive = FALSE, quiet = quiet)
  settings$SWCROP <- ifelse(is.null(settings$SWCROP), 1, settings$SWCROP)
  if (settings$SWCROP == 0) {
    error_message <- "no cropgrowth activated; use SWCROP = 1"
    if (error) stop(error_message) else warning(error_message); return()
  }
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract
  if (growth == "AGB") variable <- c("CPWDM", "CWDM")
  if (growth == "SO") variable <- c("CPWSO", "CWSO")
  if (growth == "LV") variable <- c("PWLV", "WLV")
  if (growth == "ST") variable <- c("PWST", "WST")
  if (growth == "RT") variable <- c("PWRT", "WRT")
  if (growth == "LAI") variable <- c("LAIPOT", "LAI")
  if (growth == "RD") variable <- c("RDPOT", "RD")
  if (!all(variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # filter crop
  rotation <- tibble(CROPSTART = settings$CROPSTART, CROPEND = settings$CROPEND, CROPFIL = settings$CROPFIL, CROPTYPE = settings$CROPTYPE) %>%
    filter(CROPTYPE == 2)
  if (nrow(rotation) == 0) {
    error_message <- "no WOFOST crop activated; check croprotation"
    if (error) stop(error_message) else warning(error_message); return()
  }
  if (!is.null(cropfil)) {
    rotation <- rotation %>%
      filter(CROPFIL %in% all_of(cropfil))
  }
  if (nrow(rotation) == 0) {
    error_message <- "selected crop not found; check croprotation"
    if (error) stop(error_message) else warning(error_message); return()
  }
  date_crop <- NA_Date_
  for (rec in 1:nrow(rotation)) date_crop <- c(date_crop, seq.Date(from = rotation$CROPSTART[rec], to = rotation$CROPEND[rec], by = 1))

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = variable) %>%
    mutate(date = as_date(datetime)) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, all_of(variable))

  # rename column
  if (growth == "AGB") {
    db_swp <- db_swp %>%
      rename(potential = CPWDM, actual = CWDM)
  }
  if (growth == "SO") {
    db_swp <- db_swp %>%
      rename(potential = CPWSO, actual = CWSO)
  }
  if (growth == "LV") {
    db_swp <- db_swp %>%
      rename(potential = PWLV, actual = WLV)
  }
  if (growth == "ST") {
    db_swp <- db_swp %>%
      rename(potential = PWST, actual = WST)
  }
  if (growth == "RT") {
    db_swp <- db_swp %>%
      rename(potential = PWRT, actual = WRT)
  }
  if (growth == "LAI") {
    db_swp <- db_swp %>%
      rename(potential = LAIPOT, actual = LAI)
  }
  if (growth == "RD") {
    db_swp <- db_swp %>%
      rename(potential = RDPOT, actual = RD)
  }

  # add observations
  variable_obs <- ifelse(growth == "AGB", "CWDM", ifelse(growth == "SO", "CWSO", ifelse(growth == "LV", "WLV", ifelse(growth == "ST", "WST", ifelse(growth == "RT", "WRT", ifelse(growth == "LAI", "LAI", "RD"))))))
  db_swp <- add_observed(db = db_swp, dir_obs = dir_obs, variable = variable_obs)
  observations <- !all(is.na(db_swp$observed))

  # set model performance (if possible)
  if (observations & !is.null(performance)) {
    modelperformance <- get_modelperformance(actual = db_swp$observed, predicted = db_swp$actual, performance = performance, digits = digits)
    text <- str_c(names(modelperformance), ": ", modelperformance, collapse = "  ")
  }

  # rearrange data
  db_fig <- NULL
  levels <- c("potential", "actual")
  for (level in levels) {
    db_tmp <- db_swp %>%
      rename(value = all_of(level)) %>%
      mutate(level = all_of(level)) %>%
      select(date, level, value, observed)
    if (level == "potential") db_tmp$observed <- NA_real_
    db_fig <- rbind(db_fig, db_tmp)
  }

  # reset value in case rootzone
  if (growth == "RD") db_fig <- db_fig %>% mutate(value = -value, observed = -observed)

  # filter crop
  db_fig <- db_fig %>%
    mutate(value = if_else(date %in% date_crop, value, NA_real_))

  # set labels
  if (language == "NL") {
    name <- "ontwikkeling"
    labels <- c("potentieel", "actueel")
    y_label <- ifelse(growth == "AGB", "bovengrondse biomassa", ifelse(growth == "SO", "gewicht vrucht", ifelse(growth == "LV", "gewicht bladeren", ifelse(growth == "ST", "gewicht stengels", ifelse(growth == "RT", "gewicht wortels", ifelse(growth == "LAI", "Leaf Area Index", "wortelzone"))))))
  }
  if (language == "EN") {
    name <- "growth"
    labels <- c("potential", "actual")
    y_label <- ifelse(growth == "AGB", "aboveground biomass", ifelse(growth == "SO", "weight storage organs", ifelse(growth == "LV", "weight leaves", ifelse(growth == "ST", "weight stems", ifelse(growth == "RT", "weight roots", ifelse(growth == "LAI", "Leaf Area Index", "rootzone"))))))
  }
  unit <- ifelse(growth == "LAI", "cm2 cm-2", ifelse(growth == "RD", "cm", "kgds ha-1"))

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = levels, labels = labels, ordered = TRUE)

  # create basic plot
  P <- ggplot(data = db_fig) +
    geom_line(aes_string(x = "date", y = "value", colour = "level"), na.rm = TRUE) +
    scale_colour_manual(name = create_label(label = name), values = c("green", "black")) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # add observations
  if (observations) {
    P <- P +
      geom_point(aes_string(x = "date", y = "observed"), colour = "red", na.rm = TRUE, alpha = 0.8)

    # add performance
    if (!is.null(performance)) {
      P <- P +
        geom_text(aes(x = tstart, y = ylim[1], label = text), hjust = -0.2, vjust = -2, colour = "grey45", fontface = "italic")
    }
  }

  # set return of procedure
  output <- list()
  output$plot <- P
  if (observations & !is.null(performance)) output$modelperformance <- modelperformance

  # ---- return of procedure ----

  return(output)
}

#' Create timeseries-plot of potential and actual yield
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c
#' @importFrom lubridate as_date NA_Date_
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate filter select rename all_of left_join rename_all
#' @importFrom ggplot2 ggplot aes_string geom_col scale_fill_manual scale_x_date coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_yield
#' @description Creates timeseries-plot of simulated potential and actual crop yield
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) a timeseries-plot is created of the potential and actual crop yield.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' By default the crop development of aboveground biomass is plotted for all crops (simulated by WOFOST; CROPTYPE = 2) in the crop-calendar during the simulation period.
#' Specify a list of crops with \code{cropfil} in case only the development of specific crop(s) is preferred.
#' In case the development of a certain part of the plant is preferred specify \code{growth} (options: aboveground biomass ('AGB') and storage organs ('SO').
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
#' @examples
#' # extract example
#' dir_out <- tempfile(pattern = "swap_")
#' file_swp <- paste0(dir_out, "/swap.swp")
#' file_csv <- paste0(dir_out, "/result_output.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, exdir = dir_out)
#'
#' # plot timeseries of crop development
#' plot_yield(file_swp = file_swp, growth = "AGB")
#'
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
plot_yield <- function(file_swp, ...) {

  CROPFIL <- CROPTYPE <- CPWDM <- CPWSO <- CWDM <- CWSO <- NULL
  datetime <- value <- actual <- potential <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "growth", "ylim", "cropfil", "language", "error", "quiet")
  tstart <- tend <- growth <- ylim <- cropfil <- language <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(0,NA_real_)
  if (is.null(growth)) growth <- "AGB"
  if (is.null(language)) language <- "EN"
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  ymin_default <- 0

  # ---- main part of procedure ----

  # check user-specified input
  if (!growth %in% c("AGB", "SO")) stop("unknown growth type selected; options are 'AGB' (aboveground) or 'SO' (organs)")
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # check if ymin is set
  if (is.na(ylim[1])) ylim[1] <- ymin_default

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND", str_c("CROPROTATION::", c("CROPSTART", "CROPEND", "CROPFIL", "CROPTYPE"))), case_sensitive = FALSE, quiet = quiet)
  settings$SWCROP <- ifelse(is.null(settings$SWCROP), 1, settings$SWCROP)
  if (settings$SWCROP == 0) {
    error_message <- "no cropgrowth activated; use SWCROP = 1"
    if (error) stop(error_message) else warning(error_message); return()
  }
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract
  if (growth == "AGB") variable <- c("CPWDM", "CWDM")
  if (growth == "SO") variable <- c("CPWSO", "CWSO")
  if (!all(variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # filter crop
  rotation <- tibble(CROPSTART = settings$CROPSTART, CROPEND = settings$CROPEND, CROPFIL = settings$CROPFIL, CROPTYPE = settings$CROPTYPE) %>%
    filter(CROPTYPE == 2)
  if (nrow(rotation) == 0) {
    error_message <- "no WOFOST crop activated; check croprotation"
    if (error) stop(error_message) else warning(error_message); return()
  }
  if (!is.null(cropfil)) {
    rotation <- rotation %>%
      filter(CROPFIL %in% all_of(cropfil))
  }
  if (nrow(rotation) == 0) {
    error_message <- "selected crop not found; check croprotation"
    if (error) stop(error_message) else warning(error_message); return()
  }
  date_crop <- NA_Date_
  for (rec in 1:nrow(rotation)) date_crop <- c(date_crop, rotation$CROPEND[rec])

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = variable) %>%
    mutate(
      date = as_date(datetime),
      year = year(date)
    ) %>%
    filter(date >= tstart & date <= tend) %>%
    filter(date %in% date_crop) %>%
    select(year, all_of(variable))

  # rename column
  if (growth == "AGB") {
    db_swp <- db_swp %>%
      rename(potential = CPWDM, actual = CWDM) %>%
      mutate(potential = potential - actual)
  }
  if (growth == "SO") {
    db_swp <- db_swp %>%
      rename(potential = CPWSO, actual = CWSO) %>%
      mutate(potential = potential - actual)
  }

  # rearrange data
  db_fig <- NULL
  levels <- c("potential", "actual")
  for (level in levels) {
    db_tmp <- db_swp %>%
      rename(value = all_of(level)) %>%
      mutate(level = all_of(level)) %>%
      select(year, level, value)
    db_fig <- rbind(db_fig, db_tmp)
  }

  # set labels
  if (language == "NL") {
    name <- "ontwikkeling"
    labels <- c("potentieel", "actueel")
    y_label <- ifelse(growth == "AGB", "oogst bovengrondse biomassa", "oogst vrucht")
  }
  if (language == "EN") {
    name <- "growth"
    labels <- c("potential", "actual")
    y_label <- ifelse(growth == "AGB", "yield aboveground biomass", "yield storage organs")
  }

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = levels, labels = labels, ordered = TRUE)
  db_fig$year <- factor(x = db_fig$year, levels = sort(unique(db_fig$year)), ordered = TRUE)

  # create graph
  P = ggplot(data = db_fig) +
    geom_col(aes_string(x = "year", y = "value", fill = "level"), position = "stack", na.rm = TRUE, alpha = 0.8) +
    scale_fill_manual(name = create_label(label = name), values = c("darkgreen", "green")) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = "kgds ha-1")) +
    get_my_theme("figure")

  # set return of procedure
  output <- list()
  output$plot <- P

  # ---- return of procedure ----

  return(output)
}

#' Create timeseries-plot of potential and actual crop development of grassland
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_csv
#' @importFrom fs path_package file_exists
#' @importFrom stringr str_c str_to_lower
#' @importFrom lubridate as_date NA_Date_
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate filter select rename all_of left_join rename_all
#' @importFrom ggplot2 ggplot aes aes_string geom_line geom_point geom_text scale_colour_manual scale_x_date coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_grass
#' @description Creates timeseries-plot of simulated potential and actual crop development of grassland
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) a timeseries-plot is created of the potential and actual crop development of grassland.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' In case observations are available it is possible to add the observations as red dots in the plot.
#' A path to a folder with observations should be specified with \code{dir_obs}.
#' In this folder a csv-file with the name of the SWAP-variable should be present.
#' The file itself should have two columns: 'DATETIME' and 'VALUE'. The unit of the values should be equal to SWAP.
#' Optionally the model performance (\code{performance}) can be added in the plot (see also: \link{get_modelperformance}).
#'
#' By default the crop development of aboveground biomass (including harvest by mowing and grazing) is plotted for grassland (simulated by GRASS; CROPTYPE = 3) in the crop-calendar during the simulation period.
#' Specify a list of crops with \code{cropfil} in case only the development of specific crop(s) is preferred.
#' In case the development of a certain part of the plant is preferred specify \code{growth} (options: aboveground biomass and harvest ('AGB+'), aboveground biomass ('AGB'), roots ('RT'), Leaf Area Index ('LAI') or rootzone ('RD').
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
plot_grass <- function(file_swp, ...) {

  CROPFIL <- CROPTYPE <- PGRASSDM <- GRASSDM <- PMOWDM <- MOWDM <- PGRAZDM <- GRAZDM <- PWRT <- WRT <- NULL
  LAIPOT <- RDPOT <- LAI <- RD <- NULL
  datetime <- value <- observed <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "growth", "ylim", "cropfil", "mowrest", "language", "dir_obs", "performance", "digits", "error", "quiet")
  tstart <- tend <- growth <- ylim <- cropfil <- mowrest <- language <- dir_obs <- performance <- digits <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
  if (is.null(mowrest)) mowrest <- 700.0
  if (is.null(digits)) digits <- 3
  if (is.null(growth)) growth <- "AGB+"
  if (is.null(language)) language <- "EN"
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  ymin_default <- 0

  # ---- main part of procedure ----

  # check user-specified input
  if (!growth %in% c("AGB+", "AGB", "RT", "LAI", "RD")) stop("unknown growth type selected; options are 'AGB+' (aboveground and harvest), 'AGB' (aboveground), 'RT' (roots), 'LAI' (Leaf Area Index) or 'RD' (rootzone)")
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # check if ymin is set
  if (is.na(ylim[1]) & growth != "RD") ylim[1] <- ymin_default
  if (is.na(ylim[2]) & growth == "RD") ylim[2] <- ymin_default

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND", "SWCROP", str_c("CROPROTATION::", c("CROPSTART", "CROPEND", "CROPFIL", "CROPTYPE"))), case_sensitive = FALSE, quiet = quiet)
  settings$SWCROP <- ifelse(is.null(settings$SWCROP), 1, settings$SWCROP)
  if (settings$SWCROP == 0) {
    error_message <- "no cropgrowth activated; use SWCROP = 1"
    if (error) stop(error_message) else warning(error_message); return()
  }
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract
  if (growth == "AGB+") variable <- c("PGRASSDM", "GRASSDM", "PMOWDM", "MOWDM", "PGRAZDM", "GRAZDM")
  if (growth == "AGB") variable <- c("PGRASSDM", "GRASSDM")
  if (growth == "RT") variable <- c("PWRT", "WRT")
  if (growth == "LAI") variable <- c("LAIPOT", "LAI")
  if (growth == "RD") variable <- c("RDPOT", "RD")
  if (!all(variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # filter crop
  rotation <- tibble(CROPSTART = settings$CROPSTART, CROPEND = settings$CROPEND, CROPFIL = settings$CROPFIL, CROPTYPE = settings$CROPTYPE) %>%
    filter(CROPTYPE == 3)
  if (nrow(rotation) == 0) {
    error_message <- "no GRASS crop activated; check croprotation"
    if (error) stop(error_message) else warning(error_message); return()
  }
  if (!is.null(cropfil)) {
    rotation <- rotation %>%
      filter(CROPFIL %in% all_of(cropfil))
  }
  if (nrow(rotation) == 0) {
    error_message <- "selected crop not found; check croprotation"
    if (error) stop(error_message) else warning(error_message); return()
  }
  date_crop <- NA_Date_
  for (rec in 1:nrow(rotation)) date_crop <- c(date_crop, seq.Date(from = rotation$CROPSTART[rec], to = rotation$CROPEND[rec], by = 1))

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = variable) %>%
    mutate(date = as_date(datetime)) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, all_of(variable))

  # rename column
  if (growth == "AGB+") {
    db_swp <- db_swp %>%
      mutate(
        potential = pmax(0.0, PGRASSDM - mowrest) + PMOWDM + PGRAZDM,
        actual = pmax(0.0, GRASSDM - mowrest) + MOWDM + GRAZDM
      )
  }
  if (growth == "AGB") {
    db_swp <- db_swp %>%
      mutate(
        potential = pmax(0.0, PGRASSDM - mowrest),
        actual = pmax(0.0, GRASSDM - mowrest)
      )
  }
  if (growth == "RT") {
    db_swp <- db_swp %>%
      rename(potential = PWRT, actual = WRT)
  }
  if (growth == "LAI") {
    db_swp <- db_swp %>%
      rename(potential = LAIPOT, actual = LAI)
  }
  if (growth == "RD") {
    db_swp <- db_swp %>%
      rename(potential = RDPOT, actual = RD)
  }

  # add observations
  variable_obs <- ifelse(growth == "AGB+", "GRASSDM+", ifelse(growth == "AGB", "GRASSDM", ifelse(growth == "RT", "WRT", ifelse(growth == "LAI", "LAI", "RD"))))
  db_swp <- add_observed(db = db_swp, dir_obs = dir_obs, variable = variable_obs)
  observations <- !all(is.na(db_swp$observed))

  # set model performance (if possible)
  if (observations & !is.null(performance)) {
    modelperformance <- get_modelperformance(actual = db_swp$observed, predicted = db_swp$actual, performance = performance, digits = digits)
    text <- str_c(names(modelperformance), ": ", modelperformance, collapse = "  ")
  }

  # rearrange data
  db_fig <- NULL
  levels <- c("potential", "actual")
  for (level in levels) {
    db_tmp <- db_swp %>%
      rename(value = all_of(level)) %>%
      mutate(level = all_of(level)) %>%
      select(date, level, value, observed)
    if (level == "potential") db_tmp$observed <- NA_real_
    db_fig <- rbind(db_fig, db_tmp)
  }

  # reset value in case rootzone
  if (growth == "RD") db_fig <- db_fig %>% mutate(value = -value, observed = -observed)

  # filter crop
  db_fig <- db_fig %>%
    mutate(value = if_else(date %in% date_crop, value, NA_real_))

  # set labels
  if (language == "NL") {
    name <- "ontwikkeling"
    labels <- c("potentieel", "actueel")
    y_label <- ifelse(growth == "AGB+", "bovengrondse biomassa", ifelse(growth == "AGB", "bovengrondse biomassa", ifelse(growth == "RT", "gewicht wortels", ifelse(growth == "LAI", "Leaf Area Index", "wortelzone"))))
  }
  if (language == "EN") {
    name <- "growth"
    labels <- c("potential", "actual")
    y_label <- ifelse(growth == "AGB+", "aboveground biomass", ifelse(growth == "AGB", "aboveground biomass", ifelse(growth == "RT", "weight roots", ifelse(growth == "LAI", "Leaf Area Index", "rootzone"))))
  }
  unit <- ifelse(growth == "LAI", "cm2 cm-2", ifelse(growth == "RD", "cm", "kgds ha-1"))

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = levels, labels = labels, ordered = TRUE)

  # create basic plot
  P <- ggplot(data = db_fig) +
    geom_line(aes_string(x = "date", y = "value", colour = "level"), na.rm = TRUE) +
    scale_colour_manual(name = create_label(label = name), values = c("green", "black")) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # add observations
  if (observations) {
    P <- P +
      geom_point(aes_string(x = "date", y = "observed"), colour = "red", na.rm = TRUE, alpha = 0.8)

    # add performance
    if (!is.null(performance)) {
      P <- P +
        geom_text(aes(x = tstart, y = ylim[1], label = text), hjust = -0.2, vjust = -2, colour = "grey45", fontface = "italic")
    }
  }

  # set return of procedure
  output <- list()
  output$plot <- P
  if (observations & !is.null(performance)) output$modelperformance <- modelperformance

  # ---- return of procedure ----

  return(output)
}

#' Create water balance plot for total soil column
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds
#' @importFrom fs path_package
#' @importFrom stringr str_c str_replace
#' @importFrom lubridate as_date
#' @importFrom tibble add_column
#' @importFrom dplyr %>% mutate filter select rename all_of left_join rename_all
#' @importFrom ggplot2 ggplot aes_string geom_col geom_hline scale_fill_manual coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_watbal
#' @description Creates water balance plot for a given part of the soil-profile.
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) and SWAP variable (\code{variable}) a water balance plot is created for the total soil column.
#' The water balance will be created with net fluxes.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' Optionally the terms of the water balance can be aggregated in time by \code{aggregate} (options: "year", "month", "decade").
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
#' @examples
#' # extract example
#' dir_out <- tempfile(pattern = "swap_")
#' file_swp <- paste0(dir_out, "/swap.swp")
#' file_csv <- paste0(dir_out, "/result_output.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, files = basename(c(file_swp, file_csv)), exdir = dir_out)
#'
#' # plot (netto) waterbalance of the total soil column
#' plot_watbal(file_swp = file_swp, aggregate = "year")
#'
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
plot_watbal <- function(file_swp, ...) {

  DRAINAGE <- DSTOR <- EACT <- INTERC <- RUNOFF <- SUBLIM <- TACT <- NULL
  datetime <- value <- . <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "ylim", "aggregate", "language", "output_swap", "error", "quiet")
  tstart <- tend <- ylim <- aggregate <- language <- output_swap <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
  if (is.null(language)) language <- "EN"
  if (is.null(output_swap)) output_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_output.rds"))
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # ---- main part of procedure ----

  # check user-specified input
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # extract plot settings
  variable <- "WATBAL_MIN"
  plot_settings <- get_plot_settings(variable = variable, output_swap = output_swap)

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND"), case_sensitive = FALSE, quiet = quiet)
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract (and check if avaialable)
  extract_variable <- plot_settings$variable[!plot_settings$variable %in% plot_settings$variable_optional]
  if (!all(extract_variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(extract_variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }
  extract_variable <- plot_settings$variable
  extract_variable <- extract_variable[extract_variable %in% inlist]

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = extract_variable) %>%
    rename_all(.funs = str_replace, pattern = str_c("\\[", plot_settings$depth, "\\]"), replacement = "") %>%
    mutate(date = as_date(datetime)) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, all_of(extract_variable))

  # add default values if not exists
  default_column <- c(SNOW = 0.0, SUBLIM = 0.0, INTERC = 0.0, TACT = 0.0, DRAINAGE = 0.0, RUNON = 0.0, QBOTTOM = 0.0, QSSDI = 0.0)
  db_swp <- db_swp %>% add_column(!!!default_column[!names(default_column) %in% names(.)])

  # set sign of waterbalance terms
  db_swp <- db_swp %>%
    mutate(
      INTERC = -INTERC,
      RUNOFF = -RUNOFF,
      EACT = -EACT,
      TACT = -TACT,
      SUBLIM = -SUBLIM,
      DRAINAGE = -DRAINAGE,
      DSTOR = -DSTOR
    )

  # aggregate data (if needed)
  if (!is.null(aggregate)) db_swp <- summarise_db(db = db_swp, level = aggregate, column = extract_variable)

  # rearrange data
  db_fig <- NULL
  levels <- extract_variable
  for (level in levels) {
    db_tmp <- db_swp %>%
      rename(value = all_of(level)) %>%
      mutate(level = all_of(level)) %>%
      select(date, level, value)
    db_fig <- rbind(db_fig, db_tmp)
  }

  # convert unit
  if (!is.null(plot_settings$factor)) {
    db_fig <- db_fig %>%
      mutate(value = value * plot_settings$factor)
  }

  # set labels
  unit <- plot_settings$unit
  if (language == "NL") {
    if (is.null(unit)) unit <- plot_settings$unit_NL
    if (!is.null(aggregate)) unit_time <- ifelse(aggregate == "year", "jaar", ifelse(aggregate == "month", "maand", "decade"))
    labels <- plot_settings$labels_NL
    y_label <- plot_settings$y_label_NL
  }
  if (language == "EN") {
    if (is.null(unit)) unit <- plot_settings$unit_EN
    if (!is.null(aggregate)) unit_time <- aggregate
    labels <- plot_settings$labels_EN
    y_label <- plot_settings$y_label_EN
  }
  labels <- labels[plot_settings$variable %in% levels]
  values <- plot_settings$colours[plot_settings$variable %in% levels]
  unit <- ifelse(is.null(aggregate), str_c(unit, " d-1"), str_c(unit, " ", unit_time, "-1"))

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = levels, labels = labels, ordered = TRUE)

  # create ggplot
  P <- ggplot(data = db_fig) +
    geom_col(aes_string(x = "date", y = "value", fill = "level"), na.rm = TRUE, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = 3) +
    scale_fill_manual(name = "", values = values) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # set return of procedure
  output <- list()
  output$plot <- P
  output$data <- db_swp

  # ---- return of procedure ----

  return(output)
}

#' Create water balance plot for a subregion of the soil column
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param variable character string of SWAP variable; options are 'SUBREG_MIN[\emph{range}]' or 'SUBREG_ALL[\emph{range}]'.
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds
#' @importFrom fs path_package
#' @importFrom stringr str_c str_replace
#' @importFrom lubridate as_date
#' @importFrom tibble deframe add_column
#' @importFrom dplyr %>% mutate filter select rename all_of left_join rename_all if_else group_by summarise slice_max
#' @importFrom ggplot2 ggplot aes_string geom_col geom_text geom_hline scale_fill_manual coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_subreg
#' @description Creates water balance plot for a subregion of the soil column.
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) and SWAP variable (\code{variable}) a water balance plot is created for a subregion of the soil column.
#' The water balance can be created with net fluxes using variable 'SUBREG_MIN[\emph{range}]', or with all fluxes using variable 'SUBREG_ALL[\emph{range}]'.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' Optionally the terms of the water balance can be aggregated in time by \code{aggregate} (options: "year", "month", "decade").
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
#' @examples
#' # extract example
#' dir_out <- tempfile(pattern = "swap_")
#' file_swp <- paste0(dir_out, "/swap.swp")
#' file_csv <- paste0(dir_out, "/result_output.csv")
#' zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
#' unzip(zipfile = zipfile, files = basename(c(file_swp, file_csv)), exdir = dir_out)
#'
#' # plot (netto) waterbalance of the first 40.0 cm from the top of the soil column
#' plot_subreg(file_swp = file_swp, variable = "SUBREG_MIN[0.0:-40.0]")
#'
#' # plot (netto) waterbalance of the first 40.0 cm from the top of the soil column aggregated to monthly values
#' plot_subreg(file_swp = file_swp, variable = "SUBREG_MIN[0.0:-40.0]", aggregate = "month")
#'
#' # plot waterbalance of the first 40.0 cm from the top of the soil column aggregated to monthly values
#' plot_subreg(file_swp = file_swp, variable = "SUBREG_ALL[0.0:-40.0]", aggregate = "month")
#'
#' # clean example
#' unlink(x = dir_out, recursive = TRUE)
plot_subreg <- function(file_swp, variable, ...) {

  QTOP <- QTOPIN <- QTOPOUT <- QBOT <- QBOTIN <- QBOTOUT <- QTRANS <- QDRA <- QDRAININ <- QDRAINOUT <- QSTOR <- WTOT <- DSTOR <- NULL
  datetime <- value <- . <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "ylim", "aggregate", "language", "output_swap", "error", "quiet")
  tstart <- tend <- ylim <- aggregate <- language <- output_swap <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
  if (is.null(language)) language <- "EN"
  if (is.null(output_swap)) output_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_output.rds"))
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # ---- main part of procedure ----

  # check user-specified input
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")
  if(!delete_depth(variable) %in% c("SUBREG_MIN", "SUBREG_ALL")) stop("unknown variable selected; options are 'SUBREG_MIN' or 'SUBREG_ALL'")

  # extract plot settings
  plot_settings <- get_plot_settings(variable = variable, output_swap = output_swap)

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND"), case_sensitive = FALSE, quiet = quiet)
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract (and check if avaialable)
  extract_variable <- plot_settings$variable[!plot_settings$variable %in% plot_settings$variable_optional]
  extract_variable <- str_replace(string = extract_variable, pattern = "\\[depth\\]", replacement = str_c("\\[", plot_settings$depth, "\\]"))
  if (!all(extract_variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(extract_variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }
  extract_variable <- str_replace(string = plot_settings$variable, pattern = "\\[depth\\]", replacement = str_c("\\[", plot_settings$depth, "\\]"))
  extract_variable <- extract_variable[extract_variable %in% inlist]

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = extract_variable) %>%
    rename_all(.funs = str_replace, pattern = str_c("\\[", plot_settings$depth, "\\]"), replacement = "") %>%
    mutate(date = as_date(datetime)) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, all_of(delete_depth(extract_variable)))

  # set sign of waterbalance terms and set change of storage
  if (delete_depth(variable) == "SUBREG_MIN") {

    # add default values if not exists
    default_column <- c(QTRANS = 0.0, QDRA = 0.0, QBOT = 0.0)
    db_swp <- db_swp %>% add_column(!!!default_column[!names(default_column) %in% names(.)])

    db_swp <- db_swp %>%
      mutate(
        QTRANS = -QTRANS,
        QBOT = -QBOT,
        QDRA = -QDRA,
        DSTOR = WTOT - lag(WTOT)
      ) %>%
      mutate(
        DSTOR = if_else(is.na(DSTOR), QTOP + QBOT + QTRANS + QDRA, DSTOR) * -1
      )

  } else {

    # add default values if not exists
    default_column <- c(QTRANS = 0.0, QDRAININ = 0.0, QDRAINOUT = 0.0, QBOTIN = 0.0, QBOTOUT = 0.0)
    db_swp <- db_swp %>% add_column(!!!default_column[!names(default_column) %in% names(.)])

    # set change in storage
    db_swp <- db_swp %>%
      mutate(
        QTOPOUT = -QTOPOUT,
        QBOTOUT = -QBOTOUT,
        QTRANS = -QTRANS,
        QDRAINOUT = -QDRAINOUT,
        DSTOR = WTOT - lag(WTOT)
      ) %>%
      mutate(
        DSTOR = if_else(is.na(DSTOR), QTOPIN + QTOPOUT + QBOTIN + QBOTOUT + QTRANS + QDRAININ + QDRAINOUT, DSTOR) * -1
      )
  }

  # aggregate data (if needed)
  if (!is.null(aggregate)) db_swp <- summarise_db(db = db_swp, level = aggregate, column = plot_settings$levels)

  # rearrange data
  db_fig <- NULL
  levels <- delete_depth(extract_variable)
  levels[levels == "WTOT"] <- "DSTOR"
  for (level in levels) {
    db_tmp <- db_swp %>%
      rename(value = all_of(level)) %>%
      mutate(level = all_of(level)) %>%
      select(date, level, value)
    db_fig <- rbind(db_fig, db_tmp)
  }

  # convert unit
  if (!is.null(plot_settings$factor)) {
    db_fig <- db_fig %>%
      mutate(value = value * plot_settings$factor)
  }

  # set labels
  unit <- plot_settings$unit
  if (language == "NL") {
    if (is.null(unit)) unit <- plot_settings$unit_NL
    if (!is.null(aggregate)) unit_time <- ifelse(aggregate == "year", "jaar", ifelse(aggregate == "month", "maand", "decade"))
    labels <- plot_settings$labels_NL
    y_label <- plot_settings$y_label_NL
  }
  if (language == "EN") {
    if (is.null(unit)) unit <- plot_settings$unit_EN
    if (!is.null(aggregate)) unit_time <- aggregate
    labels <- plot_settings$labels_EN
    y_label <- plot_settings$y_label_EN
  }
  labels <- labels[plot_settings$levels %in% levels]
  values <- plot_settings$colours[plot_settings$levels %in% levels]
  unit <- ifelse(is.null(aggregate), str_c(unit, " d-1"), str_c(unit, " ", unit_time, "-1"))

  # set y-max
  ymax <- db_fig %>%
    group_by(date) %>%
    summarise(
      value = sum(if_else(value >= 0.0, value, 0.0)),
      .groups = "drop"
    ) %>%
    slice_max(value) %>%
    select(value) %>%
    deframe()

  ymax <- max(c(ymax, ylim), na.rm = TRUE)
  depth <- str_c(str_replace(string = str_replace(string = plot_settings$depth, pattern = "-", replacement = ""), pattern = ":", replacement = " - "), " cm")

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = levels, labels = labels, ordered = TRUE)

  # create ggplot
  P <- ggplot(data = db_fig) +
    geom_col(aes_string(x = "date", y = "value", fill = "level"), na.rm = TRUE, alpha = 0.8) +
    geom_text(x = tstart, y = ymax, label = depth, vjust = "outward", hjust = "inward") +
    geom_hline(yintercept = 0, linetype = 3) +
    scale_fill_manual(name = "", values = values) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # set return of procedure
  output <- list()
  output$plot <- P
  output$data <- db_swp

  # ---- return of procedure ----

  return(output)
}

#' Create drainage plot
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds
#' @importFrom fs path_package
#' @importFrom stringr str_c str_replace
#' @importFrom lubridate as_date
#' @importFrom tibble add_column
#' @importFrom dplyr %>% mutate filter select rename all_of rename_all
#' @importFrom ggplot2 ggplot aes_string geom_col geom_hline scale_fill_manual coord_cartesian labs
#' @importFrom controlR get_my_theme create_label
#' @export plot_drains
#' @description Creates drainage plot for a maximum of five drainage systems.
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) a drainage plot is created.
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' Optionally the drainage fluxes can be aggregated in time by \code{aggregate} (options: "year", "month", "decade").
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
plot_drains <- function(file_swp, ...) {

  DRAINAGE_1 <- DRAINAGE_2 <- DRAINAGE_3 <- DRAINAGE_4 <- DRAINAGE_5 <- NULL
  datetime <- value <- . <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "ylim", "aggregate", "language", "output_swap", "error", "quiet")
  tstart <- tend <- ylim <- aggregate <- language <- output_swap <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
  if (is.null(language)) language <- "EN"
  if (is.null(output_swap)) output_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_output.rds"))
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # ---- main part of procedure ----

  # check user-specified input
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # extract plot settings
  variable <- "DRAINAGE_MIN"
  plot_settings <- get_plot_settings(variable = variable, output_swap = output_swap)

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "TSTART", "TEND"), case_sensitive = FALSE, quiet = quiet)
  inlist <- extract_variable_inlist(file_swp = file_swp)

  # set variables to extract (and check if avaialable)
  extract_variable <- plot_settings$variable
  extract_variable <- extract_variable[extract_variable %in% inlist]
  if (length(extract_variable) ==  0) {
    error_message <- str_c("missing variables in output '", str_c(extract_variable, collapse = "' '"), "'; check INLIST_CSV")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = extract_variable) %>%
    rename_all(.funs = str_replace, pattern = str_c("\\[", plot_settings$depth, "\\]"), replacement = "") %>%
    mutate(date = as_date(datetime)) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, all_of(delete_depth(extract_variable)))

  # add default values if not exists
  default_column <- c(DRAINAGE_1 = 0.0, DRAINAGE_2 = 0.0, DRAINAGE_3 = 0.0, DRAINAGE_4 = 0.0, DRAINAGE_5 = 0.0)
  db_swp <- db_swp %>% add_column(!!!default_column[!names(default_column) %in% names(.)])

  # aggregate data (if needed)
  if (!is.null(aggregate)) db_swp <- summarise_db(db = db_swp, level = aggregate, column = plot_settings$levels)

  # rearrange data
  db_fig <- NULL
  levels <- delete_depth(extract_variable)
  levels[levels == "WTOT"] <- "DSTOR"
  for (level in levels) {
    db_tmp <- db_swp %>%
      rename(value = all_of(level)) %>%
      mutate(level = all_of(level)) %>%
      select(date, level, value)
    db_fig <- rbind(db_fig, db_tmp)
  }

  # convert unit
  if (!is.null(plot_settings$factor)) {
    db_fig <- db_fig %>%
      mutate(value = value * plot_settings$factor)
  }

  # set labels
  unit <- plot_settings$unit
  if (language == "NL") {
    if (is.null(unit)) unit <- plot_settings$unit_NL
    if (!is.null(aggregate)) unit_time <- ifelse(aggregate == "year", "jaar", ifelse(aggregate == "month", "maand", "decade"))
    labels <- plot_settings$labels_NL
    y_label <- plot_settings$y_label_NL
  }
  if (language == "EN") {
    if (is.null(unit)) unit <- plot_settings$unit_EN
    if (!is.null(aggregate)) unit_time <- aggregate
    labels <- plot_settings$labels_EN
    y_label <- plot_settings$y_label_EN
  }
  labels <- labels[plot_settings$levels %in% levels]
  values <- plot_settings$colours[plot_settings$levels %in% levels]
  unit <- ifelse(is.null(aggregate), str_c(unit, " d-1"), str_c(unit, " ", unit_time, "-1"))

  # factors
  db_fig$level <- factor(x = db_fig$level, levels = levels, labels = labels, ordered = TRUE)

  # create ggplot
  P <- ggplot(data = db_fig) +
    geom_col(aes_string(x = "date", y = "value", fill = "level"), na.rm = TRUE, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = 3) +
    scale_fill_manual(name = "", values = values) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = unit)) +
    get_my_theme("figure")

  # set return of procedure
  output <- list()
  output$plot <- P
  output$data <- db_swp

  # ---- return of procedure ----

  return(output)
}

#' Create contour-plot with the reduction of root water uptake (uncompensated)
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom readr read_rds read_csv
#' @importFrom fs path_package
#' @importFrom stringr str_c str_to_upper str_split str_trim
#' @importFrom lubridate as_date
#' @importFrom tibble tibble
#' @importFrom dplyr %>% mutate filter select rename all_of inner_join lag
#' @importFrom ggplot2 ggplot aes_string geom_rect scale_fill_gradientn scale_x_date coord_cartesian labs
#' @importFrom pals viridis coolwarm
#' @importFrom controlR get_my_theme create_label
#' @export plot_rrwu
#' @description Creates contour-plot with the reduction of root water uptake (for uncompensated situation).
#' @details
#' Based on the main inputfile of SWAP (\code{file_swp}) and PRWU, RRWU and RDENS a contour-plot is created with the reduction of root water uptake (uncompensated situation).
#' The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
#' The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.
#'
#' The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
plot_rrwu <- function(file_swp, ...) {

  datetime <- depth <- value <- . <- hcomp <- bottom <- top <- node <- NULL
  CROPFIL <- PRWU <- RRWU <- RDENS <- rdens <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("tstart", "tend", "ylim", "cropfil", "language", "output_swap", "error", "quiet")
  tstart <- tend <- ylim <- cropfil <- language <- output_swap <- error <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(tstart)) tstart <- as_date(tstart)
  if (!is.null(tend)) tend <- as_date(tend)
  if (is.null(ylim)) ylim <- c(NA_real_, NA_real_)
  if (is.null(language)) language <- "EN"
  if (is.null(output_swap)) output_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_output.rds"))
  if (is.null(error)) error <- TRUE
  if (is.null(quiet)) quiet <- TRUE

  # ---- main part of procedure ----

  # check user-specified input
  if (!language %in% c("EN", "NL")) stop("unknown language selected; options are 'EN' (English) or 'NL' (Dutch)")

  # extract plot settings
  variable <- "STRESS"
  plot_settings <- get_plot_settings(variable = variable)

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("OUTFIL", "INLIST_CSV_TZ", "TSTART", "TEND", str_c("SOILPROFILE::", c("HSUBLAY", "NCOMP")), "SWCROP", str_c("CROPROTATION::", c("CROPSTART", "CROPEND", "CROPFIL"))), case_sensitive = FALSE, quiet = quiet)
  settings$SWCROP <- ifelse(is.null(settings$SWCROP), 1, settings$SWCROP)
  if (settings$SWCROP == 0) {
    error_message <- "no cropgrowth activated; use SWCROP = 1"
    if (error) stop(error_message) else warning(error_message); return()
  }
  if (is.null(settings$INLIST_CSV_TZ)) stop("no csv-output generated; use SW_CSV_TZ = 1")
  inlist <- str_to_upper(string = str_trim(string = str_split(string = settings$INLIST_CSV_TZ, pattern = ",", simplify = TRUE)))

  # extract discretization
  db_cmp <- NULL
  for (rec in 1:length(settings$HSUBLAY)) {
    db_tmp <- tibble(layer = rep(x = rec, times = settings$NCOMP[rec]), hcomp = rep(x = settings$HSUBLAY[rec] / settings$NCOMP[rec], times = settings$NCOMP[rec]))
    db_cmp <- rbind(db_cmp, db_tmp)
  }

  db_cmp <- db_cmp %>%
    mutate(
      node = 1:nrow(.),
      bottom = cumsum(hcomp),
      top = lag(bottom, default = 0),
      depth = as.integer((0.5 * top + 0.5 * bottom) * -10)
    ) %>%
    mutate(
      bottom = -bottom,
      top = -top
    )

  # set variables to extract
  extract_variable <- plot_settings$variable
  if (!all(extract_variable %in% inlist)) {
    error_message <- str_c("missing variables in output '", str_c(variable, collapse = "' '"), "'; check INLIST_CSV_TZ")
    if (error) stop(error_message) else warning(error_message); return()
  }

  # set period
  if (is.null(tstart)) tstart <- settings$TSTART
  if (is.null(tend)) tend <- settings$TEND
  if (min(tstart, tend) < settings$TSTART | max(tstart, tend) > settings$TEND) stop("selected period outside simulation period!")

  # filter crop
  rotation <- tibble(CROPSTART = settings$CROPSTART, CROPEND = settings$CROPEND, CROPFIL = settings$CROPFIL)
  if (!is.null(cropfil)) {
    rotation <- rotation %>%
      filter(CROPFIL %in% all_of(cropfil))
  }
  if (nrow(rotation) == 0) stop("no crop found; check croprotation")
  date_crop <- NA_Date_
  for (rec in 1:nrow(rotation)) date_crop <- c(date_crop, seq.Date(from = rotation$CROPSTART[rec], to = rotation$CROPEND[rec], by = 1))

  # load simulation results
  file_csv <- str_c(path_dir(file_swp), "/", settings$OUTFIL, "_output_tz.csv")
  db_swp <- read_csv_SWAP(file = file_csv, variable = extract_variable) %>%
    rename(rdens = RDENS) %>%
    mutate(
      date = as_date(datetime),
      depth = as.integer(depth * 10),
      value = 100 * RRWU / PRWU
    ) %>%
    filter(date >= tstart & date <= tend) %>%
    select(date, depth, rdens, value)

  # filter crop
  db_swp <- db_swp %>%
    filter(date %in% date_crop)

  # combine data
  db_fig <- inner_join(x = db_cmp, y = db_swp, by = "depth") %>%
    filter(rdens > 0) %>%
    select(-depth, -rdens) %>%
    arrange(date, node)

  # set labels
  unit <- plot_settings$unit
  if (language == "NL") {
    if (is.null(unit)) unit <- plot_settings$unit_NL
    y_label <- "diepte"
    y_unit <- "cm + mv"
    label <- plot_settings$y_label_NL
  }
  if (language == "EN") {
    if (is.null(unit)) unit <- plot_settings$unit_EN
    y_label <- "depth"
    y_unit <- "cm + sl"
    label <- plot_settings$y_label_EN
  }

  # set colours
  if (plot_settings$colours == "coolwarm") colours <- coolwarm(100)
  if (plot_settings$colours == "viridis") colours <- viridis(100)
  if (plot_settings$flip_colours) colours <- colours[100:1]


  # open device
  P <- ggplot(data = db_fig) +
    geom_rect(aes_string(xmin = "date", xmax = "date + 1", ymin = "bottom", ymax = "top", fill = "value"), na.rm = TRUE, alpha = 0.8) +
    scale_fill_gradientn(name = create_label(label = label, unit = unit, linebreak = TRUE), colours = colours) +
    scale_x_date(limits = c(tstart, tend)) +
    coord_cartesian(ylim = ylim) +
    labs(x = "", y = create_label(label = y_label, unit = y_unit)) +
    get_my_theme("figure")

  # set return of procedure
  output <- list()
  output$plot <- P

  # ---- return of procedure ----

  return(output)
}

#' Aggregate data in time
#'
#' @param db tibble to aggregate.
#' @param level level of aggregation.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_replace str_to_lower
#' @importFrom lubridate year month day
#' @importFrom tibble deframe
#' @importFrom dplyr %>% mutate select all_of left_join group_by ungroup summarise summarise_at
#' @export summarise_db
#' @keywords internal
#' @details
#' Summarise data in time set by \code{level} (options: "year", "month", "decade").
#' Optionally columns to summarise can be set by \code{column}.
summarise_db <- function(db, level, ...) {

  MM <- DD <- decade <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- "column"
  column <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(column)) {
    column <- names(db)
    column <- column[column != "date"]
  }

  # ---- main part of procedure ----

  # check user-specified input
  if (!level %in% c("year", "month", "decade")) stop("unknown aggregation level selected; options are 'year', 'month' or 'decade'")

  # set aggregate level
  if (level == "year") col_agg <- c("year")
  if (level == "month") col_agg <- c("year", "month")
  if (level == "decade") col_agg <- c("year", "decade")

  # add all levels of aggregation
  db <- db %>%
    mutate(
      year = year(date),
      month = month(date),
      day = day(date),
      decade = if_else(day <= 10, 1, if_else(day <= 20, 2, 3)) + (month - 1) * 3
    )

  # aggregate data
  db_agg <- db %>%
    select(-date) %>%
    group_by(db[, col_agg]) %>%
    summarise_at(column, sum) %>%
    ungroup

  db_per <- db %>%
    group_by(db[, col_agg]) %>%
    summarise(
      date = min(date),
      .groups = "drop"
    )

  # reset date to start of aggregation period
  if (level == "year") {
      db_per <- db_per %>%
        mutate(date = as_date(str_c(year(date), "-01-01")))
  }
  if (level == "month") {
    db_per <- db_per %>%
      mutate(
        MM = formatC(x = month(date), format = "d", width = 2, flag = "0"),
        date = as_date(str_c(year(date), "-", MM, "-01"))
      ) %>%
      select(-MM)
  }
  if (level == "decade") {
    db_per <- db_per %>%
      mutate(
        MM = month(date),
        DD = if_else(decade %in% c(1,4,7,10,13,16,19,22,25,28,31,34), "01", if_else(decade %in% c(2,5,8,11,14,17,20,23,26,29,32,35), "11", "21")),
        date = as_date(str_c(year(date), "-", MM, "-", DD))
      ) %>%
      select(-MM, -DD)
  }

  db <- left_join(x = db_per, y = db_agg, by = col_agg) %>%
    select(-all_of(col_agg))

  # ---- return of procedure ----

  return(db)

}

#' Extract output variables of SWAP-model
#'
#' @param file_swp character string of main SWAP inputfile (swp-file).
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_trim str_split str_locate_all str_sub str_subset str_detect str_to_upper
#' @keywords internal
#' @export extract_variable_inlist
extract_variable_inlist <- function(file_swp, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- "quiet"
  quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(quiet)) quiet <- TRUE

  # available shortlist in SWAP
  watbal <- c(
    "RAIN", "SNOW", "INTERC", "RUNON", "RUNOFF", "IRRIG", "TPOT", "TACT", "EPOT", "EACT", "SUBLIM",
    "QSSDI", "QBOTTOM", "DRAINAGE", "VOLACT","DSTOR", "BALDEV")
  solbal <- c("SQPREC", "SQIRRIG", "SQBOT", "DECTOT", "ROTTOT", "SQDRA", "SAMPRO")
  etterms <- c("INTERC", "TPOT", "TACT", "TREDDRY", "TREDWET", "TREDSOL", "TREDFRS", "EPOT", "EACT")
  crop <- c(
    "TSUM", "DVS", "PGASSPOT", "PGASS", "CPWDM", "CWDM", "CPWSO", "CWSO", "PWLV", "WLV", "PWST", "WST", "PWRT", "WRT",
    "DWSO", "DWLV", "DWLVPOT", "DWST", "DWSTPOT", "DWRT", "DWRTPOT", "HEIGHT", "CRPFAC", "LAIPOT", "LAI", "RDPOT", "RD", "PP", "PL")
  grass <- c(
    "TSUM", "PGRASSDM", "GRASSDM", "PMOWDM", "MOWDM", "PGRAZDM", "GRAZDM", "PLOSSDM", "LOSSDM",
    "HEIGHT", "CRPFAC", "LAIPOT", "LAI", "RDPOT", "RD", "PP", "PL")
  subreg_min <- c("QTRANS", "QTOP", "QBOT", "QDRA", "WTOT")
  subreg_all <- c("QTRANS", "QTOPIN", "QTOPOUT", "QBOTIN", "QBOTOUT", "QDRAININ", "QDRAINOUT", "WTOT")

  # ---- main part of procedure ----

  # load simulation settings
  settings <- get_settings_SWAP(file = file_swp, variable = c("INLIST_CSV", "SWSNOW", "SWFROST", "SWSOLU", "SWDRA", "SWBOTB", "SWRUNON", "SWSSDI", "SWCROP"), case_sensitive = FALSE, quiet = quiet)
  settings$SWCROP <- ifelse(is.null(settings$SWCROP), 1, settings$SWCROP)
  settings$SWSSDI <- ifelse(is.null(settings$SWSSDI), 0, settings$SWSSDI)
  if (is.null(settings$INLIST_CSV)) stop("no csv-output generated; use SW_CSV = 1")

  # set uppercase
  inlist <- str_to_upper(string = settings$INLIST_CSV)

  # extract single variables
  inlist <- str_trim(string = str_split(string = inlist, pattern = ",", simplify = TRUE)[1,])

  # replace multiple depth by single depth
  for (string in str_subset(string = inlist, pattern = ";")) {
    locate <- str_locate_all(string = string, pattern = "\\[|\\]")[[1]][,"start"]
    depth <- str_sub(string = string, start = locate[1] + 1, end = locate[2] - 1)
    depth <- str_trim(string = str_split(string = depth, pattern = ";", simplify = TRUE)[1,])
    variable <- str_sub(string = string, start = 1, end = locate[1] - 1)
    inlist <- c(inlist, str_c(variable, "[", depth, "]"))
  }
  inlist <- inlist[!str_detect(string = inlist, pattern = ";")]

  # replace alias with variables
  inlist_alias <- NULL
  alias <- c("WATBAL", "SOLBAL" , "ETTERMS", "CROP", "GRASS", "SUBREG_MIN", "SUBREG_ALL")
  for (pattern in alias) {
    rec <- match(x = pattern, table = delete_depth(inlist))
    if (!is.na(rec)) {
      if (pattern == "WATBAL") variable <- watbal
      if (pattern == "SOLBAL") variable <- solbal
      if (pattern == "ETTERMS") variable <- etterms
      if (pattern == "CROP") variable <- crop
      if (pattern == "GRASS") variable <- grass
      if (pattern == "SUBREG_MIN") variable <- subreg_min
      if (pattern == "SUBREG_ALL") variable <- subreg_all
      if (str_detect(string = inlist[rec], pattern = "\\[|\\]")) {
        locate <- str_locate_all(string = inlist[rec], pattern = "\\[|\\]")[[1]][,"start"]
        depth <- str_sub(string = inlist[rec], start = locate[1] + 1, end = locate[2] - 1)
        variable <- str_c(variable, "[", depth, "]")
      }
      inlist_alias <- c(inlist_alias, variable)
      inlist <- inlist[-rec]
    }
  }

  # filter shortlist in case process is not activated
  if (!is.null(inlist_alias)) {
    variable <- NULL
    if (settings$SWSNOW == 0) variable <- c(variable, "SNOW", "SUBLIM", "SSNOW")
    if (settings$SWFROST == 0) variable <- c(variable, "TREDFRS")
    if (settings$SWSOLU == 0) variable <- c(variable, "TREDSOL")
    if (settings$SWCROP == 0) variable <- c(variable, "INTERC", "TPOT", "TACT", "TREDDRY", "TREDWET", "TREDSOL", "TREDFRS", "QTRANS")
    if (settings$SWDRA == 0) variable <- c(variable, "DRAINAGE", "QDRA", "QDRAININ", "QDRAINOUT")
    if (settings$SWRUNON == 0) variable <- c(variable, "RUNON")
    if (settings$SWBOTB == 6) variable <- c(variable, "QBOTTOM", "QBOT", "QBOTIN", "QBOTOUT")
    if (settings$SWSSDI == 0) variable <- c(variable, "QSSDI")
    inlist_alias <- inlist_alias[!delete_depth(inlist_alias) %in% unique(variable)]
    inlist <- c(inlist, inlist_alias)
  }

  # set uppercase
  inlist <- str_to_upper(string = unique(inlist))

  # ---- return of procedure ----

  return(inlist)
}

#' Detect depth information SWAP variable
#'
#' @param variable character string of SWAP variable.
#' @importFrom stringr str_detect
#' @keywords internal
#' @export check_depth
check_depth <- function(variable) {

  # ---- main part of procedure ----

  # detect depth information
  check <- str_detect(string = variable, pattern = "\\[")

  # ---- return of procedure ----

  return(check)
}

#' Delete depth information from SWAP variable
#'
#' @param variable character string of SWAP variable.
#' @importFrom stringr str_which str_locate str_sub
#' @keywords internal
#' @export delete_depth
delete_depth <- function(variable) {

  # ---- main part of procedure ----

  # delete depth information
  for (rec in str_which(string = variable, pattern = "\\[")) {
    done <- FALSE
    while (!done) {
      start <- str_locate(string = variable[rec], pattern = "\\[")[, "start"]
      if (!is.na(start)) {
        end <- str_locate(string = variable[rec], pattern = "\\]")[, "start"]
        variable[rec] <- str_c(str_sub(string = variable[rec], start = 1, end = start - 1), ifelse(end == nchar(variable[rec]), "", str_sub(string = variable[rec], start = end + 1, end = nchar(variable[rec]))))
      } else {
        done <- TRUE
      }
    }
  }

  # ---- return of procedure ----

  return(variable)
}

#' Get plot settings based on SWAP variable
#'
#' @param variable character string of SWAP variable.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c str_locate_all str_sub
#' @importFrom fs path_package
#' @importFrom readr read_rds
#' @keywords internal
#' @export get_plot_settings
get_plot_settings <- function(variable, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- "output_swap"
  output_swap <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(output_swap)) output_swap <- read_rds(file = str_c(path_package(package = "SWAPtools"), "/rds/swap_output.rds"))

  # ---- main part of procedure ----

  # start with clean list
  settings <- list()

  # extract depth from variable (if needed)
  if (check_depth(variable)) {
    locate <- str_locate_all(string = variable, pattern = "\\[|\\]")[[1]][,"start"]
    settings$name <- varname <- str_sub(string = variable, start = 1, end = locate[1] - 1)
    settings$depth <- str_sub(string = variable, start = locate[1] + 1, end = locate[2] - 1)
  } else {
    settings$name <- variable
  }

  # combine with plot settings
  if (!settings$name %in% names(output_swap)) stop("unknown variable selected")
  settings <- c(settings,output_swap[[settings$name]])

  # ---- return of procedure ----

  return(settings)

}
