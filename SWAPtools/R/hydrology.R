#' Determine mean highest and mean lowest groundwater level
#'
#' @param db dataframe with columns 'date' and 'value'.
#' @param ... further arguments passed to or from other methods
#' @importFrom stringr str_c
#' @importFrom lubridate year month day as_date interval time_length
#' @importFrom tibble tibble deframe
#' @importFrom dplyr %>% mutate select filter left_join group_by summarise arrange
#' @importFrom stats aggregate
#' @importFrom controlR hydryear
#' @return return mean highest and mean lowest groundwater level [cm+mv]
#' @details
#' To derive the mean highest (GHG) and mean lowest (GLG) groundwater level, all values on the 14th and 28th day of the month are selected leaded to 24 values for each hydrologic year (April - March).
#' From this the three highest (HG3) and three lowest (LG3) values are selected and averaged over a long term period resulting in the GHG and GLG. Normally the long term period should be 8 years at least.
#'
#' In case of incomplete timeseries of groundwater level observations on the 14th and 28th day of the month a certain shift in days is allowed. As a default a maximum of 5 days is used, but this can be adjusted by \code{max_shift}.
#' Although the shift in days, it is still possible the timeseries is missing observations. A certain amount of missing observations is allowed (deault 0.9; this can be adjusted by \code{min_frac}.
#' Furthermore the length of the long term period can be adjusted by \code{min_year} (default: 8 years).
#'
#' The period over which the groundwater characteristics should be determined can be adjusted by specifying \code{tstart} and/or \code{tend}.
#' @export get_gxg
get_gxg <- function(db, ...) {

  date_org <- value <- observed <- NULL

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("max_shift", "min_frac", "min_year", "tstart", "tend", "quiet")
  max_shift <- min_frac <- min_year <- tstart <- tend <- quiet <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", str_c(unused_param, collapse = ', '))

  # check used arguments
  if (is.null(max_shift)) max_shift <- 5
  if (is.null(min_frac)) min_frac <- 0.9
  if (is.null(min_year)) min_year <- 8
  if (is.null(quiet)) quiet <- FALSE

  # ---- main part of procedure ----

  # check if columns 'date' and 'value' are available
  if (!all(c("date", "value") %in% names(db))) stop("data should contain columns 'date' and 'value'")

  # set period
  if (is.null(tstart)) tstart <- min(db$date)
  if (is.null(tend)) tend <- max(db$date)

  if (time_length(interval(tstart, tend), "day") < 365) {
    message("unable to calculate gxg, interval is too small")
    return(c(NA, NA))
  }

  # sequence of 14th and 28th
  db_gxg <- tibble(date = seq(from = tstart, to = tend, by = 1), observed = NA) %>%
    filter(day(date) %in% c(14, 28))

  # merge datasets
  db_gxg <- left_join(x = db_gxg, y = db[, c("date", "value")], by = "date") %>%
    mutate(observed = if_else(is.na(value), NA_real_, value)) %>%
    select(date, observed)

  # shift observation in case of missing values
  done <- all(!is.na(db_gxg$observed))
  if (!done) {

    shift <- 1
    db <- db %>%
      mutate(date_org = date)

    # shift days till all records are filled
    while (!done) {

      # shift up and down
      db_min <- db %>%
        mutate(date = date_org - shift) %>%
        filter(day(date) %in% c(14, 28)) %>%
        select(date, value)
      db_max <- db %>%
        mutate(date = date_org + shift) %>%
        filter(day(date) %in% c(14, 28)) %>%
        select(date, value)
      db_add <- rbind(db_min, db_max) %>%
        group_by(date) %>%
        summarise(
          value = mean(value),
          .groups = "drop"
        )

      # merge shifted observations
      if (nrow(db_add) != 0) {
        db_gxg <- left_join(x = db_gxg, y = db_add, by = "date") %>%
          mutate(observed = if_else(is.na(observed), if_else(is.na(value), NA_real_, value), observed)) %>%
          select(date, observed)
      }

      # check if done
      shift <- shift + 1
      if (shift > max_shift) done <- TRUE
      if (all(!is.na(db_gxg$observed))) done <- TRUE
    }
  }

  # determine GxG
  db_gxg <- db_gxg %>% mutate(hydryear = hydryear(db_gxg$date))
  hydryear <- unique(db_gxg$hydryear)
  ghg <- NULL
  glg <- NULL
  n_year <- 0
  for (s_hydryear in hydryear) {
    gxg <- db_gxg %>%
      filter(hydryear == all_of(s_hydryear) & !is.na(observed)) %>%
      select(observed) %>%
      arrange(observed) %>%
      deframe
    n_obs <- length(gxg)
    if (n_obs >= min_frac * 24) {
      n_year <- n_year + 1
      ghg <- c(ghg, gxg[(n_obs - 2):n_obs])
      glg <- c(glg, gxg[1:3])
    }
  }

  if (n_year >= min_year) {

    ghg <- mean(ghg)
    glg <- mean(glg)

    if (!quiet) {
      message(str_c("\nGHG:", formatC(x = round(-ghg), format = "d", width = 4), " [cm-mv]"))
      message(str_c("GLG:", formatC(x = round(-glg), format = "d", width = 4), " [cm-mv]"))
    }

  } else {

    ghg <- NA
    glg <- NA

    if (!quiet) {
      frac <- nrow(db_gxg[!is.na(db_gxg$observed),]) / nrow(db_gxg)
      message(str_c("unable to determine GxG (", formatC(x = frac * 100, format = "f", digits = 1), "% observed)"))
    }

  }

  # ---- return of procedure ----

  return(c(ghg,glg))
}

#' Get GT class
#'
#' @param ghg value. Average highest groundwaterlevel [cm+mv].
#' @param glg value. Average lowest groundwaterlevel [cm+mv].
#' @param opt character string. options: 1966, 1977 or 1995.
#' @return return GT based on \code{GHG} and \code{GLG}
#' @export get_gt
get_gt <- function(ghg, glg, opt="1966") {

  # ---- initial part of procedure ----

  # check selected option of GT
  opts <- c("1966","1977","1995")
  if (!opt %in% opts) stop(paste0("selected opt '",opt,"' not available"))

  # initialize Gt
  gt    <- rep(x=NA,times=length(ghg))

  # ---- main part of procedure ----

  if (opt == "1966") {
    gt[glg > -50] <- "I"
    gt[glg <= -50 & glg > -80] <- "II"
    gt[glg <= -80 & glg > -120 & ghg > -40] <- "III"
    gt[glg <= -80 & glg > -120 & ghg <= -40] <- "IV"
    gt[glg <= -120 & ghg > -40] <- "V"
    gt[glg <= -120 & ghg <= -40 & ghg > -80] <- "VI"
    gt[glg <= -120 & ghg <= -80] <- "VII"
  }

  if (opt == "1977") {
    gt[glg > -50] <- "I"
    gt[glg <= -50  & glg > -80 & ghg > -25] <- "II"
    gt[glg <= -50  & glg > -80 & ghg <= -25] <- "II*"
    gt[glg <= -80  & glg > -120 & ghg > -25] <- "III"
    gt[glg <= -80  & glg > -120 & ghg <= -25 & ghg > -40] <- "III*"
    gt[glg <= -80  & glg > -120 & ghg <= -40] <- "IV"
    gt[glg <= -120 & ghg > -25] <- "V"
    gt[glg <= -120 & ghg <= -25 & ghg > -40] <- "V*"
    gt[glg <= -120 & ghg <= -40 & ghg > -80] <- "VI"
    gt[glg <= -120 & ghg <= -80 & ghg > -140] <- "VII"
    gt[glg <= -120 & ghg <= -140] <- "VII*"
  }

  if (opt == "1995") {
    gt[glg > -50   & ghg > -25] <- "Ia"
    gt[glg > -50   & ghg <= -25] <- "Ic"
    gt[glg <= -50  & glg > -80 & ghg > -25] <- "IIa"
    gt[glg <= -50  & glg > -80 & ghg <= -25 & ghg > -40] <- "IIb"
    gt[glg <= -50  & glg > -80 & ghg <= -40] <- "IIc"
    gt[glg <= -80  & glg > -120 & ghg > -25] <- "IIIa"
    gt[glg <= -80  & glg > -120 & ghg <= -25 & ghg > -40] <- "IIIb"
    gt[glg <= -80  & glg > -120 & ghg <= -40 & ghg > -80] <- "IVu"
    gt[glg <= -80  & glg > -120 & ghg <= -80] <- "IVc"
    gt[glg <= -120 & ghg > -25] <- "Va"
    gt[glg <= -120 & ghg <= -25 & ghg > -40] <- "Vb"
    gt[glg <= -120 & ghg <= -40 & ghg > -80] <- "VI"
    gt[glg <= -120 & ghg <= -80 & ghg > -140] <- "VII"
    gt[glg <= -120 & ghg <= -140] <- "VIII"
  }

  # ---- return of procedure ----

  return(gt)
}
