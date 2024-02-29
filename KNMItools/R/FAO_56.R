#' Reference evapotranspiration (mm d-1)
#'
#' Reference: \href{http://www.fao.org/3/X0490E/x0490e00.htm#Contents}{FAO 56}; equation 6
#'
#' @param date date (yyyy-mm-dd)
#' @param Q global radiation (kJ m-2 d-1)
#' @param TN minimum temperature (Celsius)
#' @param TX maximum temperature (Celsius)
#' @param UN minimum relative humidity (\%)
#' @param UX maximum relative humidity (\%)
#' @param FG wind speed at height \code{altw} (m s-1)
#' @param lat lattitude (degrees, minutes)
#' @param alt altitude above sea level (m)
#' @param altw height at which \code{FG} was measured (m)
#' @export reference_evapotranspiration
#' @examples
#' reference_evapotranspiration(date = "2001-07-06", Q = 22070, TN = 12.3,
#'  TX = 21.5, UN = 63, UX = 84, FG = 2.778, lat = 50.48, alt = 100.0, altw = 10.0)
reference_evapotranspiration <- function(date, Q, TN, TX, UN, UX, FG, lat, alt, altw) {

  # ---- initial part of procedure ----

  # parameters
  lambda <- 2.45
  albedo <- 0.23
  eps <- 0.622
  cp <- 1.013e-3
  p1 <- 4096
  p2 <- 0.6108

  # ---- main part of procedure ----

  # average daily temperature
  Tmean = 0.5 * (TN + TX)

  # unit conversion from kJ to MJ
  Rs <- Q / 1000

  # windspeed correction
  u2 <- FG * 4.87 / log(67.8 * altw - 5.42)

  # vapour pressure
  ea <- actual_vapour_pressure(TN = TN, TX = TX, UN = UN, UX = UX)
  es <- (saturation_vapour_pressure(T = TN) + saturation_vapour_pressure(T = TX)) / 2

  # gamma
  gamma <- cp * (101.3 * ((293 - 0.0065 * alt) / 293)^5.26) / (eps * lambda)

  # delta
  delta <- p1 * p2 * exp(17.27 * Tmean / (Tmean + 237.3)) / (Tmean + 237.3)^2

  # net Radiation
  Rn <- net_radiation(date = date, Rs = Rs, Tmin = TN, Tmax = TX, ea = ea, lat = lat, alt = alt, albedo = albedo)

  # reference evapotranspiration
  x1 <- 0.408 * delta * Rn
  x2 <- gamma * 900 / (Tmean + 273) * u2
  x3 <- es - ea
  x4 <- delta + gamma * (1 + 0.34 * u2)
  ET0 <- (x1 + x2 * x3) / x4

  # ---- return of procedure ----

  return(ET0)
}

#' Net radiation (Mj m-2 d-1)
#'
#' Reference: \href{http://www.fao.org/3/X0490E/x0490e00.htm#Contents}{FAO 56}; equation 40
#'
#' @param date date (yyyy-mm-dd)
#' @param Rs global radiation (MJ m-2 d-1)
#' @param Tmin minimum temperature (Celsius)
#' @param Tmax maximum temperature (Celsius)
#' @param ea vapour pressure (kPa)
#' @param lat lattitude (degrees, minutes)
#' @param alt altitude above sea level (m)
#' @param albedo albedo (-)
#' @export net_radiation
#' @examples
#' net_radiation(date = "2001-07-06", Rs = 22.07, Tmin = 12.3,
#'  Tmax = 21.5, ea = 1.40824, lat = 50.48, alt = 100.0)
net_radiation <- function(date, Rs, Tmin, Tmax, ea, lat, alt, albedo = 0.23) {

  # ---- main part of procedure ----

  Ra <- extraterrestrial_radiation(date = date, lat = lat)
  Rso <- (0.75 + 2e-5 * alt) * Ra
  Rns <- (1 - albedo) * Rs
  Rnl <- net_longwave_radiation(Rs = Rs, Rso = Rso, Tmin = Tmin, Tmax = Tmax, ea = ea)
  net_radiation <- Rns - Rnl

  # ---- return of procedure ----

  return(net_radiation)
}

#' Net longwave radiation (MJ m-2 d-1)
#'
#' Reference: \href{http://www.fao.org/3/X0490E/x0490e00.htm#Contents}{FAO 56}; equation 39
#'
#' @param Rs global radiation (MJ m-2 d-1)
#' @param Rso clear sky solar radiation (MJ m-2 d-1)
#' @param Tmin minimum temperature (Celsius)
#' @param Tmax maximum temperature (Celsius)
#' @param ea actual vapour pressure (kPa)
#' @export net_longwave_radiation
#' @examples
#' net_longwave_radiation(Rs = 22.07, Rso = 30.90, Tmin = 12.3, Tmax = 21.5, ea = 1.40824)
net_longwave_radiation <- function(Rs, Rso, Tmin, Tmax, ea) {

  # ---- initial part of procedure ----

  stef_bol <- 4.903e-9

  # ---- main part of procedure ----

  t1 <- Tmin + 273.15
  t2 <- Tmax + 273.15

  Rnl <- stef_bol * ((t1^4 + t2^4) / 2) * (0.34 - 0.14 * ea^0.5) * (1.35 * (min(Rs / Rso, 1.0)) - 0.35)

  # ---- return of procedure ----

  return(Rnl)
}

#' Extraterrestrial radiation (MJ m-2 d-1)
#'
#' Reference: \href{http://www.fao.org/3/X0490E/x0490e00.htm#Contents}{FAO 56}; equation 21
#'
#' @param date date (yyyy-mm-dd)
#' @param lat lattitude (degrees, minutes)
#' @importFrom controlR dayyear
#' @export extraterrestrial_radiation
#' @examples
#' extraterrestrial_radiation(date = "2001-07-06", lat = 50.48)
extraterrestrial_radiation <- function(date, lat) {

  # ---- initial part of procedure ----

  gsc <- 0.082

  # ---- main part of procedure ----

  lat_dec <- floor(lat) + ((lat%%1) * 100) / 60
  fi = (lat_dec / 180.) * pi

  daynr <- dayyear(date = date)
  dr <- 1 + 0.033 * cos(2 * pi * daynr / 365)
  delta <- 0.409 * sin(2 * pi * daynr / 365 - 1.39)
  omega <- acos(- tan(fi) * tan(delta))
  Ra <- (24 * 60 / pi) * gsc * dr * (omega * sin(fi) * sin(delta) + cos(fi) * cos(delta) * sin(omega))

  # ---- return of procedure ----

  return(Ra)
}

#' Actual vapour pressure derived from relative humidity
#'
#' @param TN numeric, minimum daily temperature (Celsius)
#' @param TX numeric, maximum daily temperature (Celsius)
#' @param ... further arguments passed to or from other methods.
#' @export actual_vapour_pressure
#' @description Calculation of actual vapour pressure (kPa; reference: \href{http://www.fao.org/3/X0490E/x0490e00.htm#Contents}{FAO 56}).
#' @details Either minimum and maximum relative humidity (\code{UN} and \code{UX}) or mean relative humidity (\code{UG}) should be specified.
#' In case of usage \code{UN} and \code{UX} the actual vapour pressure is calculated by equation 17.
#' In absence of \code{UN} and \code{UX} the actual vapour pressure is calculated by equation 19 using \code{UG}.
#' @examples
#' actual_vapour_pressure(TN = 18.0, TX = 25.0, UN = 54.0, UX = 82.0)
#' actual_vapour_pressure(TN = 18.0, TX = 25.0, UG = (54.0 + 82.0) / 2)
actual_vapour_pressure <- function(TN, TX, ...) {

  # ---- initial part of procedure ----

  # set optional arguments
  opt_param <- c("UN", "UX", "UG")
  UN <- UX <- UG <- NULL

  # load additional arguments
  param <- list(...)
  for (name in names(param) ) assign(name, param[[name]])

  # check unused arguments
  unused_param <- setdiff(names(param), opt_param)
  if (length(unused_param)) stop("unused parameter: ", paste(unused_param, collapse = ', '))

  # check used arguments
  if (!is.null(UN) & is.null(UX)) stop("'UN' and 'UX' should be specified")
  if (is.null(UN) & !is.null(UX)) stop("'UN' and 'UX' should be specified")
  if ((is.null(UN) | is.null(UX)) & is.null(UG)) stop("either 'UN' and 'UX' or 'UG' should be specified")

  # ---- main part of procedure ----

  # calculate saturated vapour pressure
  e0TN <- saturation_vapour_pressure(T = TN)
  e0TX <- saturation_vapour_pressure(T = TX)

  # calculate actual vapour pressure
  if (!is.null(UN)) {
    value <- (e0TN * (UX / 100) + e0TX * (UN / 100)) / 2
  } else {
    value <- UG / 100 * ((e0TN + e0TX) / 2)
  }

  # ---- return of procedure ----

  return(value)
}

#' Estimation of saturation vapour pressure
#'
#' Reference: \href{http://www.fao.org/3/X0490E/x0490e00.htm#Contents}{FAO 56}; equation 11
#'
#' @param T numeric, air temperature (Celsius)
#' @export saturation_vapour_pressure
saturation_vapour_pressure <- function(T) {

  # Reference FAO 56
  # Chapter 3 Meteorological data
  # Equation 11 (mean saturation vapour pressure)
  # kPa

  # ---- main part of procedure ----

  value <- 0.6108 * exp((17.27 * T) / (T + 237.3))

  # ---- return of procedure ----

  return(value)
}
