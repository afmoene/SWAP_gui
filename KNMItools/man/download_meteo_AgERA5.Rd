% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AgERA5.R
\name{download_meteo_AgERA5}
\alias{download_meteo_AgERA5}
\title{Download observations from \href{https://cds.climate.copernicus.eu}{AgERA5}}
\usage{
download_meteo_AgERA5(dir_out, latitude, longitude, tstart, tend, ...)
}
\arguments{
\item{dir_out}{character string, name of directory}

\item{latitude}{numeric, the latitude of the site in decimal degrees}

\item{longitude}{numeric, the longitude of the site in decimal degrees}

\item{tstart}{date, start of period}

\item{tend}{date, end of period}

\item{...}{further arguments passed to or from other methods}
}
\description{
Download observations from \href{https://cds.climate.copernicus.eu}{AgERA5}
}
\examples{
# download meteo from AgERA5 (do not run!)
#download_meteo_AgERA5(dir_out = "../temp_agera5",
# latitude = 52.1, longitude = 5.18, tstart = "2000-01-01", tend = "2001-12-31")
}
