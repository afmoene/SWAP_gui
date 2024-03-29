% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/template.R
\name{get_settings_SWAP}
\alias{get_settings_SWAP}
\title{Get settings of SWAP-variable(s)}
\usage{
get_settings_SWAP(variable, ...)
}
\arguments{
\item{variable}{character string, name of SWAP variable.}

\item{...}{further arguments passed to or from other methods}
}
\description{
Get settings of SWAP-variable(s)
}
\examples{
# specify swp-file
file_swp <- paste0(tempfile(pattern = "swap"), "/swap.swp")
zipfile <- system.file("extdata/input.zip", package = "SWAPtools")
unzip(zipfile = zipfile, files = basename(file_swp), exdir = dirname(file_swp))

# extract name of project
get_settings_SWAP(file = file_swp, variable = c("PROJECT", "OUTFIL"))

# extract number of layers
get_settings_SWAP(file = file_swp, variable = "SOILPROFILE::ISOILLAY")

# extract saturated conductivity
get_settings_SWAP(file = file_swp, variable = c("SOILHYDRFUNC::OSAT", "SOILHYDRFUNC::ORES"))

# clean example
unlink(x = dirname(file_swp), recursive = TRUE)
}
