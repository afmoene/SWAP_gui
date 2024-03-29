% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_grass}
\alias{plot_grass}
\title{Create timeseries-plot of potential and actual crop development of grassland}
\usage{
plot_grass(file_swp, ...)
}
\arguments{
\item{file_swp}{character string of main SWAP inputfile (swp-file).}

\item{...}{further arguments passed to or from other methods}
}
\description{
Creates timeseries-plot of simulated potential and actual crop development of grassland
}
\details{
Based on the main inputfile of SWAP (\code{file_swp}) a timeseries-plot is created of the potential and actual crop development of grassland.
The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.

In case observations are available it is possible to add the observations as red dots in the plot.
A path to a folder with observations should be specified with \code{dir_obs}.
In this folder a csv-file with the name of the SWAP-variable should be present.
The file itself should have two columns: 'DATETIME' and 'VALUE'. The unit of the values should be equal to SWAP.
Optionally the model performance (\code{performance}) can be added in the plot (see also: \link{get_modelperformance}).

By default the crop development of aboveground biomass (including harvest by mowing and grazing) is plotted for grassland (simulated by GRASS; CROPTYPE = 3) in the crop-calendar during the simulation period.
Specify a list of crops with \code{cropfil} in case only the development of specific crop(s) is preferred.
In case the development of a certain part of the plant is preferred specify \code{growth} (options: aboveground biomass and harvest ('AGB+'), aboveground biomass ('AGB'), roots ('RT'), Leaf Area Index ('LAI') or rootzone ('RD').

The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
}
