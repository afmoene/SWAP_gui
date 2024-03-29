% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_rrwu}
\alias{plot_rrwu}
\title{Create contour-plot with the reduction of root water uptake (uncompensated)}
\usage{
plot_rrwu(file_swp, ...)
}
\arguments{
\item{file_swp}{character string of main SWAP inputfile (swp-file).}

\item{...}{further arguments passed to or from other methods}
}
\description{
Creates contour-plot with the reduction of root water uptake (for uncompensated situation).
}
\details{
Based on the main inputfile of SWAP (\code{file_swp}) and PRWU, RRWU and RDENS a contour-plot is created with the reduction of root water uptake (uncompensated situation).
The plot can be adjusted by \code{tstart}, \code{tend} and \code{ylim}.
The labels of the plot can be set to 'EN' (English; default) or 'NL' (Dutch) using \code{language}.

The function will created an error by default which will stop the R-program. The error-message can be changed into a warning by setting \code{error} to 'FALSE'.
}
