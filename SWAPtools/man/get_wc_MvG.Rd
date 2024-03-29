% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MvG.R
\name{get_wc_MvG}
\alias{get_wc_MvG}
\title{Get water content based on pressure head}
\usage{
get_wc_MvG(H, WCR, WCS, ALPHA, NPAR)
}
\arguments{
\item{H}{pressure head [cm].}

\item{WCR}{residual water content [cm3 cm-3].}

\item{WCS}{saturated water content [cm3 cm-3].}

\item{ALPHA}{curve shape parameter [-].}

\item{NPAR}{curve shape parameter [-].}
}
\value{
return water content [cm3 cm-3].
}
\description{
Get water content based on pressure head
}
\keyword{internal}
