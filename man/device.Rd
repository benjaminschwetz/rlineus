% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/device.R
\name{device}
\alias{device}
\alias{lineus_dev}
\title{graphics device for line-us plotter}
\usage{
lineus_dev(portrait = TRUE)
}
\arguments{
\item{portrait}{set orientation. \code{TRUE} for portrait, \code{FALSE} for landscape}
}
\description{
readme for how to use it
}
\examples{
## Only run this example in interactive R sessions
if (interactive()) {
 lineus_dev()
 boxplot(data = iris, Sepal.Length ~Species)
 dev.off()
}
}
