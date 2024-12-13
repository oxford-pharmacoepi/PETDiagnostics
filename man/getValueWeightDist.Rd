% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getValueWeightDist.R
\name{getValueWeightDist}
\alias{getValueWeightDist}
\title{tells us what the distribution of birth weight in the baby table is}
\usage{
getValueWeightDist(babytable)
}
\arguments{
\item{babytable}{is the babytable}
}
\value{
returns values of birthweight
}
\description{
tells us what the distribution of birth weight in the baby table is
}
\examples{
\donttest{
cdm <- mockPregnancy()
getValueWeightDist(cdm$babytable)
}
}
