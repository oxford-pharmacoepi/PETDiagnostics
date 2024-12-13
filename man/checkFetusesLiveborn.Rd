% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkFetusesLiveborn.R
\name{checkFetusesLiveborn}
\alias{checkFetusesLiveborn}
\title{tells us how number of infants and liveborns add up in the mother table}
\usage{
checkFetusesLiveborn(worktable)
}
\arguments{
\item{worktable}{is the motherTable}
}
\value{
returns a table with the checks on fetuses and liveborns
}
\description{
tells us how number of infants and liveborns add up in the mother table
}
\examples{
\donttest{
cdm <- mockPregnancy()
checkFetusesLiveborn(cdm$mothertable)
}
}
