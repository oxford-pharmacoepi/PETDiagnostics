% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getValueDatesAgeDist.R
\name{getValueDatesAgeDist}
\alias{getValueDatesAgeDist}
\title{Title}
\usage{
getValueDatesAgeDist(mothertable)
}
\arguments{
\item{mothertable}{is the mothertable}
}
\value{
returns a table with the distrubtion of pregnancy starte date, end date, and gestational Age
}
\description{
Title
}
\examples{
\donttest{
cdm <- mockPregnancy()
getValueDatesAgeDist(cdm$mothertable)
}
}
