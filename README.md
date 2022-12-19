---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# PETDiagnostics

<!-- badges: start -->
<!-- badges: end -->

The goal of PETDiagnostics is to assess feasibility of data sources to perform observational studies on pregnancy related topics using the pregnancy extension tables in OHDSI.

## Installation

You can install the development version of PETDiagnostics like so:


```r
install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/PETDiagnostics")
```

## Example
# Create a reference to data in the OMOP CDM format

The PETDiagnostics package is designed to work with data in the OMOP CDM format, so our first step is to create a reference to the data using the CDMConnector package. Here we´ll generate an example reference with simulated data (to see how you would create a reference to your database please consult the CDMConnector package documentation).


```r
library(PETDiagnostics)
library(CDMConnector)
#> Warning: package 'CDMConnector' was built under R version 4.2.2
library(dplyr)
#> Warning: package 'dplyr' was built under R version 4.2.2
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union


# We first need to create a mock database with a cdm reference
# this function creates a motherTable and a babyTable
cdm<-mockPregnancy(motherTable = NULL,
                          babyTable = NULL,
                          pregnancy_size = 100,
                          fetus_size = 110,
                          seed = 1)
#> Error in mockPregnancy(motherTable = NULL, babyTable = NULL, pregnancy_size = 100, : could not find function "mockPregnancy"

# this is what the table(s) look like
# use the motherTable and/or the babyTable depending on your data
head(cdm$motherTable)
#> Error in head(cdm$motherTable): object 'cdm' not found
head(cdm$babyTable)
#> Error in head(cdm$babyTable): object 'cdm' not found
```

## Execute the diagnostic checks of your table(s)
# if both tables are available, all checks are possible
# if only the motherTable is available, the "fetusid" check is not possible, put babyTable = NULL
# if only the babyTable is available, only the "overview" and "missing" check is possible, put motherTable = NULL



```r
resultList <- executeChecks (
                          motherTable = cdm$motherTable,               
                          babyTable = cdm$babyTable,                  
                          checks = c("overview", "missing", "gestationalAge", "outcomeMode", "fetusesLiveborn",
                                     "fetusid"),                       
                          minCellCount = 5,
                          verbose = FALSE)
#> Error in executeChecks(motherTable = cdm$motherTable, babyTable = cdm$babyTable, : could not find function "executeChecks"
```
## Exporting results
# resultList is the named list with results
# databaseId is the database identifier
# outputFolder is the folder to write to


```r
writeResultToDisk <- function(resultList = resultList, databaseId, outputFolder)
#> Error: <text>:2:0: unexpected end of input
#> 1: writeResultToDisk <- function(resultList = resultList, databaseId, outputFolder)
#>    ^
```
