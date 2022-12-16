
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PETDiagnostics

<!-- badges: start -->
<!-- badges: end -->

## Package overview

The goal of PETDiagnostics is to assess feasibility of data sources to perform observational studies on pregnancy related topics using the pregnancy extension tables in OHDSI.

## Installation

You can install the development version of PETDiagnostics like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu/PETDiagnostics")
```

## Example usage
### Create a reference to data in the OMOP CDM format 
The IncidencePrevalence package is designed to work with data in the OMOP CDM format, so our first step is to create a reference to the data using the CDMConnector package. Here we´ll generate an example reference with simulated data (to see how you would create a reference to your database please consult the CDMConnector package documentation).


``` r
library(CDMConnector)
library(dplyr)
library(PETDiagnostics)

# We first need to create a mock database with a cdm reference
# this function creates a motherTable and a babyTable
cdm<-mockPregnancy(motherTable = NULL,
                          babyTable = NULL,
                          pregnancy_size = 100,
                          fetus_size = 110,
                          seed = 1)

# this is what the table(s) look like
# use the motherTable and/or the babyTable depending on your data
head(cdm$motherTable)
head(cdm$babyTable)

```

## Execute the diagnostic checks of your table(s)
### if both tables are available, all checks are possible
### if only the motherTable is available, the "fetusid" check is not possible
### if only the babyTable is available, only the "overview" and "missing" check is possible
``` r
resultList <- executeChecks (
                          motherTable = cdm$motherTable,               #if not available, put NULL
                          babyTable = cdm$babyTable,                   #if not available, put NULL
                          checks = c("overview", "missing", "gestationalAge", "outcomeMode", "fetusesLiveborn",
                                     "fetusid"),                       
                          minCellCount = 5,
                          verbose = FALSE)
```


## Exporting results
### resultList is the named list with results
### databaseId is the database identifier
### outputFolder is the folder to write to
``` r
writeResultToDisk <- function(resultList = resultList, databaseId, outputFolder)
```


You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
