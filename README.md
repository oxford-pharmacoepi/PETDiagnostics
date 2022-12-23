
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PETDiagnostics

<!-- badges: start -->
<!-- badges: end -->

The goal of PETDiagnostics is to assess feasibility of data sources to
perform observational studies on pregnancy related topics using the
pregnancy extension tables in OHDSI.

## Installation

You can install the development version of PETDiagnostics like so:

``` r
install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/PETDiagnostics")
```

## Example

### Create a reference to data in the OMOP CDM format

The PETDiagnostics package is designed to work with data in the OMOP CDM
format, so our first step is to create a reference to the data using the
CDMConnector package. Here we´ll generate an example reference with
simulated data (to see how you would create a reference to your database
please consult the CDMConnector package documentation).

``` r
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

# this is what the table(s) look like
# use the motherTable and/or the babyTable depending on your data
head(cdm$motherTable)
#> # Source:   SQL [6 x 27]
#> # Database: DuckDB 0.5.1 [tburkard@Windows 10 x64:R 4.2.1/:memory:]
#>   pregna…¹ perso…² pregnanc…³ pregnanc…⁴ gesta…⁵ pregn…⁶ pregn…⁷ pregn…⁸ pregn…⁹
#>   <chr>    <chr>   <date>     <date>       <dbl>   <dbl>   <dbl>   <int>   <dbl>
#> 1 1        68      2010-12-05 2011-02-01      58 4092289 4015701 4188539 4095714
#> 2 2        39      2004-09-01 2005-02-21     173 4081422       0 4188540 4053842
#> 3 3        1       2019-10-03 2020-01-19     108 4092289       0 4188540 4242253
#> 4 4        34      2001-05-15 2002-01-08     238  443213       0 4188539 4095714
#> 5 5        87      2010-05-15 2011-01-14     244 4092289 4125611 4188540 4095714
#> 6 6        43      2018-03-21 2018-08-13     145       0       0 4188539 4338692
#> # … with 18 more variables: pregnancy_number_fetuses <int>,
#> #   pregnancy_number_liveborn <int>, prev_pregnancy_gravidity <int>,
#> #   prev_livebirth_number <int>, prev_stillbirth_number <int>,
#> #   prev_miscar_number <int>, prev_TOP_number <int>, prev_TOP12_number <int>,
#> #   prev_pregnancy_parity <dbl>, pregnancy_BMI <int>,
#> #   pregnancy_outcome_source_value <chr>,
#> #   pregnancy_mode_delivery_source_value <chr>, pregnancy_folic <int>, …
head(cdm$babyTable)
#> # Source:   SQL [6 x 8]
#> # Database: DuckDB 0.5.1 [tburkard@Windows 10 x64:R 4.2.1/:memory:]
#>   pregnancy_id fetus_id birth_outcome birth_we…¹ birth…² birth…³ birth…⁴ birth…⁵
#>   <chr>        <chr>            <dbl>      <int>   <int>   <int>   <int>   <int>
#> 1 1            1              4092289       2447 4188540 4188539 4188540       6
#> 2 2            2              4092289       1827 4188540 4188540 4188539      10
#> 3 3            3              4092289       1056 4188539 4188539 4188539       0
#> 4 4            4                   NA        786 4188539 4188539 4188539      10
#> 5 5            5              4092289       3716 4188540 4188539 4188539       8
#> 6 6            6               443213       4201 4188540 4188540 4188539       7
#> # … with abbreviated variable names ¹​birth_weight, ²​birth_con_malformation,
#> #   ³​birth_SGA, ⁴​birth_FGR, ⁵​birth_APGAR
```

## Execute the diagnostic checks of your table(s)

### if both tables are available, all checks are possible

### if only the motherTable is available, the “fetusid” and “weightDist” check is not possible, put babyTable = NULL

### if only the babyTable is available, only the “overview”, “missing”, “weightDist”, and “bitSet” check is possible, put motherTable = NULL

``` r
resultList <- executeChecks (
                          motherTable = cdm$motherTable,               
                          babyTable = cdm$babyTable,                  
                          checks = c("overview","annualOverview","missing", "unknown","gestationalAge","datesAgeDist","outcomeMode",
                                     "fetusesLiveborn","fetusid","weightDist","bitSet"),                       
                          minCellCount = 5,
                          verbose = FALSE)
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> Warning: attributes are not identical across measure variables; they will be
#> dropped
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> No id variables; using all as measure variables
#> Warning: attributes are not identical across measure variables; they will be
#> dropped
```

## Exporting results

### resultList is the named list with results

### databaseId is the database identifier

### outputFolder is the folder to write to

``` r
writeResultToDisk (resultList = resultList, databaseId = "mock", outputFolder = tempdir())
```
