#' this function executes the desired checks "overview","annualOverview","missing", "unknown","gestationalAge","datesAgeDist","outcomeMode", "fetusesLiveborn","fetusid","weightDist","bitSet"
#'
#' @param mothertable input a table according to the mothertable of the pregnancy extension Table in OHDSI, can be NULL
#' @param babytable input a table according to the mothertable of the pregnancy extension Table in OHDSI, can be NULL
#' @param checks chose the checks you want to perform, not all checks are possible depending on which tables are available
#' @param minCellCount chose a number below you want to obscure counts, 0 is not obscured
#' @param minGestAge_Days chose a number below you think that the pregnancy entries were wrong / should not be there
#' @param verbose will give you the information the package is performing at that moment
#'
#' @return all the individual result tables with data obscured if need be
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' executeChecks(cdm$mothertable,cdm$babytable)
#' }
executeChecks <- function(#cdm,
                          mothertable = NULL,
                          babytable = NULL,
                          checks = c("overview","annualOverview","missing", "unknown","gestationalAge","datesAgeDist","outcomeMode", "fetusesLiveborn",
                                     "fetusid","weightDist","bitSet"),
                          minCellCount = 5,
                          minGestAge_Days = 21,
                          verbose = FALSE) {

  errorMessage <- checkmate::makeAssertCollection()
  # only as soon as these are part of the cdm
  # checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  # checkTableExists(cdm = cdm, tableName = mothertable,
  #                  messageStore = errorMessage)
  # checkTableExists(cdm = cdm, tableName = babytable,
  #                  messageStore = errorMessage)
  checkmate::assertTRUE(is.null(mothertable) || inherits(mothertable, 'tbl_dbi'), add = errorMessage)
  checkmate::assertTRUE(is.null(babytable) || inherits(babytable, 'tbl_dbi'), add = errorMessage)

  checkmate::assertTRUE(is.numeric(minCellCount) || is.null(minCellCount), add = errorMessage)
  checkmate::assertTRUE(is.numeric(minGestAge_Days) || is.null(minGestAge_Days), add = errorMessage)
  checkLogical(verbose, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  PETOverviewMother <- NULL
  PETOverviewBaby <- NULL
  if ("overview" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: total number of women, pregnancies (and fetuses)", start)
    }
    if (!is.null(mothertable)) {

      PETOverviewMother <- getOverview(mothertable) %>% dplyr::collect()
    }
    if (!is.null(babytable)) {

      PETOverviewBaby <- getOverview(babytable) %>% dplyr::collect()
    }
  }



  AnnualPETOverviewMother <- NULL
  if ("annualOverview" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: total number of women, pregnancies (and fetuses) per year", start)
    }
    if (!is.null(mothertable)) {

      AnnualPETOverviewMother <- getAnnualOverview(mothertable) %>% dplyr::collect()
    }

  }


  missingSummaryMother <- NULL
  missingSummaryBaby <- NULL
  if ("missing" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check Missing of all variables", start)
    }
    if (!is.null(mothertable)) {

    missingSummaryMother <- getMissings(mothertable) %>% dplyr::collect()
    }
  if (!is.null(babytable)) {

    missingSummaryBaby <- getMissings(babytable) %>% dplyr::collect()
  }
  }



  unknownSummaryMother <- NULL
  if ("unknown" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check unknowns of required variables", start)
    }
    if (!is.null(mothertable)) {

      unknownSummaryMother <- getUnknown(mothertable) %>% dplyr::collect()
    }

  }



  gestationalAgeMatch <-  NULL
  if ("gestationalAge" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check Gestational Age", start)
    }
    if (!is.null(mothertable)) {

  gestationalAgeMatch <- summariseGestationalAge(mothertable,minGestAge_Days) %>% dplyr::collect()
    }
  }




  valueDatesAgeDist <-  NULL
  if ("datesAgeDist" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check values of dates and Gestational Age", start)
    }
    if (!is.null(mothertable)) {

      valueDatesAgeDist <- getValueDatesAgeDist(mothertable) %>% dplyr::collect()
    }
  }




  outcomeModeMatch <-  NULL
  if ("outcomeMode" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check Outcome and Mode of Delivery", start)
    }
    if (!is.null(mothertable)) {

   outcomeModeMatch <- checkOutcomeMode(mothertable) %>% dplyr::collect()
    }
  }



  fetusesLivebornNumber <-  NULL
  if ("fetusesLiveborn" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check number of fetuses versus liveborn", start)
    }
# pregnancy_single is a required variable
    if ("pregnancy_number_fetuses" %in% colnames(mothertable) && "pregnancy_number_liveborn" %in% colnames(mothertable)) {

   fetusesLivebornNumber <- tibble::as_tibble(checkFetusesLiveborn(mothertable)) %>% dplyr::collect()
    }
  }




  fetusIdMatch <- NULL
  if ("fetusid" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check number of fetuses versus liveborn", start)
    }
    if (!is.null(mothertable) && !is.null(babytable)) {

   fetusIdMatch <- checkFetusId(mothertable,babytable) %>% dplyr::collect()
      }

  }



  valueWeightDist <- NULL
  if ("weightDist" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check values of birthweight", start)
    }
    if (!is.null(babytable)) {

      valueWeightDist <- getValueWeightDist(babytable) %>% dplyr::collect()
    }

  }



  bitSetOverviewAll <- NULL
  bitSetOverviewMother <- NULL
  bitSetOverviewBaby <- NULL
  if ("bitSet" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check missing/unknown data pattern", start)
    }
    if (!is.null(mothertable) && !is.null(babytable)) {

      bitSetOverviewAll  <- getBitSet(mothertable,babytable) %>% dplyr::collect()
    }
    if (!is.null(mothertable) && "pregnancy_number_fetuses" %in% colnames(mothertable)) {

        bitSetOverviewMother  <- getBitSet(mothertable, babytable = NULL) %>% dplyr::collect()
      }
    if (!is.null(babytable)) {

        bitSetOverviewBaby  <- getBitSet(mothertable = NULL, babytable) %>% dplyr::collect()
      }
    }



   if (verbose == TRUE) {
    start <- printDurationAndMessage("Finished", start)
  }



  if (!is.null(mothertable) && !is.null(babytable)) {

  result <- list("PETOverviewMother" = PETOverviewMother,
                 "AnnualPETOverviewMother" = AnnualPETOverviewMother,
                 "PETOverviewBaby" = PETOverviewBaby,
                 "missingSummaryMother" = missingSummaryMother,
                 "missingSummaryBaby" = missingSummaryBaby,
                 "unknownSummaryMother" = unknownSummaryMother,
                 "gestationalAgeMatch" = gestationalAgeMatch,
                 "valueDatesAgeDist" =  valueDatesAgeDist,
                 "outcomeModeMatch" = outcomeModeMatch,
                 "fetusesLivebornNumber" = fetusesLivebornNumber,
                 "fetusIdMatch" = fetusIdMatch,
                 "valueWeightDist" = valueWeightDist,
                 "bitSetOverviewAll" =  bitSetOverviewAll,
                 "bitSetOverviewMother" =  bitSetOverviewMother,
                 "bitSetOverviewBaby" =  bitSetOverviewBaby
                 )

  } else if  (!is.null(mothertable)) {

    result <- list("PETOverviewMother" = PETOverviewMother,
                   "AnnualPETOverviewMother" = AnnualPETOverviewMother,
                   "missingSummaryMother" = missingSummaryMother,
                   "unknownSummaryMother" = unknownSummaryMother,
                   "gestationalAgeMatch" = gestationalAgeMatch,
                   "valueDatesAgeDist" =  valueDatesAgeDist,
                   "outcomeModeMatch" = outcomeModeMatch,
                   "fetusesLivebornNumber" = fetusesLivebornNumber,
                   "bitSetOverviewMother" =  bitSetOverviewMother)

  }   else  if  (!is.null(babytable)) {

    result <- list("PETOverviewBaby" = PETOverviewBaby,
                   "missingSummaryBaby" = missingSummaryBaby,
                   "valueWeightDist" = valueWeightDist,
                   "bitSetOverviewBaby" =  bitSetOverviewBaby)

  }


  return(Filter(Negate(is.null), sapply(names(result),
                                        FUN = function(tableName) {
                                          table <- result[[tableName]]
                                          if (!is.null(table)) {
                                          obscureCounts(table = table,
                                                        tableName = tableName,
                                                        minCellCount = minCellCount,
                                                        substitute = NA)
                                          }
                                        },
                                        simplify = FALSE,
                                        USE.NAMES = TRUE)))


}


#' Write diagnostics results to a zip file on disk in given output folder.
#'
#' @param resultList named list with results
#' @param databaseId database identifier
#' @param outputFolder folder to write to
#' @importFrom here here
#'
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' resultList <- executeChecks(cdm$mothertable,cdm$babytable)
#' writeResultToDisk(resultList = resultList, databaseId = "test", outputFolder= here::here())
#' }
writeResultToDisk <- function(resultList, databaseId, outputFolder) {
  if (!dir.exists(outputFolder)) {
    dir.create(outputFolder)
  }
  tempDir <- databaseId
  tempDirCreated <- FALSE
  if (!dir.exists(tempDir)) {
    dir.create(tempDir)
    tempDirCreated <- TRUE
  }
  lapply(names(resultList), FUN = function(checkResultName) {
    checkResult <- resultList[[checkResultName]]
    checkResult <- dplyr::bind_cols(databaseId = databaseId, checkResult)
    utils::write.csv(checkResult,
                     file = file.path(
                       tempDir,
                       paste0(checkResultName, ".csv")
                     ),
                     row.names = FALSE
    )
  })
  zip::zip(zipfile = file.path(outputFolder, paste0(databaseId, ".zip")),
           files = list.files(tempDir, full.names = TRUE))
  if (tempDirCreated) {
    unlink(tempDir, recursive = TRUE)
  }
}

