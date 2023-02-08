#' Title
#'
#' @param motherTable input a table according to the motherTable of the pregnancy extension Table in OHDSI, can be NULL
#' @param babyTable input a table according to the motherTable of the pregnancy extension Table in OHDSI, can be NULL
#' @param checks chose the checks you want to perform, not all checks are possible depending on which tables are available
#' @param minCellCount chose a number below you want to obscure counts, 0 is not obscured
#' @param minGestAge_Days chose a number below you think that the pregnancy entries were wrong / should not be there
#' @param verbose will give you the information the package is performing at that moment
#'
#' @return all the individual result tables with data obscured if need be
#' @export
#'
#' @examples
executeChecks <- function(#cdm,
                          motherTable = NULL,
                          babyTable = NULL,
                          checks = c("overview","annualOverview","missing", "unknown","gestationalAge","datesAgeDist","outcomeMode", "fetusesLiveborn",
                                     "fetusid","weightDist","bitSet"),
                          minCellCount = 5,
                          minGestAge_Days = 21,
                          verbose = FALSE) {

  errorMessage <- checkmate::makeAssertCollection()
  # only as soon as these are part of the cdm
  # checkDbType(cdm = cdm, type = "cdm_reference", messageStore = errorMessage)
  # checkTableExists(cdm = cdm, tableName = motherTable,
  #                  messageStore = errorMessage)
  # checkTableExists(cdm = cdm, tableName = babyTable,
  #                  messageStore = errorMessage)
  checkmate::assertTRUE(is.null(motherTable) || inherits(motherTable, 'tbl_dbi'), add = errorMessage)
  checkmate::assertTRUE(is.null(babyTable) || inherits(babyTable, 'tbl_dbi'), add = errorMessage)

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
    if (!is.null(motherTable)) {

      PETOverviewMother <- getOverview(motherTable) %>% dplyr::collect()
    }
    if (!is.null(babyTable)) {

      PETOverviewBaby <- getOverview(babyTable) %>% dplyr::collect()
    }
  }



  AnnualPETOverviewMother <- NULL
  if ("annualOverview" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: total number of women, pregnancies (and fetuses) per year", start)
    }
    if (!is.null(motherTable)) {

      AnnualPETOverviewMother <- getAnnualOverview(motherTable) %>% dplyr::collect()
    }

  }


  missingSummaryMother <- NULL
  missingSummaryBaby <- NULL
  if ("missing" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check Missing of all variables", start)
    }
    if (!is.null(motherTable)) {

    missingSummaryMother <- getMissings(motherTable) %>% dplyr::collect()
    }
  if (!is.null(babyTable)) {

    missingSummaryBaby <- getMissings(babyTable) %>% dplyr::collect()
  }
  }



  unknownSummaryMother <- NULL
  if ("unknown" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check unknowns of required variables", start)
    }
    if (!is.null(motherTable)) {

      unknownSummaryMother <- getUnknown(motherTable) %>% dplyr::collect()
    }

  }



  gestationalAgeMatch <-  NULL
  if ("gestationalAge" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check Gestational Age", start)
    }
    if (!is.null(motherTable)) {

  gestationalAgeMatch <- summariseGestationalAge(motherTable,minGestAge_Days) %>% dplyr::collect()
    }
  }




  valueDatesAgeDist <-  NULL
  if ("datesAgeDist" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check values of dates and Gestational Age", start)
    }
    if (!is.null(motherTable)) {

      valueDatesAgeDist <- getValueDatesAgeDist(motherTable) %>% dplyr::collect()
    }
  }




  outcomeModeMatch <-  NULL
  if ("outcomeMode" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check Outcome and Mode of Delivery", start)
    }
    if (!is.null(motherTable)) {

   outcomeModeMatch <- checkOutcomeMode(motherTable) %>% dplyr::collect()
    }
  }



  fetusesLivebornNumber <-  NULL
  if ("fetusesLiveborn" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check number of fetuses versus liveborn", start)
    }
# pregnancy_single is a required variable
    if ("pregnancy_number_fetuses" %in% colnames(motherTable) && "pregnancy_number_liveborn" %in% colnames(motherTable)) {

   fetusesLivebornNumber <- tibble::as_tibble(checkFetusesLiveborn(motherTable)) %>% dplyr::collect()
    }
  }




  fetusIdMatch <- NULL
  if ("fetusid" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check number of fetuses versus liveborn", start)
    }
    if (!is.null(motherTable) && !is.null(babyTable)) {

   fetusIdMatch <- checkFetusId(motherTable,babyTable) %>% dplyr::collect()
      }

  }



  valueWeightDist <- NULL
  if ("weightDist" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check values of birthweight", start)
    }
    if (!is.null(babyTable)) {

      valueWeightDist <- getValueWeightDist(babyTable) %>% dplyr::collect()
    }

  }



  bitSetOverviewAll <- NULL
  bitSetOverviewMother <- NULL
  bitSetOverviewBaby <- NULL
  if ("bitSet" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check missing/unknown data pattern", start)
    }
    if (!is.null(motherTable) && !is.null(babyTable)) {

      bitSetOverviewAll  <- getBitSet(motherTable,babyTable) %>% dplyr::collect()
    }
    if (!is.null(motherTable)) {

        bitSetOverviewMother  <- getBitSet(motherTable, babyTable = NULL) %>% dplyr::collect()
      }
    if (!is.null(babyTable)) {

        bitSetOverviewBaby  <- getBitSet(motherTable = NULL, babyTable) %>% dplyr::collect()
      }
    }



   if (verbose == TRUE) {
    start <- printDurationAndMessage("Finished", start)
  }



  if (!is.null(motherTable) && !is.null(babyTable)) {

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

  } else if  (!is.null(motherTable)) {

    result <- list("PETOverviewMother" = PETOverviewMother,
                   "AnnualPETOverviewMother" = AnnualPETOverviewMother,
                   "missingSummaryMother" = missingSummaryMother,
                   "unknownSummaryMother" = unknownSummaryMother,
                   "gestationalAgeMatch" = gestationalAgeMatch,
                   "valueDatesAgeDist" =  valueDatesAgeDist,
                   "outcomeModeMatch" = outcomeModeMatch,
                   "fetusesLivebornNumber" = fetusesLivebornNumber,
                   "bitSetOverviewMother" =  bitSetOverviewMother)

  }   else  if  (!is.null(babyTable)) {

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
#'
#' @export
#'
#' @examples
#'
#'
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

