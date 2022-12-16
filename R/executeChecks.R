executeChecks <- function(#cdm,
                          motherTable = NULL,
                          babyTable = NULL,
                          checks = c("overview", "missing", "gestationalAge", "outcomeMode", "fetusesLiveborn",
                                     "fetusid"),
                          minCellCount = 5,
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
  checkLogical(verbose, messageStore = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)


  if ("overview" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: total number of women, pregnancies (and fetuses)", start)
    }
    if (!is.null(motherTable)) {
      PETOverviewMother <- NULL
      PETOverviewMother <- getOverview(motherTable) %>% dplyr::collect()
    }
    if (!is.null(babyTable)) {
      PETOverviewBaby <- NULL
      PETOverviewBaby <- getOverview(babyTable) %>% dplyr::collect()
    }
  }



  missingSummary <- NULL
  if ("missing" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check Missing of all variables", start)
    }
    if (!is.null(motherTable)) {
      missingSummaryMother <- NULL
    missingSummaryMother <- getMissings(motherTable) %>% dplyr::collect()
    }
  if (!is.null(babyTable)) {
    missingSummaryBaby <- NULL
    missingSummaryBaby <- getMissings(babyTable) %>% dplyr::collect()
  }
  }


  gestationalAgeMatch <-  NULL
  if ("gestationalAge" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check Gestational Age", start)
    }
    if (!is.null(motherTable)) {
  gestationalAgeMatch <- summariseGestationalAge(motherTable) %>% dplyr::collect()
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
    if ("pregnancy_number_fetuses" %in% colnames(motherTable) & "pregnancy_number_liveborn" %in% colnames(motherTable)) {
   fetusesLivebornNumber <- tibble::as_tibble(checkFetusesLiveborn(motherTable)) %>% dplyr::collect()
    }
  }


  fetusIdMatch <- NULL
  if ("fetusid" %in% checks) {
    if (verbose == TRUE) {
      start <- printDurationAndMessage("Progress: check number of fetuses versus liveborn", start)
    }
    if (!is.null(motherTable) & "fetus_id" %in% colnames(babyTable)) {

   fetusIdMatch <- checkFetusId(motherTable,babyTable) %>% dplyr::collect()
      }

  }


   if (verbose == TRUE) {
    start <- printDurationAndMessage("Finished", start)
  }



  if (!is.null(motherTable) & !is.null(babyTable)) {

  result <- list("PETOverviewMother" = PETOverviewMother,
                 "PETOverviewBaby" = PETOverviewBaby,
                 "missingSummaryMother" = missingSummaryMother,
                 "missingSummaryBaby" = missingSummaryBaby,
                 "gestationalAgeMatch" = gestationalAgeMatch,
                 "outcomeModeMatch" = outcomeModeMatch,
                 "fetusesLivebornNumber" = fetusesLivebornNumber,
                 "fetusIdMatch" = fetusIdMatch)

  } else if  (!is.null(motherTable)) {

    result <- list("PETOverviewMother" = PETOverviewMother,
                   "missingSummaryMother" = missingSummaryMother,
                   "gestationalAgeMatch" = gestationalAgeMatch,
                   "outcomeModeMatch" = outcomeModeMatch,
                   "fetusesLivebornNumber" = fetusesLivebornNumber)

  }   else  if  (!is.null(babyTable)) {

    result <- list("PETOverviewBaby" = PETOverviewBaby,
                   "missingSummaryBaby" = missingSummaryBaby)

  }


  return(Filter(Negate(is.null), sapply(names(result),
                                        FUN = function(tableName) {
                                          table <- result[[tableName]]
                                          obscureCounts(table = table,
                                                        tableName = tableName,
                                                        minCellCount = minCellCount,
                                                        substitute = NA)
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
