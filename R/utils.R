checkDbType <- function(cdm, type = "cdm_reference", messageStore) {
  dbInheritsCheck <- inherits(cdm, type)
  checkmate::assertTRUE(dbInheritsCheck,
                        add = messageStore)
  if (!isTRUE(dbInheritsCheck)) {
    messageStore$push(glue::glue("- cdm must be a CDMConnector {type} object"))
  }
}




LowHighSampling <- function(low, high, size) {

  sample(c(low:high),size,
         replace = TRUE)
}




#' Check if given table exists in cdm.
#'
#' @param cdm CDMConnector reference object
#' @param tableName checkmate collection
#' @param messageStore the message store
#'
checkTableExists <- function(cdm, tableName, messageStore) {
  table_exists <- inherits(cdm[[tableName]], 'tbl_dbi')
  checkmate::assertTRUE(table_exists, add = messageStore)
  if (!isTRUE(table_exists)) {
    messageStore$push(glue::glue("- {tableName} is not found"))
  }
}



#
# CheckCohortTableExists <- function(cdm, tableName, messageStore) {
#   table_exists <- tableName %in% names(cdm)
#   checkmate::assertTRUE(table_exists,
#                         add = errorMessage
#   )
#   if (!isTRUE(table_exists)) {
#     messageStore$push(glue::glue("- {tableName} is not found"))
#   }
# }


#' Check if given object is a boolean.
#'
#' @param input the input
#' @param messageStore checkmate collection
#' @param null.ok if value null is allowed
#'
checkLogical <- function(input, messageStore, null.ok = TRUE) {
  checkmate::assert_logical(input,
                            add = messageStore,
                            null.ok = null.ok)
}


#' Print duration from start to now and print it as well as new status message
#'
#' @param message the message
#' @param start the start time
#'
#' @return the current time
printDurationAndMessage <- function(message, start) {
  currentTime <- Sys.time()
  duration <- abs(as.numeric(currentTime - start, units = "secs"))
  message(glue::glue("Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"))
  message(message)
  return(currentTime)
}


