#' Title
#'
#' @param workTable is the motherTable or babyTable
#'
#' @return returns a table with the dates checks
#' @export
#'
#' @examples
summariseGestationalAge <- function(
    workTable
)
  {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(workTable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  records <- workTable %>%
    dplyr::select(
      "gestational_length_in_day",
      "pregnancy_start_date",
      "pregnancy_end_date")


    records <- records %>% dplyr::mutate(
      n = dplyr::if_else((!is.na(.data$gestational_length_in_day) &
                           !is.na(.data$pregnancy_start_date) &
                           !is.na(.data$pregnancy_end_date)), dplyr::if_else(.data$gestational_length_in_day ==
                           !!CDMConnector::datediff("pregnancy_start_date", "pregnancy_end_date", interval = "day"), 0, 1,missing = NULL),
      NA,missing=NULL),
      endBeforeStart = dplyr::if_else((.data$pregnancy_start_date>=.data$pregnancy_end_date),1,0,missing = NULL),
      endAfterStart = dplyr::if_else((.data$pregnancy_start_date<.data$pregnancy_end_date),1,0,missing = NULL)) %>% dplyr::collect()


    records_n <- records %>%
    dplyr::summarise(
                     different_gestationalAge = sum(.data$n, na.rm = T),

                     match_gestationalAge = sum(.data$n == 0, na.rm = T),
# there should not be NAs
                     missing_information = sum(is.na(.data$n)),

                     endBeforeStart = sum(.data$endBeforeStart, na.rm =T),

                     endAfterStart = sum(.data$endAfterStart, na.rm =T))


    records_prop <- records_n %>%
      dplyr::summarise(
                       different_gestationalAge = round(.data$different_gestationalAge / nrow(tibble::as_tibble(workTable)),3)*100,

                       match_gestationalAge = round(.data$match_gestationalAge / nrow(tibble::as_tibble(workTable)),3)*100,

                       missing_information = round(.data$missing_information / nrow(tibble::as_tibble(workTable)),3)*100,

                       endBeforeStart = round(.data$endBeforeStart / nrow(tibble::as_tibble(workTable)),3)*100,

                       endAfterStart = round(.data$endAfterStart / nrow(tibble::as_tibble(workTable)),3)*100)



    records_n <- tibble::as_tibble(reshape2::melt(records_n,variable.names="variable",value.name = "count"))
    records_prop <- tibble::as_tibble(reshape2::melt(records_prop,variable.names="variable",value.name = "proportionInPercentage"))

    records_long <- records_n %>% dplyr::left_join(records_prop, by = "variable")  %>% dplyr::mutate(Total = nrow(tibble::as_tibble(workTable)))


    records <-  NULL
    records_n <- NULL
    records_prop <- NULL

  return(records_long)
}
