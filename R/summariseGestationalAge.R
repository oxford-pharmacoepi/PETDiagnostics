#' Title
#'
#' @param worktable is the mothertable
#' @param minGestAge_Days is minimum number of days of gestational age for which we test whether the end was before (start + minGestAge_Days)
#'
#' @return returns a table with the dates checks
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' summariseGestationalAge(cdm$mothertable, minGestAge_Days = 28)
#' }
summariseGestationalAge <- function(
    worktable,
    minGestAge_Days
)
  {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(worktable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  records <- worktable %>%
    dplyr::select(
      "gestational_length_in_day",
      "pregnancy_start_date",
      "pregnancy_end_date")


    records <- records %>% dplyr::mutate(
      n = dplyr::if_else((!is.na(.data$gestational_length_in_day) &
                           !is.na(.data$pregnancy_start_date) &
                           !is.na(.data$pregnancy_end_date)), dplyr::if_else(dplyr::between(!!CDMConnector::datediff("pregnancy_start_date", "pregnancy_end_date", interval = "day"),
                                                                                     .data$gestational_length_in_day -7, .data$gestational_length_in_day +7),0, 1),
      NA),
      endBeforeStart = dplyr::if_else(((!!CDMConnector::dateadd("pregnancy_start_date", minGestAge_Days, interval = "day"))>=.data$pregnancy_end_date),1,0),
      endAfterStart = dplyr::if_else(((!!CDMConnector::dateadd("pregnancy_start_date", minGestAge_Days, interval = "day"))<.data$pregnancy_end_date),1,0)) %>%
        dplyr::collect()


    records_n <- records %>%
    dplyr::summarise(
                     different_gestationalAge = sum(.data$n, na.rm = T),

                     match_gestationalAge = sum(.data$n == 0, na.rm = T),

                     missing_information = sum(is.na(.data$n)),

                     endBeforeMinGestAge = sum(.data$endBeforeStart, na.rm =T),

                     endAfterMinGestAge = sum(.data$endAfterStart, na.rm =T))


    records_prop <- records_n %>%
      dplyr::summarise(
                       different_gestationalAge = round(.data$different_gestationalAge / nrow(tibble::as_tibble(worktable)),3)*100,

                       match_gestationalAge = round(.data$match_gestationalAge / nrow(tibble::as_tibble(worktable)),3)*100,

                       missing_information = round(.data$missing_information / nrow(tibble::as_tibble(worktable)),3)*100,

                       endBeforeMinGestAge = round(.data$endBeforeMinGestAge / nrow(tibble::as_tibble(worktable)),3)*100,

                       endAfterMinGestAge = round(.data$endAfterMinGestAge / nrow(tibble::as_tibble(worktable)),3)*100) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::rename(variable = name,
                    percentage = value)



    records_n <-  records_n %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::rename(variable = name,
                    count = value)

    records_long <- records_n %>% dplyr::left_join(records_prop, by = "variable")  %>% dplyr::mutate(Total = nrow(tibble::as_tibble(worktable)))


    records <-  NULL
    records_n <- NULL
    records_prop <- NULL

  return(records_long)
}
