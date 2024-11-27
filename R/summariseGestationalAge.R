#' tells us how pregnancy dates and gestational age add up in the mothertable
#'
#' @param worktable is the mothertable
#' @param minGestAge_Days is minimum number of days of gestational age for which we test whether the end was before (start + minGestAge_Days)
#'
#' @return returns a table with the dates checks
#' @export
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
      "pregnancy_end_date") %>%
    dplyr::mutate(pregnancy_start_date = as.Date(.data$pregnancy_start_date),
                  pregnancy_end_date = as.Date(.data$pregnancy_end_date))


  records <- records %>%
    dplyr::mutate(
      n = dplyr::case_when(
        !is.na(.data$gestational_length_in_day) &
          !is.na(.data$pregnancy_start_date) &
          !is.na(.data$pregnancy_end_date) ~
          dplyr::if_else(
            !!CDMConnector::datediff("pregnancy_start_date", "pregnancy_end_date", interval = "day") >= .data$gestational_length_in_day - 7 &
              !!CDMConnector::datediff("pregnancy_start_date", "pregnancy_end_date", interval = "day") <= .data$gestational_length_in_day + 7,
            1,
            0
          ),
        TRUE ~ NA_real_
      ),
      endBeforeStart = dplyr::if_else(
        !!CDMConnector::dateadd("pregnancy_start_date", minGestAge_Days, interval = "day") >= .data$pregnancy_end_date,
        1,
        0
      ),
      endAfterStart = dplyr::if_else(
        !!CDMConnector::dateadd("pregnancy_start_date", minGestAge_Days, interval = "day") < .data$pregnancy_end_date,
        1,
        0
      )
    )

  records_n <- records %>%
    dplyr::summarise(
      different_gestationalAge = sum(.data$n, na.rm = TRUE),
      match_gestationalAge = sum(ifelse(.data$n == 0, 1, 0), na.rm = TRUE),
      missing_information = sum(ifelse(is.na(.data$n), 1, 0), na.rm = TRUE),
      endBeforeMinGestAge = sum(.data$endBeforeStart, na.rm = TRUE),
      endAfterMinGestAge = sum(.data$endAfterStart, na.rm = TRUE)
    )


  total_records <- nrow(tibble::as_tibble(records))

  records_prop <- records_n %>%
    dplyr::mutate(
      different_gestationalAge = round(.data$different_gestationalAge / total_records, 3) * 100,
      match_gestationalAge = round(.data$match_gestationalAge / total_records, 3) * 100,
      missing_information = round(.data$missing_information / total_records, 3) * 100,
      endBeforeMinGestAge = round(.data$endBeforeMinGestAge / total_records, 3) * 100,
      endAfterMinGestAge = round(.data$endAfterMinGestAge / total_records, 3) * 100
    ) %>%
    tidyr::pivot_longer(
      cols = tidyr::everything(),
      names_to = "variable",
      values_to = "percentage"
    )

    records_n <-  records_n  %>%
      tidyr::pivot_longer(
        cols = tidyr::everything(),
        names_to = "variable",
        values_to = "count"
      )

    records_long <- records_n %>% dplyr::left_join(records_prop, by = "variable")  %>% dplyr::mutate(total = total_records)


    records <-  NULL
    records_n <- NULL
    records_prop <- NULL

  return(records_long)
}
