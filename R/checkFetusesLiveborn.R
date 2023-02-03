#' Title
#'
#' @param workTable is the motherTable or babyTable
#'
#' @return returns a table with the checks on fetuses and liveborns
#' @export
#'
#' @examples
checkFetusesLiveborn <- function(
    workTable
) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(workTable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)


  records <- workTable %>%
    dplyr::select(
      "pregnancy_number_fetuses",
      "pregnancy_number_liveborn",
      "pregnancy_single"
      )

  #check if number of fetuses is equal or larger to number of liveborn
  #check if single / multiple pregnancy adds up with fetus number
  records <- records %>% dplyr::mutate(
    numberNotRight = dplyr::if_else((.data$pregnancy_number_fetuses) >= (.data$pregnancy_number_liveborn),0,1),

    multipleWrong = dplyr::if_else((.data$pregnancy_number_fetuses > 1 &  .data$pregnancy_single == 4188539) |
                                        (.data$pregnancy_number_fetuses == 1 &  .data$pregnancy_single == 4188540),1,0),

    multipleRight = dplyr::if_else((.data$pregnancy_number_fetuses >1 &  .data$pregnancy_single == 4188540 )|
                                     (.data$pregnancy_number_fetuses == 1 &  .data$pregnancy_single == 4188539),1,0)) %>%
    dplyr::collect()


  records_n <- records %>%
    dplyr::summarise(
                     multipleWrong = sum(.data$multipleWrong, na.rm = T),

                     multipleRight = sum(.data$multipleRight, na.rm = T),

                     missing_multiple = sum(as.integer(is.na((.data$pregnancy_number_fetuses) | is.na(.data$pregnancy_single)) |
                                                  (is.na(.data$pregnancy_number_fetuses) & is.na(.data$pregnancy_single)))),

                     relativeNumberWrong = sum(.data$numberNotRight, na.rm = T),

                     relativeNumberRight = sum(.data$numberNotRight == 0, na.rm = T),

                     missing_relativeNumber = sum(is.na(.data$numberNotRight))

       )


  records_prop <- records_n %>%
    dplyr::summarise(
                     multipleWrong = round(.data$multipleWrong / nrow(tibble::as_tibble(workTable)),3)*100,

                     multipleRight = round(.data$multipleRight / nrow(tibble::as_tibble(workTable)),3)*100,

                     missing_multiple = round(.data$missing_multiple / nrow(tibble::as_tibble(workTable)),3)*100,

                     relativeNumberWrong = round(.data$relativeNumberWrong / nrow(tibble::as_tibble(workTable)),3)*100,

                     relativeNumberRight = round(.data$relativeNumberRight / nrow(tibble::as_tibble(workTable)),3)*100,

                     missing_relativeNumber = round(.data$missing_relativeNumber / nrow(tibble::as_tibble(workTable)),3)*100


    )

  records_n <- tibble::as_tibble(reshape2::melt(records_n,variable.names="variable",value.name = "count"))
  records_prop <- tibble::as_tibble(reshape2::melt(records_prop,variable.names="variable",value.name = "Percentage"))

  records_long <- records_n %>% dplyr::left_join(records_prop, by = "variable")  %>% dplyr::mutate(Total = nrow(tibble::as_tibble(workTable)))


  records <-  NULL
  records_n <- NULL
  records_prop <- NULL

  return(records_long)
}
