#' Title
#'
#' @param worktable is the mothertable
#'
#' @return returns a table with the information of differences in outcome mode and delivery data
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' checkOutcomeMode(cdm$mothertable)
#' }
checkOutcomeMode <- function(
    worktable
) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(worktable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  records <- worktable %>%
    dplyr::select(
     "pregnancy_outcome",
     "pregnancy_mode_delivery")

  #check if miscarriage or TOP has vaginal or c-section delivery
   records <- records %>% dplyr::mutate(
    n = dplyr::if_else(.data$pregnancy_outcome !=0 , dplyr::if_else(
      (.data$pregnancy_outcome == 4067106 | .data$pregnancy_outcome == 4081422)
                           & (.data$pregnancy_mode_delivery ==4015701),1,0),NA)) %>%
     dplyr::collect()

  records_n <- records %>%
    dplyr::summarise(
                     no_match = sum(.data$n, na.rm = T),

                     match = sum(.data$n==0, na.rm = T),

                     missingUnknown_information = sum(is.na(.data$n)),

                     )
  records_prop <- records_n %>%
    dplyr::summarise(

      no_match = round(.data$no_match / nrow(tibble::as_tibble(worktable)),3)*100,

      match = round(.data$match / nrow(tibble::as_tibble(worktable)),3)*100,

      missingUnknown_information = round(.data$missingUnknown_information /nrow(tibble::as_tibble(worktable)),3)*100) %>%
    tidyr::pivot_longer(cols = everything()) %>%
    dplyr::rename(variable = name,
                  percentage = value)


  records_n <- records_n %>% tidyr::pivot_longer(cols = everything()) %>%
    dplyr::rename(variable = name,
                  count = value)

records_long <- records_n %>% dplyr::left_join(records_prop, by = "variable")  %>% dplyr::mutate(Total = nrow(tibble::as_tibble(worktable)))


records <-  NULL
records_n <- NULL
records_prop <- NULL



  return(records_long)

}
