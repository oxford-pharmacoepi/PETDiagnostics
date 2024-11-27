#' tells us how the missing/unknown data pattern of both tables, mother or baby table looks like (depending on the input)
#'
#' @param mothertable is the mothertable
#' @param babytable is the babytable
#'
#' @return returns a table with the pattern of missing data
#' @export
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' getBitSet(cdm$mothertable,cdm$babytable)
#' }
getBitSet <- function(
    mothertable = NULL,
    babytable = NULL
) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(is.null(mothertable) || inherits(mothertable, 'tbl_dbi'), add = errorMessage)
  checkmate::assertTRUE(is.null(babytable) || inherits(babytable, 'tbl_dbi'), add = errorMessage)

  checkmate::reportAssertions(collection = errorMessage)



  if (!is.null(mothertable) && !is.null(babytable)) {

        recordshelp <- mothertable %>%
      dplyr::select(
        "pregnancy_number_fetuses",
        "pregnancy_single",
        "pregnancy_start_date",
        "pregnancy_end_date",
        "gestational_length_in_day",
        "pregnancy_id",
        "pregnancy_outcome",
        "pregnancy_number_liveborn",
        "pregnancy_mode_delivery"
      ) %>%
      dplyr::left_join((dplyr::select(babytable,"fetus_id",
                                      "pregnancy_id",
                                      "birth_outcome",
                                      "birth_weight",
                                      "birth_con_malformation",
                                      "birth_sga",
                                      "birth_fgr",
                                      "birth_apgar")),by = "pregnancy_id")  %>%
      dplyr::collect()

    #set required variables with 0 to missing because missing data pattern function expect a missing
    #a zero is equal to a missing, because we do not know the information
    recordshelp[recordshelp==0]=NA

    records <- misty::na.pattern(recordshelp,order = TRUE, digits = 1, as.na = NULL, write = NULL,
                                 check = TRUE, output = FALSE)

    recordshelp <- NULL

  } else {
    if (!is.null(mothertable)){

      recordshelp <- mothertable %>%
        dplyr::select(
          "pregnancy_number_fetuses",
          "pregnancy_single",
          "pregnancy_start_date",
          "pregnancy_end_date",
          "gestational_length_in_day",
          "pregnancy_id",
          "pregnancy_outcome",
          "pregnancy_number_liveborn",
          "pregnancy_mode_delivery"
        ) %>%  dplyr::collect()

      #set required variables with 0 to missing because missing data pattern function expect a missing
      #a zero is equal to a missing, because we do not know the information
      recordshelp[recordshelp==0]=NA

      records <- misty::na.pattern(recordshelp,order = TRUE, digits = 1, as.na = NULL, write = NULL,
                                   check = TRUE, output = FALSE)

      recordshelp <- NULL

    }
    if (!is.null(babytable)){

      recordshelp <- babytable %>%
        dplyr::select(
          "fetus_id",
          "pregnancy_id",
          "birth_outcome",
          "birth_weight",
          "birth_con_malformation",
          "birth_sga",
          "birth_fgr",
          "birth_apgar"
        ) %>%
        dplyr::collect()

      records <- misty::na.pattern(recordshelp,order = TRUE, digits = 1, as.na = NULL, write = NULL,
                                   check = TRUE, output = FALSE)

      recordshelp <- NULL

    }
  }

  return(tibble::as_tibble(records$result))
}
