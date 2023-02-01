#' Title
#'
#' @param motherTable is the motherTable
#' @param babyTable is the babyTable
#'
#' @return returns a table with the pattern of missing data
#' @export
#'
#' @examples
getBitSet <- function(
    motherTable = NULL,
    babyTable = NULL
) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(is.null(motherTable) || inherits(motherTable, 'tbl_dbi'), add = errorMessage)
  checkmate::assertTRUE(is.null(babyTable) || inherits(babyTable, 'tbl_dbi'), add = errorMessage)

  checkmate::reportAssertions(collection = errorMessage)



  if (!is.null(motherTable) && !is.null(babyTable)) {

        recordshelp <- motherTable %>%
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
      dplyr::left_join((dplyr::select(babyTable,"fetus_id",
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
    if (!is.null(motherTable)){

      recordshelp <- motherTable %>%
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
    if (!is.null(babyTable)){

      recordshelp <- babyTable %>%
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
