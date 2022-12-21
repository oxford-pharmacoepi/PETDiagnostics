#' Title
#'
#' @param motherTable is the motherTable
#'
#' @return returns a table with the number of annual pregnancies
#' @export
#'
#' @examples
getAnnualOverview <- function(
    motherTable
)
{

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(motherTable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)


    records <- motherTable %>%
      dplyr::select(
        "pregnancy_id",
        "pregnancy_end_date"
      ) %>% dplyr::collect()

    records <- records %>%
      dplyr::mutate(
        year = format(.data$pregnancy_end_date, "%Y")
      )   %>% dplyr::group_by(.data$year)  %>%
      dplyr::summarise(
       pregnancies = dplyr::n_distinct(.data$pregnancy_id)
      )

  return(records)

}
