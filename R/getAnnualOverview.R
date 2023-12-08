
#' Title
#'
#' @param mothertable is the mothertable
#'
#' @return a table which shows the number of pregnancies per year
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' getAnnualOverview(cdm$mothertable)
#' }
getAnnualOverview <- function(
    mothertable
)
{

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(mothertable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)


    records <- mothertable %>%
      dplyr::select(
        "pregnancy_id",
        "pregnancy_end_date"
      ) %>% dplyr::collect()

    records <- records %>%
      dplyr::mutate(
        year = format(.data$pregnancy_end_date, "%Y")
      )   %>% dplyr::group_by(.data$year)  %>%
      dplyr::summarise(
       count = dplyr::n_distinct(.data$pregnancy_id)
      )

  return(records)

}
