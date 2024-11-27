#' Tells us how many women and pregnancies there are when the mother table is assessed and how many pregnancies and infants there are in case the infant table is assessed
#'
#' @param workTable is the motherTable or babyTable
#'
#' @return returns a table with the overview
#' @export
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' getOverview(cdm$babytable)
#' }
getOverview <- function(
    workTable
)
{

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(workTable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)


  if ("fetus_id" %in% colnames(workTable)){


      records <- workTable %>%
    dplyr::select(
      "pregnancy_id",
      "fetus_id"
      ) %>%
        dplyr::summarise(
          pregnancies = dplyr::n_distinct(.data$pregnancy_id),
          fetuses = dplyr::n_distinct(.data$fetus_id)
        ) %>% dplyr::collect() %>% tidyr::pivot_longer(cols = tidyr::everything()) %>%
        dplyr::rename(variable = name,
                      count = value)


  } else {


    records <- workTable %>%
      dplyr::select(
        "person_id",
        "pregnancy_id"
        ) %>%
      dplyr::summarise(
        women = dplyr::n_distinct(.data$person_id),
        pregnancies = dplyr::n_distinct(.data$pregnancy_id)
      ) %>% dplyr::collect() %>% tidyr::pivot_longer(cols = tidyr::everything()) %>%
      dplyr::rename(variable = name,
                    count = value)

  }

return(records)

}
