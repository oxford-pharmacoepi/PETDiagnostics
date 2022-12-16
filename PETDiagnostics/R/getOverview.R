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
        ) %>% dplyr::collect()

      records <- tibble::as_tibble(reshape2::melt(records,variable.names="variable",value.name = "count"))


  } else {


    records <- workTable %>%
      dplyr::select(
        "person_id",
        "pregnancy_id"
        ) %>%
      dplyr::summarise(
        women = dplyr::n_distinct(.data$person_id),
        pregnancies = dplyr::n_distinct(.data$pregnancy_id)
      ) %>% dplyr::collect()

    records <- tibble::as_tibble(reshape2::melt(records,variable.names="variable",value.name = "count"))

  }

return(records)

}
