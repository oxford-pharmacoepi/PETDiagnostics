#' Title
#'
#' @param motherTable is the motherTable
#'
#' @return returns a table with the distrubtion of pregnancy starte date, end date, and gestational Age
#' @export
#'
#' @examples
getValueDatesAgeDist <- function(
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
      "pregnancy_start_date",
      "pregnancy_end_date"
    ) %>% dplyr::summarise(
      min_start = min(.data$pregnancy_start_date, na.rm=T),
      max_start = max(.data$pregnancy_start_date, na.rm = T),
      min_end = min(.data$pregnancy_end_date, na.rm=T),
      max_end = max(.data$pregnancy_end_date, na.rm = T)
    ) %>% dplyr::collect()

  records <- tibble::as_tibble(reshape2::melt(records,variable.names="variable",value.name = "value"))
  records$value <- as.character(records$value)

  records2 <- motherTable %>%
    dplyr::select(
      "gestational_length_in_day"
    )  %>% dplyr::collect() %>% dplyr::summarise(
      min_gestationalAge_inDays = min(.data$gestational_length_in_day, na.rm=T),
      q05_gestationalAge_inDays = stats::quantile(.data$gestational_length_in_day,0.05, na.rm = T),
      q10_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.10, na.rm = T
      ),
      q25_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.25, na.rm = T
      ),
      median_gestationalAge_inDays = stats::median(.data$gestational_length_in_day, na.rm = T),
      q75_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.75, na.rm = T
      ),
      q90_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.90, na.rm = T
      ),
      q95_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.95, na.rm = T
      ),
      max_gestationalAge_inDays = max(.data$gestational_length_in_day, na.rm = T)
    )


  records2 <- tibble::as_tibble(reshape2::melt(records2,variable.names="variable",value.name = "value"))
  records2$value <- as.character(records2$value)


  records_bound <- rbind(records,records2)


  return(records_bound)

}
