#' tells us what the minimum and maximum of pregnancy start and end dates as well as the distribution of gestational age are
#'
#' @param mothertable is the mothertable
#'
#' @return returns a table with the distrubtion of pregnancy starte date, end date, and gestational Age
#' @export
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' getValueDatesAgeDist(cdm$mothertable)
#' }
getValueDatesAgeDist <- function(
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
      "pregnancy_start_date",
      "pregnancy_end_date"
    ) %>% dplyr::summarise(
      min_start = min(.data$pregnancy_start_date, na.rm=T),
      max_start = max(.data$pregnancy_start_date, na.rm = T),
      min_end = min(.data$pregnancy_end_date, na.rm=T),
      max_end = max(.data$pregnancy_end_date, na.rm = T)
    ) %>% dplyr::collect() %>% tidyr::pivot_longer(cols = tidyr::everything()) %>%
    dplyr::rename(variable = name,
                  value = value)

  records$value <- as.character(records$value)

  records2 <- mothertable %>%
    dplyr::select(
      "gestational_length_in_day"
    )  %>% dplyr::collect() %>% dplyr::summarise(
      min_gestationalAge_inDays = min(.data$gestational_length_in_day, na.rm=T),
      q01_gestationalAge_inDays = stats::quantile(.data$gestational_length_in_day,0.01, na.rm = T),
      q05_gestationalAge_inDays = stats::quantile(.data$gestational_length_in_day,0.05, na.rm = T),
      q10_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.10, na.rm = T
      ),
      q15_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.15, na.rm = T
      ),
      q20_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.20, na.rm = T
      ),
      q25_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.25, na.rm = T
      ),
      q30_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.30, na.rm = T
      ),
      q35_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.35, na.rm = T
      ),
      q40_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.40, na.rm = T
      ),
      q45_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.45, na.rm = T
      ),
      median_gestationalAge_inDays = stats::median(.data$gestational_length_in_day, na.rm = T),
      q55_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.55, na.rm = T
      ),
      q60_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.60, na.rm = T
      ),
      q70_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.70, na.rm = T
      ),
      q75_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.75, na.rm = T
      ),
      q80_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.80, na.rm = T
      ),
      q85_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.85, na.rm = T
      ),
      q90_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.90, na.rm = T
      ),
      q95_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.95, na.rm = T
      ),
      q99_gestationalAge_inDays = stats::quantile(
        .data$gestational_length_in_day,
        0.99, na.rm = T
      ),
      max_gestationalAge_inDays = max(.data$gestational_length_in_day, na.rm = T)
    ) %>% tidyr::pivot_longer(cols = tidyr::everything()) %>%
    dplyr::rename(variable = name,
                  value = value)

  records2$value <- as.character(records2$value)


  records_bound <- rbind(records,records2)


  return(records_bound)

}
