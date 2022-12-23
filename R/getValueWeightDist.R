#' Title
#'
#' @param babyTable is the babyTable
#'
#' @return returns values of birthweight
#' @export
#'
#' @examples
getValueWeightDist <- function(
      babyTable
) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(babyTable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)


  records <- babyTable  %>% dplyr::collect()   %>% dplyr::select(
      "birth_weight"
      ) %>% dplyr::summarise(
      min_birth_weight_in_gram = min(.data$birth_weight, na.rm=T),
      q05_birth_weight_in_gram = stats::quantile(
        .data$birth_weight,
        0.05, na.rm = T
      ),
      q10_birth_weight_in_gram = stats::quantile(
        .data$birth_weight,
        0.10, na.rm = T
      ),
      q25_birth_weight_in_gram = stats::quantile(
        .data$birth_weight,
        0.25, na.rm = T
      ),
      median_birth_weight_in_gram = stats::median(.data$birth_weight, na.rm = T),
      q75_birth_weight_in_gram = stats::quantile(
        .data$birth_weight,
        0.75, na.rm = T
      ),
      q90_birth_weight_in_gram = stats::quantile(
        .data$birth_weight,
        0.90, na.rm = T
      ),
      q95_birth_weight_in_gram = stats::quantile(
        .data$birth_weight,
        0.95, na.rm = T
      ),
      max_birth_weight_in_gram = max(.data$birth_weight, na.rm = T)
    )


  records <- tibble::as_tibble(reshape2::melt(records,variable.names="variable",value.name = "value"))

  return(records)
}
