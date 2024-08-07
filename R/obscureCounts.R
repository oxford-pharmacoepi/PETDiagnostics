obscureCounts <- function(table,
                          tableName,
                          minCellCount = 5,
                          substitute = NA) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_tibble(table, add = errorMessage)
  checkmate::assertTRUE(is.numeric(minCellCount) || is.null(minCellCount), add = errorMessage)
  checkmate::assertTRUE(is.numeric(substitute) || is.na(substitute), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  if (!is.null(minCellCount)) {
    # initialise result_obscured as FALSE
    table$result_obscured <- "FALSE"


    colNames <- setdiff(colnames(table), c("value","variable","year","result_obscured"))
    checkColNames <- NULL
    checkColNames <- colNames[!grepl("variable|year|value|Percentage", colNames)]
    toBeSubstituted <- colNames[grepl("count|Percentage", colNames)]


    # if any count in the specified columns is less than minCellCount but bigger than 0, replace the value with substitute
    if (!is.null(checkColNames)) {
      table <- table %>%
        dplyr::rowwise() %>%
        dplyr::mutate(result_obscured = any(dplyr::across(tidyr::all_of(checkColNames), ~ (. < minCellCount & . > 0)))) %>%
        dplyr::mutate_at(dplyr::vars(tidyr::all_of(toBeSubstituted)), ~ ifelse(result_obscured, substitute, .))
    }
  }

  return(table)
}
