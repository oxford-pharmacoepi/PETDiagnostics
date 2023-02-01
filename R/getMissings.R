#' Title
#'
#' @param workTable is the motherTable or babyTable
#'
#' @return returns a table with all the missings
#' @export
#'
#' @examples
getMissings <- function(
                        workTable
                        ) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(workTable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  workTable <- workTable %>% dplyr::collect()

  n_missing <- tibble::tibble(.rows=1)
  prop_missing <- tibble::tibble(.rows=1)

  #loop through all columnnames and sum the missings
  for (i in colnames(workTable)){

           n_missing[i] <- sum(as.integer(is.na(workTable[i]), na.rm = TRUE))
           prop_missing[i] <- round(n_missing[i]/nrow(workTable),3)*100

  }
# all the required variables do not have missings, they have "0", whereas non-required variables have NAs

  n_missing_long <-  tibble::as_tibble(reshape2::melt(n_missing, variable.names="variable",value.name = "count"))
  prop_missing_long <- tibble::as_tibble(reshape2::melt(prop_missing, variable.names="variable",value.name = "Percentage"))


  summMissings <- n_missing_long %>% dplyr::left_join(prop_missing_long, by = "variable")  %>% dplyr::mutate(Total = nrow(workTable))


  n_missing_long <-  NULL
  prop_missing_long <- NULL
  n_missing <-  NULL
  prop_missing <- NULL


  return(summMissings)
}
