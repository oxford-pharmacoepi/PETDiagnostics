#' Title
#'
#' @param worktable is the mothertable or babytable
#'
#' @return returns a table with all the missings
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' getMissings(cdm$babytable)
#' }
getMissings <- function(
                        worktable
                        ) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(worktable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  worktable <- worktable %>% dplyr::collect()

  n_missing <- tibble::tibble(.rows=1)
  prop_missing <- tibble::tibble(.rows=1)

  #loop through all columnnames and sum the missings
  for (i in colnames(worktable)){

           n_missing[i] <- sum(as.integer(is.na(worktable[i]), na.rm = TRUE))
           prop_missing[i] <- round(n_missing[i]/nrow(worktable),3)*100

  }
# all the required variables do not have missings, they have "0", whereas non-required variables have NAs

  n_missing_long <-  n_missing %>% tidyr::pivot_longer(cols = everything()) %>%
    dplyr::rename(variable = name,
                  count = value)
  prop_missing_long <-  prop_missing %>% tidyr::pivot_longer(cols = everything()) %>%
    dplyr::rename(variable = name,
                  percentage = value)


  summMissings <- n_missing_long %>% dplyr::left_join(prop_missing_long, by = "variable")  %>% dplyr::mutate(Total = nrow(worktable))


  n_missing_long <-  NULL
  prop_missing_long <- NULL
  n_missing <-  NULL
  prop_missing <- NULL


  return(summMissings)
}
