#' Title
#'
#' @param mothertable is the mothertable
#'
#' @return returns a table with all the zero counts of required variables in the mothertable
#' @export
#'
#' @examples
#' \donttest{
#' cdm <- mockPregnancy()
#' getUnknown(cdm$mothertable)
#' }
getUnknown <- function(
   mothertable
) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(mothertable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  mothertable <- mothertable %>% dplyr::collect()

  n_unknown <- tibble::tibble(.rows=1)
  prop_unknown <- tibble::tibble(.rows=1)

  checkCol <- c("pregnancy_id","person_id","pregnancy_start_date","pregnancy_end_date","gestational_length_in_day","pregnancy_outcome","pregnancy_mode_delivery","pregnancy_single")

  #loop through the required columns and count the zeros
  for (i in 1:length(checkCol)){

    n_unknown[[checkCol[i]]]<-sum(as.integer(mothertable[[checkCol[i]]]==0), na.rm = TRUE)
    prop_unknown[[checkCol[i]]] <- round(n_unknown[[checkCol[i]]]/nrow(mothertable),3)*100

  }


  n_unknown_long <-  n_unknown %>% tidyr::pivot_longer(cols = tidyr::everything()) %>%
    dplyr::rename(variable = name,
                  count = value)
  prop_unknown_long <-  prop_unknown %>% tidyr::pivot_longer(cols = tidyr::everything()) %>%
    dplyr::rename(variable = name,
                  percentage = value)

  summUnknowns <- n_unknown_long %>% dplyr::left_join(prop_unknown_long, by = "variable")  %>% dplyr::mutate(Total = nrow(mothertable))


  n_unknown_long <-  NULL
  prop_unknown_long <- NULL
  n_unknown <-  NULL
  prop_unknown <- NULL


  return(summUnknowns)
}
