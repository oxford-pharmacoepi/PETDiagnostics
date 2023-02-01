#' Title
#'
#' @param motherTable is the motherTable
#'
#' @return returns a table with all the zero counts of required variables in the motherTable
#' @export
#'
#' @examples
getUnknown <- function(
   motherTable
) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(motherTable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)

  motherTable <- motherTable %>% dplyr::collect()

  n_unknown <- tibble::tibble(.rows=1)
  prop_unknown <- tibble::tibble(.rows=1)

  checkCol <- c("pregnancy_id","person_id","pregnancy_start_date","pregnancy_end_date","gestational_length_in_day","pregnancy_outcome","pregnancy_mode_delivery","pregnancy_single")

  #loop through the required columns and count the zeros
  for (i in 1:length(checkCol)){

    n_unknown[[checkCol[i]]]<-sum(as.integer(motherTable[[checkCol[i]]]==0), na.rm = TRUE)
    prop_unknown[[checkCol[i]]] <- round(n_unknown[[checkCol[i]]]/nrow(motherTable),3)*100

  }

  n_unknown_long <-  tibble::as_tibble(reshape2::melt(n_unknown, variable.names="variable",value.name = "count"))
  prop_unknown_long <- tibble::as_tibble(reshape2::melt(prop_unknown, variable.names="variable",value.name = "Percentage"))


  summUnknowns <- n_unknown_long %>% dplyr::left_join(prop_unknown_long, by = "variable")  %>% dplyr::mutate(Total = nrow(motherTable))


  n_unknown_long <-  NULL
  prop_unknown_long <- NULL
  n_unknown <-  NULL
  prop_unknown <- NULL


  return(summUnknowns)
}
