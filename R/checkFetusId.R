#' Title
#'
#' @param motherTable is the motherTable
#' @param babyTable is the babyTable
#'
#' @return returns a table with the fetuses checks
#' @export
#'
#' @examples
checkFetusId <- function(
    motherTable ,
    babyTable
) {

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  #checkDbType(cdm = cdm, messageStore = errorMessage)
  checkmate::assertTRUE(inherits(motherTable, 'tbl_dbi'), add = errorMessage)
  checkmate::assertTRUE(inherits(babyTable, 'tbl_dbi'), add = errorMessage)
  checkmate::reportAssertions(collection = errorMessage)


recordshelp <- motherTable %>%
  dplyr::select(
    "pregnancy_number_fetuses",
    "pregnancy_single",
    "pregnancy_id") %>%
  dplyr::left_join((dplyr::select(babyTable,"fetus_id","pregnancy_id")),by = "pregnancy_id")


#check if there are several fetus id if pregnancy is multiple
#check if the number of fetuses match with the count of fetus id for this pregnancy

records<- recordshelp %>% dplyr::left_join((recordshelp %>% dplyr::group_by(.data$pregnancy_id) %>% dplyr::tally ()), by= "pregnancy_id") %>% dplyr::collect()
#add fetus count to pregnancy
recordshelp <- NULL


records<- records %>% dplyr::mutate(

  single_not_align_with_noOfFetusId = ifelse(.data$pregnancy_single !=0 ,dplyr::if_else(
       (.data$n > 1  &  .data$pregnancy_single == 4188539) | (.data$n == 1  &  .data$pregnancy_single == 4188540),1,0,missing = NULL),NA),

  single_align_with_noOfFetusId = ifelse(.data$pregnancy_single !=0 , dplyr::if_else(
    (.data$n > 1  &  .data$pregnancy_single == 4188540) | (.data$n == 1  &  .data$pregnancy_single == 4188539),1,0,missing = NULL),NA),

  noOfFetus_not_align_with_noOfFetusId = dplyr::if_else((.data$pregnancy_number_fetuses  != .data$n ),1,0,missing = NULL),

  noOfFetus_align_with_noOfFetusId = dplyr::if_else((.data$pregnancy_number_fetuses  == .data$n ),1,0,missing = NULL),

  pregnancy_id = .data$pregnancy_id,
  .keep=c("used"))  %>%
  dplyr::distinct(.data$pregnancy_id, .keep_all = TRUE) %>%
  dplyr::collect()

records_n <- records %>%
  dplyr::summarise(
                   single_not_align_with_noOfFetusId = sum(.data$single_not_align_with_noOfFetusId, na.rm = T),

                   single_align_with_noOfFetusId = sum(.data$single_align_with_noOfFetusId, na.rm = T),

                   missingUnknown_single = sum(is.na(.data$pregnancy_single)),

                   noOfFetus_not_align_with_noOfFetusId = sum(.data$noOfFetus_not_align_with_noOfFetusId, na.rm = T),

                   noOfFetus_align_with_noOfFetusId = sum(.data$noOfFetus_align_with_noOfFetusId, na.rm = T),

                   missing_noOfFetus = sum(is.na(.data$pregnancy_number_fetuses))


  )

records_prop <- records_n %>%
  dplyr::summarise(

    single_not_align_with_noOfFetusId = round(.data$single_not_align_with_noOfFetusId /nrow(tibble::as_tibble(motherTable)),3)*100,

    single_align_with_noOfFetusId = round(.data$single_align_with_noOfFetusId /nrow(tibble::as_tibble(motherTable)),3)*100,

    missingUnknown_single = round(.data$missingUnknown_single /nrow(tibble::as_tibble(motherTable)),3)*100,

    noOfFetus_not_align_with_noOfFetusId = round(.data$noOfFetus_not_align_with_noOfFetusId / nrow(tibble::as_tibble(motherTable)),3)*100,

    noOfFetus_align_with_noOfFetusId = round(.data$noOfFetus_align_with_noOfFetusId / nrow(tibble::as_tibble(motherTable)),3)*100,

    missing_noOfFetus = round(.data$missing_noOfFetus /nrow(tibble::as_tibble(motherTable)),3)*100,

  )


records_n <- tibble::as_tibble(reshape2::melt(records_n,variable.names="variable",value.name = "count"))
records_prop <- tibble::as_tibble(reshape2::melt(records_prop,variable.names="variable",value.name = "proportionInPercentage"))

records_long <- records_n %>% dplyr::left_join(records_prop, by = "variable")  %>% dplyr::mutate(Total = nrow(tibble::as_tibble(motherTable)))


records <-  NULL
records_n <- NULL
records_prop <- NULL

return(records_long)
}
