test_that("check working example 1) < min cell count 2) and not 0 obscured", {
  table <- tibble::tibble(
    variable = c("women","pregnancies"),
    count = c(60,100)
  )

  result <- obscureCounts(table, "PETOverviewMother", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE))
  expect_equal(result$variable, c("women","pregnancies"))
  expect_equal(result$count, c(60,100))


  table <- tibble::tibble(
    variable = c("pregnancies","fetuses"),
    count = c(100,110)
  )

  result <- obscureCounts(table, "PETOverviewBaby", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE))
  expect_equal(result$variable, c("pregnancies","fetuses"))
  expect_equal(result$count, c(100,110))



  table <- tibble::tibble(
    variable = c("pregnancy_id","person_id","pregnancy_start_date","pregnancy_end_date","gestational_length_in_day",
                 "pregnancy_outcome","pregnancy_mode_delivery","pregnancy_single","pregnancy_marital_status"),
    count = c(10,10,40,13,12,5,0,0,2),
    Percentage = c(10.0,10.0,40.0,13.0,12.0,5.0,0.0,0.0,2.0),
    total = c(100,100,100,100,100,100,100,100,100)
  )


  result <- obscureCounts(table, "missingSummaryMother", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE, FALSE, FALSE,FALSE, FALSE,FALSE, FALSE,TRUE))
  expect_equal(result$variable, c("pregnancy_id","person_id","pregnancy_start_date","pregnancy_end_date","gestational_length_in_day",
                                  "pregnancy_outcome","pregnancy_mode_delivery","pregnancy_single","pregnancy_marital_status"))
  expect_equal(result$count, c(10,10,40,13,12,5,0,0,NA))



  table <- tibble::tibble(
    variable = c("pregnancy_id","fetus_id","birth_outcome","birth_weight","birth_con_malformation"),
    count = c(0,0,6,2,0),
    Percentage = c(0.0,0.0,5.5,2.2,0.0),
    total = c(110,110,110,110,110)
  )


  result <- obscureCounts(table, "missingSummaryBaby", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE,FALSE,TRUE,FALSE))
  expect_equal(result$variable, c("pregnancy_id","fetus_id","birth_outcome","birth_weight","birth_con_malformation"))
  expect_equal(result$count, c(0,0,6,NA,0))



  table <- tibble::tibble(
    variable = c("different_gestationalAge","match_gestationalAge","missing_information","endBeforeStart","endAfterStart"),
    count = c(50,50,0,4,96),
    Percentage = c(50.0,50.0,0.0,4.0,96.0),
    total = c(100,100,100,100,100)
  )


  result <- obscureCounts(table, "gestationalAgeMatch", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE,FALSE,TRUE,FALSE))
  expect_equal(result$variable, c("different_gestationalAge","match_gestationalAge","missing_information","endBeforeStart","endAfterStart"))
  expect_equal(result$count, c(50,50,0,NA,96))


  table <- tibble::tibble(
   variable = c("no_match","match","missing_information"),
   count = c(30,66,4),
   Percentage = c(30.0,66.0,4.0),
   total = c(100,100,100)
 )

  result <- obscureCounts(table, "outcomeModeMatch", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(FALSE, FALSE,TRUE))
  expect_equal(result$variable, c("no_match","match","missing_information"))
  expect_equal(result$count, c(30,66,NA))



  table <- tibble::tibble(
    variable = c("relativeNumberWrong","relativeNumberRight","missing_relativeNumber","multipleWrong","multipleRight",
                 "missing_multiple"),
    count = c(4,60,36,3,87,20),
    Percentage = c(4.0,60.0,36.0,3.0,87.0,20.0),
    total = c(100,100,100,100,100,100)
  )

  result <- obscureCounts(table, "fetusesLivebornNumber", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(TRUE,FALSE, FALSE,TRUE,FALSE, FALSE))
  expect_equal(result$variable, c("relativeNumberWrong","relativeNumberRight","missing_relativeNumber","multipleWrong","multipleRight",
                                  "missing_multiple"))
  expect_equal(result$count, c(NA,60,36,NA,87,20))


  table <- tibble::tibble(
    variable = c("single_not_align_with_noOfFetusId","single_align_with_noOfFetusId","missing_single","noOfFetus_not_align_with_noOfFetusId","noOfFetus_align_with_noOfFetusId",
                 "missing_noOfFetus"),
    count = c(4,60,36,3,87,20),
    Percentage = c(4.0,60.0,36.0,3.0,87.0,20.0),
    total = c(100,100,100,100,100,100)
  )

  result <- obscureCounts(table, "fetusIdMatch", minCellCount = 5, substitute = NA)

  expect_equal(result$result_obscured, c(TRUE,FALSE, FALSE,TRUE,FALSE, FALSE))
  expect_equal(result$variable, c("single_not_align_with_noOfFetusId","single_align_with_noOfFetusId","missing_single","noOfFetus_not_align_with_noOfFetusId","noOfFetus_align_with_noOfFetusId",
                                  "missing_noOfFetus"))
  expect_equal(result$count, c(NA,60,36,NA,87,20))



})

