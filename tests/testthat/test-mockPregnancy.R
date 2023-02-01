test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("check working example with defaults", {
  db <- mockPregnancy()

  cdmCheck <- inherits(db, "cdm_reference")
  expect_true(cdmCheck)

  expect_true(nrow(db$motherTable %>%
                     dplyr::collect()) >= 1)
  expect_true(nrow(db$babyTable %>%
                     dplyr::collect()) >= 1)


  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)

})



test_that("check presence of columns in motherTable", {
  db <- mockPregnancy()

  ColsDbNames <- c("pregnancy_id","person_id","pregnancy_start_date","pregnancy_end_date",
                    "gestational_length_in_day","pregnancy_outcome","pregnancy_mode_delivery","pregnancy_single",
                    "pregnancy_marital_status","pregnancy_number_fetuses","pregnancy_number_liveborn","prev_pregnancy_gravidity",
                    "prev_livebirth_number","prev_stillbirth_number","prev_miscar_number","prev_top_number","prev_top12_number",
                    "prev_pregnancy_parity","pregnancy_bmi","pregnancy_outcome_source_value","pregnancy_mode_delivery_source_value",
                    "pregnancy_mode_delivery_source_value","pregnancy_folic","pregnancy_topfa",
                    "pregnancy_art","pregnancy_smok","pregnacy_alc","pregnancy_subs")
  ColsNamesCheck <- all(ColsDbNames %in%
                             names(db$motherTable %>%
                                     utils::head(1) %>%
                                     dplyr::collect() ))
  expect_true(ColsNamesCheck)


  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)

})

test_that("check presence of columns in babyTable", {
  db <- mockPregnancy()

  ColsDbNames <- c("pregnancy_id","fetus_id","birth_outcome","birth_weight","birth_con_malformation",
                   "birth_sga","birth_fgr","birth_apgar")
  ColsNamesCheck <- all(ColsDbNames %in%
                          names(db$babyTable %>%
                                  utils::head(1) %>%
                                  dplyr::collect() ))
  expect_true(ColsNamesCheck)



  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})





test_that("check working example pregnancy size and motherTable length", {
  db <- mockPregnancy(pregnancy_size = 100)

expect_true(nrow(db$motherTable %>%
                     dplyr::collect()) == 100)


   DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})




test_that("check working example fetus size and babyTable length", {
  db <- mockPregnancy(fetus_size = 110)

  expect_true(nrow(db$babyTable %>%
                     dplyr::collect()) == 110)


  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})



test_that("check expect_error", {

  expect_error(mockPregnancy(motherTable = "x",
                             ))

  expect_error(mockPregnancy(babyTable = "x"
                             ))

    expect_error(mockPregnancy(pregnancy_size = "x"
                             ))

  expect_error(mockPregnancy(pregnancy_size = -1
  ))


  expect_error(mockPregnancy(fetus_size = "x"
  ))

  expect_error(mockPregnancy(fetus_size = -1
  ))


})


