test_that("check working example 1) each count 2) counts add up to total", {
  MT<- tibble::tibble(
    pregnancy_id = c("4","5","6","7"),
    person_id = c("1","2","2","3"),
    pregnancy_start_date = c(as.Date("2012-10-15"),as.Date("2013-07-22"),as.Date("2015-07-22"),as.Date("2010-01-12")),
    pregnancy_end_date = c(as.Date("2013-06-22"),as.Date("2014-02-07"),as.Date("2016-02-07"),as.Date("2010-03-02")),
    gestational_length_in_day = c(300, 200,201,49),
    pregnancy_outcome = c(4092289,443213,0,4081422),
    pregnancy_mode_delivery = c(4015701,4125611,4125611,4125611),
    pregnancy_single = c(NA,4188540,4188540,4188539),
    pregnancy_marital_status = c(4338692,4338692,4338692,4053842),
    pregnancy_number_fetuses = c(NA,2,2,NA),
    pregnancy_number_liveborn = c(3,1,1,0),
    prev_pregnancy_parity = c(4012561,4102166,4012561,4012561),
    prev_pregnancy_gravidity = c(9,9,10,2),
    prev_livebirth_number = c(8,8,9,1),
    prev_stillbirth_number = c(3,3,3,0),
    prev_miscar_number = c(1,1,1,0),
    prev_top_number = c(0,0,0,1),
    prev_top12_number = c(9,9,9,0),
    pregnancy_bmi = c(51,48,48,30),
    pregnancy_folic = c(4188539,4188539,4188539,4188540),
    pregnancy_topfa = c(4188539,4188539,4188539,4188540),
    pregnancy_art = c(4188539,4188539,4188539,4188540),
    pregnancy_smok = c(4188539,4188539,4188539,4188540),
    pregnacy_alc = c(4188539,4188539,4188539,4188540),
    pregnancy_subs = c(4188539,4188539,4188539,4188540),
    pregnancy_outcome_source_value = c(69617,34789,20934,23948),
    pregnancy_mode_delivery_source_value = c(69617,23423,23423,13204),
  )


  BT <- tibble::tibble(
    pregnancy_id = c("4","5","6","7"),
    fetus_id = c("4","5","6","7"),
    birth_outcome = c(4092289,443213,4092289,4081422),
    birth_weight = c(6917,NA,2094, NA),
    birth_con_malformation = c(4188540,4188540,NA,NA),
    birth_sga = c(NA,NA,4188540,NA),
    birth_fgr = c(NA,4188540,NA,NA),
    birth_apgar = c(4188539,NA,NA,NA)
  )



  # into in-memory database
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  DBI::dbWriteTable(db, "person",
                    MT,
                    overwrite = TRUE)

  DBI::dbWriteTable(db, "observation_period",
                    BT,
                    overwrite = TRUE)

  # add other tables required for snapshot

  cdmSource <- dplyr::tibble(
    cdm_source_name = "test_database",
    cdm_source_abbreviation = NA,
    cdm_holder = NA,
    source_description = NA,
    source_documentation_reference = NA,
    cdm_etl_reference = NA,
    source_release_date = NA,
    cdm_release_date = NA,
    cdm_version = NA,
    vocabulary_version = NA
  )

  DBI::dbWriteTable(db, "cdm_source",
                    cdmSource,
                    overwrite = TRUE
  )


  cdm <- CDMConnector::cdm_from_con(db,
                                    write_schema = "main",
  )

  cdm$MT <- cdm$person
  cdm$BT <- cdm$observation_period


  seeFetId <- checkFetusId(cdm$MT,cdm$BT)

  #check all the counts
  expect_true(seeFetId[1,2]==2)
  expect_true(seeFetId[2,2]==1)
  expect_true(seeFetId[3,2]==1)
  expect_true(seeFetId[4,2]==2)
  expect_true(seeFetId[5,2]==0)
  expect_true(seeFetId[6,2]==2)


  #check that all counts add up to the Total
  expect_true(seeFetId[1,2] + seeFetId[2,2] + seeFetId[3,2] == seeFetId[1,4])
  expect_true(seeFetId[4,2] + seeFetId[5,2] + seeFetId[6,2] == seeFetId[4,4])


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})


