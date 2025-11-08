test_that("check working example birth weight distribution", {
  bt <- tibble::tibble(
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



  cdm_source <- dplyr::tibble(
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

  person <- dplyr::tibble(
    person_id = 1,
    gender_concept_id = 1,
    year_of_birth = 1,
    race_concept_id = 1,
    ethnicity_concept_id = 1
  )

  observation_period <- dplyr::tibble(
    person_id = 1,
    observation_period_id = 1,
    observation_period_start_date = as.Date(2002-01-01),
    observation_period_end_date = as.Date(2002-01-01),
    period_type_concept_id = 1
  )


  DBI::dbWriteTable(db, "cdm_source",
                    cdm_source,
                    overwrite = TRUE
  )

  DBI::dbWriteTable(db, "person",
                    person,
                    overwrite = TRUE)

  DBI::dbWriteTable(db, "observation_period",
                    observation_period,
                    overwrite = TRUE)


  cdm <- CDMConnector::cdmFromCon(db,
                                    cdmSchema = "main",
                                    writeSchema = "main",
  )
  writeSchema = "main"

  DBI::dbWriteTable(db, CDMConnector::inSchema(writeSchema, "bt"),
                    bt,
                    overwrite = TRUE)

  cdm$bt <- dplyr::tbl(db, CDMConnector::inSchema(writeSchema, "bt"))

  seeWeightDist <- getValueWeightDist(cdm$bt)

  #check the values
  expect_true(seeWeightDist[1,2]==2094)
  expect_true(seeWeightDist[9,2]==6917)


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})


