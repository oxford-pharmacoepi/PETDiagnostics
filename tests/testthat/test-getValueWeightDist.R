test_that("check working example birth weight distribution", {
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


  cdm$BT <- cdm$observation_period

  seeWeightDist <- getValueWeightDist(cdm$BT)

  #check the values
  expect_true(seeWeightDist[1,2]==2094)
  expect_true(seeWeightDist[9,2]==6917)


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})


