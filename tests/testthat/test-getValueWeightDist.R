test_that("check working example birth weight distribution", {
  BT <- tibble::tibble(
    pregnancy_id = c("4","5","6","7"),
    fetus_id = c("4","5","6","7"),
    birth_outcome = c(4092289,443213,4092289,4081422),
    birth_weight = c(6917,NA,2094, NA),
    birth_con_malformation = c(4188540,4188540,NA,NA),
    birth_SGA = c(NA,NA,4188540,NA),
    birth_FGR = c(NA,4188540,NA,NA),
    birth_APGAR = c(4188539,NA,NA,NA)
  )


  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")


  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "BT",
                      BT,
                      overwrite = TRUE)
  })

  cdm <- CDMConnector::cdm_from_con(db,
                                    cdm_tables = c(),
                                    cohort_tables = c(
                                     "BT"
                                    ))

  seeWeightDist <- getValueWeightDist(cdm$BT)

  #check the values
  expect_true(seeWeightDist[1,2]==2094)
  expect_true(seeWeightDist[9,2]==6917)


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})


