

library(CDMConnector)
source("~/GitHub/PETDiagnostics/R/utils.R", echo=TRUE)

motherTable = NULL
babytable = NULL
pregnancy_size = 1000
fetus_size = 1100
seed = 1

## there is a function to call the download of Eunomia dataset
# CDMConnector::downloadEunomiaData()
# put your file path to the eunomia dataset
dbdir= "C:/Users/tburkard/OneDrive - Nexus365/Documents/Darwin/eunomiaData/GiBleed_5.3_0.5.duckdb"

errorMessage <- checkmate::makeAssertCollection()
checkmate::assert_int(pregnancy_size, lower = 1)
checkmate::assert_int(fetus_size, lower = 1)
checkmate::assertTRUE(fetus_size >= pregnancy_size)
checkmate::reportAssertions(collection = errorMessage)


#mock mothertable
set.seed(seed)

if (is.null(mothertable)) {
  pregnancy_id <-
    (seq(1:pregnancy_size)) #generate number of unique pregnancy_id (integers)
  person_id <-
    (sample(seq(1:pregnancy_size),pregnancy_size, replace = TRUE)) #generate number of unique person id (integers)


  # generate pregnancy start date
  pregnancy_start_date <-  sample(seq(as.Date("2000-01-01"),
                                      as.Date("2020-01-01"),
                                      by = "day"),
                                  pregnancy_size,
                                  replace = TRUE)


  # generate pregnancy end date to happen after pregnancy start date
  pregnancy_end_date  <-
    pregnancy_start_date + lubridate::days(sample(c(44:280),
                                                  pregnancy_size,
                                                  replace = TRUE)
    )

  # define other columns in the dataset
  # generate gestational_length_in_day from end date minus start date
  gestational_length_in_day <- as.numeric(difftime(pregnancy_end_date,pregnancy_start_date,units="days"))



  pregnancy_outcome <-  sample(
    c(
      4092289,
      443213,
      4067106,
      4081422,
      4095714,
      0
    ),
    pregnancy_size,
    replace = TRUE,
    prob = c(0.65,0.1,0.1,0.05,0.05,0.05)
  ) #assign pregnancy outcome to each pregnancy

  pregnancy_mode_delivery <-  sample(
    c(
      4125611,
      4015701,
      0
    ),
    pregnancy_size,
    replace = TRUE
  ) #assign pregnancy delivery mode to each pregnancy

  pregnancy_single <- LowHighSampling(4188539,4188540,pregnancy_size)
  #assign whether this is a single or multiple pregnancy each pregnancy


  # non required variables
  pregnancy_marital_status <- sample(
    c(
      4053842,
      4338692,
      4242253,
      4095714,
      NA
    ),
    pregnancy_size,
    replace = TRUE
  ) #assign marital status to each pregnancy


  # #assign number of fetuses each pregnancy
  pregnancy_number_fetuses <- LowHighSampling(1,10,pregnancy_size)

  # #assign number of liveborns to each pregnancy
  # #assign number of previous gravidities to each pregnancy
  # #assign number of previous livebirths to each pregnancy
  # #assign number of previous stillbirths to each pregnancy
  # #assign number of previous miscarriages to each pregnancy
  # #assign number of previous terminations of pregnancy to each pregnancy
  # #assign number of previous terminations of pregnancy (above week 12) to each pregnancy
  arguments <- c("pregnancy_number_liveborn","prev_pregnancy_gravidity",
                 "prev_livebirth_number","prev_stillbirth_number","prev_miscar_number",
                 "prev_top_number","prev_top12_number")

  b <- tibble::tibble(.rows = pregnancy_size)

  for(i in 1:length(arguments)){

    b[[arguments[i]]] <- LowHighSampling(0,10,pregnancy_size)

  }


  prev_pregnancy_parity <- sample(
    c(
      4012561,
      4102166,
      NA
    ),
    pregnancy_size,
    replace = TRUE
  ) #assign previous parity to each pregnancy


  pregnancy_bmi <- sample(15:90,pregnancy_size,
                          replace = TRUE)
  #assign Body mass index value to each pregnancy



  #assign use of folic acid to each pregnancy
  #assign whether this pregnancy was terminated due to fetal anomaly to each pregnancy
  #assign whether the mother received assisted reproductive technology for this pregnancy
  #assign whether the mother was smoking during this pregnancy
  #assign whether the mother was consuming alcohol during this pregnancy
  #assign whether the mother was using illicit drugs during this pregnancy
  arguments <- c("pregnancy_folic","pregnancy_topfa","pregnancy_art",
                 "pregnancy_smok","pregnacy_alc",
                 "pregnancy_subs")

  a <- tibble::tibble(.rows = pregnancy_size)

  for(i in 1:length(arguments)){

    a[[arguments[i]]] <- LowHighSampling(4188539,4188540,pregnancy_size)

  }



  pregnancy_outcome_source_value <- as.character(sample(55555:77777,pregnancy_size,
                                                        replace = TRUE))
  #indicates the source value of the variable pregnancy_outcome in the original data

  pregnancy_mode_delivery_source_value <- as.character(sample(55555:77777,pregnancy_size,
                                                              replace = TRUE))
  #indicates the source value of the variable pregnancy_mode_delivery in the original data


  # putting into the mothertable
  mothertable <-
    data.frame(cbind(
      pregnancy_id = pregnancy_id,
      person_id = person_id,
      pregnancy_start_date = pregnancy_start_date,
      pregnancy_end_date = pregnancy_end_date,
      gestational_length_in_day = gestational_length_in_day,
      pregnancy_outcome = pregnancy_outcome,
      pregnancy_mode_delivery = pregnancy_mode_delivery,
      pregnancy_single = pregnancy_single,
      pregnancy_marital_status = pregnancy_marital_status,
      pregnancy_number_fetuses = pregnancy_number_fetuses,
      b,
      prev_pregnancy_parity = prev_pregnancy_parity,
      pregnancy_bmi = pregnancy_bmi,
      pregnancy_outcome_source_value = pregnancy_outcome_source_value,
      pregnancy_mode_delivery_source_value = pregnancy_mode_delivery_source_value,
      a))

  #add some random missingness

}


#mock babytable
set.seed(seed)

if (is.null(babytable)) {

  #generate number of unique pregnancy_id (integers)
  pregnancy_id <- rep((seq(1:pregnancy_size)),length.out = fetus_size)
  #repeat some to make it fit with fetus id size

  fetus_id <-
    seq(1:fetus_size) #generate number of unique fetus_id (integers)


  birth_outcome <-  rep(sample(
    c(
      4092289,
      443213,
      4067106,
      4081422,
      NA
    ),
    pregnancy_size,
    replace = TRUE,
    prob = c(0.7,0.1,0.1,0.05,0.05)
  ),length.out = fetus_size) #assign birth outcome to each fetus
  #it seems that the birth outcome is identical to pregnancy outcome
  #but not quite, had to make some up to make it fit with fetus id size


  birth_weight <- sample(500:5000,fetus_size,
                         replace = TRUE)



  #assign whether the baby was born with a congenital malformation
  #assign whether the baby was small for gestational age
  #assign whether there was fetal growth gestation
  arguments <- c("birth_con_malformation","birth_sga","birth_fgr"
  )

  c <- tibble::tibble(.rows = fetus_size)

  for(i in 1:length(arguments)){

    c[[arguments[i]]] <- LowHighSampling(4188539,4188540,fetus_size)

  }



  birth_apgar <- LowHighSampling(0,10,fetus_size)
  #assign 5-minute APGAR score from 0 to 10 to the fetus

  # putting into the babytable
  babytable <-
    data.frame(cbind(
      pregnancy_id = pregnancy_id,
      fetus_id = fetus_id,
      birth_outcome = birth_outcome,
      birth_weight = birth_weight,
      c,
      birth_apgar = birth_apgar))

  #add some random missingness


}



con <- DBI::dbConnect(duckdb::duckdb(), dbdir=dbdir)


DBI::dbWriteTable(con, "mothertable",
                  mothertable,
                  overwrite = TRUE)

DBI::dbWriteTable(con, "babytable",
                  babytable,
                  overwrite = TRUE)

cdm <- CDMConnector::cdmFromCon(con,
                                  cdmSchema = "main",
                                  writeSchema = "main")

cdm$babytable <- dplyr::tbl(con, "babytable")
cdm$mothertable <- dplyr::tbl(con, "mothertable")

DBI::dbDisconnect(con, shutdown = TRUE)

