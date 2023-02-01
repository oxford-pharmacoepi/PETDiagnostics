usethis::edit_r_environ() #Add connection details + github path
#install.packages("usethis")
#install.packages("glue")
#install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/PETDiagnostics")


library(PETDiagnostics)
library(CDMConnector)
#> Warning: package 'CDMConnector' was built under R version 4.2.2
library(dplyr)




# Database connection details -----
server_dbi<-Sys.getenv("SERVER_DBI")
port<-Sys.getenv("DB_PORT")
host<-Sys.getenv("DB_HOST")
user<-Sys.getenv("DB_USER")
password<-Sys.getenv("DB_PASSWORD")

db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password,
                bigint = "integer")

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-Sys.getenv("DB_CDM_SCHEMA")

# The name of the schema that contains the vocabularies
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created
results_database_schema<-Sys.getenv("DB_WRITE_SCHEMA")

# Name of outcome table in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten
# Also note, this must be lower case
outcome_table<-"petdxresults"


# create cdm reference ----
# cdm <- cdm_From_Con(db, cdm_schema = cdm_schema)


cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)
cdm$motherTable <- tbl(db, sql("SELECT * FROM omop22t2_cmbd.pregnancy_episode"))

#cdm$motherTable%>% tally()


resultList1 <- executeChecks (
  motherTable = cdm$motherTable,
  babyTable = NULL,
  checks = c("overview","annualOverview","missing", "unknown","gestationalAge","datesAgeDist","outcomeMode",
             "fetusesLiveborn","fetusid","weightDist","bitSet"),
  minCellCount = 5,
  minGestAge_Days = 21,
  verbose = FALSE)


writeResultToDisk (resultList = resultList1, databaseId = "other5", outputFolder = "C:\\Users\\aabellan\\Documents\\Theresa")






##############
