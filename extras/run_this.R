install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/PETDiagnostics")

library(PETDiagnostics)
library(CDMConnector)
library(dplyr)

# Database connection details -----
server_dbi<-Sys.getenv("SERVER_DBI")
port<-Sys.getenv("DB_PORT")
host<-Sys.getenv("DB_HOST")
user<-Sys.getenv("DB_USER")
password<-Sys.getenv("DB_PASSWORD")

# to add bigint = "integer" or alternatively bigint = "numeric" is very important
con <- DBI::dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password,
                bigint = "integer")


# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-Sys.getenv("DB_cdmSchema")

# The name of the schema that contains the vocabularies
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created
results_database_schema<-Sys.getenv("DB_writeSchema")

# create cdm reference ----
cdm <- CDMConnector::cdmFromCon(con ,
                                  cdmSchema = cdm_database_schema,
                                  writeSchema = results_database_schema)

## add your table to the cdm and call it motherTable and babyTable (if present)
cdm$motherTable <- dplyr::tbl(con, sql("SELECT * FROM CDM_NAME.TABLE_NAME"))
cdm$babyTable <- dplyr::tbl(con, sql("SELECT * FROM CDM_NAME.TABLE_NAME"))

## run the checks, use the motherTable, babyTable or both, if one is not available put NULL
resultList <- executeChecks (
  motherTable = cdm$motherTable,
  babyTable = NULL,
  checks = c("overview","annualOverview","missing", "unknown","gestationalAge","datesAgeDist","outcomeMode",
             "fetusesLiveborn","fetusid","weightDist","bitSet"),
  minCellCount = 5,
  minGestAge_Days = 21,
  verbose = FALSE)

## put your database name and the path to your output folder where they will be stored in a zip file.
writeResultToDisk (resultList = resultList, databaseId = "YOUR_DATABASE_NAME", outputFolder = "PATH_TO_DESIRED_OUTPUT_FOLDER")

