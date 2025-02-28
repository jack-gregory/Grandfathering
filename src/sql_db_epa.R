## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/sql_db_epa.R
## Jack Gregory
## 16 April 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## INTRODUCTION -----------------------------------------------------------------------------------
## This script creates the epa schema within the local MySQL database.  The schema is populated 
## with US Environmental Protection Agency (EPA) Continuous Emissions Monitoring System (CEMS) 
## data.


## VERSION HISTORY --------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version


### START CODE ###


# epa ---------------------------------------------------------------------------------------------
## EPA schema for storage of CEMS data.

con <- DBI::dbConnect(RMySQL::MySQL(),
                      user = Sys.getenv("MYSQL_USER"),
                      password = Sys.getenv("MYSQL_PASSWORD"),
                      dbname = Sys.getenv("MYSQL_DB"),
                      host = Sys.getenv("MYSQL_HOST"))

DBI::SQL("
  CREATE SCHEMA IF NOT EXISTS `epa`;
  ") %>% {DBI::dbExecute(con, .)}

DBI::dbDisconnect(con)


### END CODE ###

