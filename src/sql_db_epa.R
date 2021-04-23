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
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  26Aug2020 Jack Gregory  New draft; ...


### START CODE ###


# epa ---------------------------------------------------------------------------------------------
## EPA schema for storage of CEMS data.

con <- DBI::dbConnect(RMySQL::MySQL(), 
                      user="root", 
                      password="blackberries22-", 
                      host="localhost")

DBI::SQL("
  CREATE SCHEMA IF NOT EXISTS `epa`;
  ") %>% {DBI::dbExecute(con, .)}

DBI::dbDisconnect(con)


### END CODE ###

