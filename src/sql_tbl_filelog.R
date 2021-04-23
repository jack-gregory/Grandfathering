## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/sql_filelog_tbl.R
## Jack Gregory
## 16 April 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script creates the filelog table within the MySQL epa database.  This table is populated 
## with US Environmental Protection Agency (EPA) Continuous Emissions Monitoring System (CEMS) raw 
## data files which have been ingested successfully into the MySQL database.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  26Aug2020 Jack Gregory  New draft; ...


### START CODE ###


# filelog -----------------------------------------------------------------------------------------
## Filelog of ingested raw files for EPA CEMS data.

con <- DBI::dbConnect(RMySQL::MySQL(), 
                      user="root", 
                      password="blackberries22-", 
                      dbname="epa", 
                      host="localhost")

DBI::SQL("
    CREATE TABLE IF NOT EXISTS `epa`.`filelog` (
      `ID`        smallint(3) UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY
                  COMMENT 'File ID number',
      `FILE`      char(12) NOT NULL 
                  COMMENT 'Filename with extension',
      `PATH`      varchar(40) NOT NULL 
                  COMMENT 'Absolute file path including filename and extension',
      `SIZE`      smallint(6) NOT NULL 
                  COMMENT 'File size in kB',
      `STARTDATE` date NOT NULL 
                  COMMENT 'Data start date for the raw file',
      `MODDATE`   datetime NOT NULL 
                  COMMENT 'Last modification date of the raw file',
      `TRANSDATE` datetime NOT NULL 
                  COMMENT 'Transfer date of the raw file to MySQL burbank_com',
      UNIQUE KEY `idx_filelog`(`FILE`)
    ) 
    ENGINE=InnoDB 
    DEFAULT CHARSET=utf8mb4 
    COMMENT='Filelog of ingested EPA CEMS raw data files to MySQL epa database.'
    ;
  ") %>% {DBI::dbExecute(con, .)}

DBI::dbDisconnect(con)


### END CODE ###

