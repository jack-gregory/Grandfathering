## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/sql_tbl_cems.R
## Jack Gregory
## 16 April 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script creates the cems table within the MySQL epa database.  This table is populated 
## with US Environmental Protection Agency (EPA) Continuous Emissions Monitoring System (CEMS) 
## cleaned data.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  26Aug2020 Jack Gregory  New draft; ...


### START CODE ###


# cems --------------------------------------------------------------------------------------------
## Cleaned EPA CEMS data.

con <- DBI::dbConnect(RMySQL::MySQL(),
                      user = Sys.getenv("MYSQL_USER"),
                      password = Sys.getenv("MYSQL_PASSWORD"),
                      dbname = Sys.getenv("MYSQL_DB"),
                      host = Sys.getenv("MYSQL_HOST"))

DBI::SQL("
    CREATE TABLE IF NOT EXISTS `epa`.`cems` (
      `FILEID`    smallint(3) UNSIGNED NOT NULL
                  COMMENT 'File identifier',
      `ORISPL`    mediumint(6) UNSIGNED NOT NULL 
                  COMMENT 'Office of Regulatory Information Systems Plant Location (ORISPL) 
                  identifier (NB - equivalent to xwalk.CAMD_PLANT_ID)',
      `UNIT`      varchar(10) NOT NULL 
                  COMMENT 'Unit identifier (NB - equivalent to xwalk.CAMD_UNIT_ID)',
      `DATETIME`  datetime NOT NULL 
                  COMMENT 'Measurement datetime at start of hourly interval',
      `DURATION`  decimal(3,2) NOT NULL 
                  COMMENT 'Operation duration within DATETIME hour',
      `GLOAD`     smallint(4) NULL 
                  COMMENT 'Gross load or average power delivered during DATETIME hour in MW',
      `SLOAD`     smallint(4) NULL 
                  COMMENT 'Steam load or average steam pressure produced during DATETIME hour 
                  in klb/hr',
      `HEAT`      decimal(9,3) NULL 
                  COMMENT 'Energy contained within fuel burned during DATETIME hour, calculated 
                  by multiplying the quantity of fuel by its heat content in mmBTU',
      `CO2_MASS`  decimal(9,3) NULL 
                  COMMENT 'Carbon dioxide emissions during DATETIME hour in tons',
      `SO2_MASS`  decimal(9,3) NULL 
                  COMMENT 'Sulfur dioxide emissions during DATETIME hour in lbs',
      `NOX_MASS`  decimal(9,3) NULL 
                  COMMENT 'Nitrogen oxide emissions during DATETIME hour in lbs',
      UNIQUE KEY `idx_cems` (`DATETIME`, `ORISPL`, `UNIT`)
    ) 
    ENGINE=InnoDB 
    DEFAULT CHARSET=utf8mb4 
    COMMENT='Cleaned EPA CEMS hourly data from 1995 onward.'
    ;
  ") %>% {DBI::dbExecute(con, .)}

DBI::dbDisconnect(con)


### END CODE ###

