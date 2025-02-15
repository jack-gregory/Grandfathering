## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/sql_tbl_xwalk.R
## Jack Gregory
## 16 April 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script creates the xwalk table within the MySQL epa database.  This table is populated 
## with US Environmental Protection Agency (EPA) and Energy Information Agency (EIA) power plant
## crosswalk data.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  26Aug2020 Jack Gregory  New draft; ...


### START CODE ###


# xwalk -------------------------------------------------------------------------------------------
## Cleaned EPA-EIA power plant crosswalk.

con <- DBI::dbConnect(RMySQL::MySQL(),
                      user = Sys.getenv("MYSQL_USER"),
                      password = Sys.getenv("MYSQL_PASSWORD"),
                      dbname = Sys.getenv("MYSQL_DB"),
                      host = Sys.getenv("MYSQL_HOST"))

DBI::SQL("
    CREATE TABLE IF NOT EXISTS `epa`.`xwalk` (
      `ID`                  smallint(4) UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY
                            COMMENT 'Crosswalk identifier',
      `CAMD_STATE`          char(2) NULL 
                            COMMENT 'CAMD state abbreviation',
      `CAMD_FACILITY_NAME`  varchar(40) NOT NULL 
                            COMMENT 'CAMD facility name',
      `CAMD_PLANT_ID`       mediumint(6) UNSIGNED NOT NULL 
                            COMMENT 'CAMD plant identifier synonymous with the Office of 
                            Regulatory Information Systems Plant Location (ORISPL) 
                            identifier (NB - equivalent to cems.ORISPL)',
      `CAMD_UNIT_ID`        varchar(10) NOT NULL 
                            COMMENT 'CAMD unit identifier (NB - equivalent to cems.UNIT)',
      `EPA_FACILITY_ID`     mediumint(6) UNSIGNED NULL 
                            COMMENT 'EPA CEMS plant identifier from CEMS data',
      `EPA_UNIT_ID`         mediumint(6) UNSIGNED NULL 
                            COMMENT 'EPA unit identifier from CEMS data',
      `CAMD_GENERATOR_ID`   varchar(10) NULL 
                            COMMENT 'CAMD generator identifier',
      `CAMD_CAPACITY`       decimal(6,2) NULL 
                            COMMENT 'CAMD nameplate capacity in MW',
      `CAMD_FUEL_TYPE`      set('Coal','Coal Refuse','Diesel Oil','Natural Gas','Other Gas',
                            'Other Oil','Other Solid Fuel','Petroleum Coke',
                            'Pipeline Natural Gas','Process Gas','Residual Oil',
                            'Tire Derived Fuel','Wood') NULL 
                            COMMENT 'CAMD primary fuel type in set {Coal, Coal Refuse, Diesel Oil,
                            Natural Gas, Other Gas, Other Oil, Other Solid Fuel, Petroleum Coke,
                            Pipeline Natural Gas, Process Gas, Residual Oil, Tire Derived Fuel,
                            Wood}',
      `CAMD_LATITUDE`       decimal(7,4) NOT NULL 
                            COMMENT 'CAMD latitude in decimal degrees',
      `CAMD_LONGITUDE`      decimal(7,4) NOT NULL 
                            COMMENT 'CAMD longitude in decimal degrees',
      `CAMD_STATUS`         set('OPR','LTCS','RET') NOT NULL 
                            COMMENT 'CAMD status in set {OPR = Operating, LTCS = Long Term
                            Cold Storage, RET = Retired}',
      `CAMD_STATUS_DATE`    date NOT NULL 
                            COMMENT 'CAMD status change date',      
      `CAMD_RETIRE_YEAR`    smallint(4) UNSIGNED NULL 
                            COMMENT 'CAMD retirement year',
      `EIA_STATE`           char(2) NULL 
                            COMMENT 'EIA state abbreviation',
      `EIA_PLANT_NAME`      varchar(55) NULL 
                            COMMENT 'EIA facility name',
      `EIA_PLANT_ID`        mediumint(6) UNSIGNED NULL 
                            COMMENT 'EIA plant identifier synonymous with the Office of 
                            Regulatory Information Systems Plant Location (ORISPL) 
                            identifier',
      `EIA_GENERATOR_ID`    varchar(10) NULL 
                            COMMENT 'EIA generator identifier',
      `EIA_CAPACITY`        decimal(6,2) NULL 
                            COMMENT 'EIA nameplate capacity in MW',
      `EIA_BOILER_ID`       varchar(10) NULL 
                            COMMENT 'EIA boiler identifier',
      `EIA_UNIT_TYPE`       set('CA','CS','CT','GT','IC','ST') NULL 
                            COMMENT 'EIA unit type in set {CA, CS, CT, GT, IC, ST}',
      `EIA_FUEL_TYPE`      set('BFG','BIT','BLQ','DFO','JF','KER','LFG','LIG','MSW','NG',
                            'OBS','OG','PC','RC','RFO','SGC','SGP','SUB','SUN','TDF','WC',
                            'WDS','WO') NULL 
                            COMMENT 'EIA fuel type in set {BFG, BIT, BLQ, DFO, JF, KER, 
                            LFG, LIG, MSW, NG, OBS, OG, PC, RC, RFO, SGC, SGP, SUB, SUN, 
                            TDF, WC, WDS, WO}',
      `EIA_LATITUDE`        decimal(7,4) NOT NULL 
                            COMMENT 'EIA latitude in decimal degrees',
      `EIA_LONGITUDE`       decimal(7,4) NOT NULL 
                            COMMENT 'EIA longitude in decimal degrees',
      `EIA_RETIRE_YEAR`     smallint(4) UNSIGNED NULL 
                            COMMENT 'EIA retirement year'
    ) 
    ENGINE=InnoDB 
    DEFAULT CHARSET=utf8mb4 
    COMMENT='Cleaned EPA-EIA power plant crosswalk.'
    ;
  ") %>% {DBI::dbExecute(con, .)}

DBI::dbDisconnect(con)


### END CODE ###

