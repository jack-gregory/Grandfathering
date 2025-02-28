## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/sql_data_transfer.R
## Jack Gregory
## 16 April 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script provides the functions necessary to transfer US Environmental Protection Agency 
## (EPA) Continuous Emissions Monitoring System (CEMS) raw files into the MySQL epa database.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version


### START CODE ###


# cems_read ---------------------------------------------------------------------------------------
## This function reads raw CEMS data from zipped csv files.

cems_read <- function(zip) {
  
  ## Assertions
  stopifnot(
    length(zip)==1,
    fs::is_file(zip)
  )
  
  ## Unzip into same dir
  unzip(zip, exdir=path_dir(zip))
  
  ## Import data
  csv <- paste0(path_ext_remove(zip), ".csv")
  df <- read_csv(csv, col_types="cciccinnnncncncncncncnii") %>%
    rename_with(.fn=function(x) str_replace(x, "\\s\\(.+\\)", ""))
  
  ## Delete csv
  file_delete(csv)
  
  if (nrow(df)>0) {
    df %>%
      assertr::chain_start() %>%
      assertr::verify(assertr::has_all_names("ORISPL_CODE","UNITID","OP_DATE","OP_HOUR",
                                             "OP_TIME","GLOAD","SLOAD","CO2_MASS",
                                             "SO2_MASS","NOX_MASS","HEAT_INPUT")) %>%
      assertr::assert(is.character, UNITID, OP_DATE) %>%
      assertr::assert(is.numeric, ORISPL_CODE, OP_HOUR, OP_TIME, ends_with("LOAD"),
                      ends_with("MASS"), HEAT_INPUT) %>%
      assertr::assert_rows(col_concat, is_uniq, ORISPL_CODE, UNITID, OP_DATE, OP_HOUR) %>%
      assertr::chain_end() %>%
      return()
  } else return(NULL)
}


# filelog_write -----------------------------------------------------------------------------------
## This function writes file record metadata to the MySQL epa.filelog table.

filelog_write <- function(con, zip) {
  
  ## Prepare record inputs
  df <- fs::dir_info(fs::path_dir(zip)) %>% 
    filter(path==zip) %>%
    mutate(FILE = fs::path_file(path),
           PATH = as.character(path),
           SIZE = round(as.numeric(size)/1024, 0),
           STARTDATE = ymd(paste0(str_extract(FILE, "^[0-9]{4}"),
                                  str_extract(FILE, "(?<=)[0-9]{2}(?=\\.)"),
                                  "01")),
           TRANSDATE = lubridate::now()) %>%
    select(FILE, PATH, SIZE, STARTDATE,
           MODDATE = modification_time,
           TRANSDATE)
  
  ## Write file record to MySQL epa.filelog
  df %>%
    glue::glue_data("
      INSERT INTO epa.filelog (
        FILE, PATH, SIZE, STARTDATE, MODDATE, TRANSDATE
      ) VALUES (
        '{FILE}','{PATH}',{SIZE},'{STARTDATE}','{MODDATE}','{TRANSDATE}'
      )
      ON DUPLICATE KEY UPDATE 
        SIZE={SIZE},
        MODDATE='{MODDATE}',
        TRANSDATE='{TRANSDATE}'
      ;
    ") %>% DBI::SQL() %>% {DBI::dbExecute(con, .)}
}


# cems_write --------------------------------------------------------------------------------------
## This function writes cems data to the MySQL epa.cems table.

cems_write <- function(con, zip, df) {
  
  ## Check for valid MySQL epa.filelog record
  df.filelog <- 
    glue::glue("
      SELECT  ID, 
              FILE
      FROM    epa.filelog
      WHERE   FILE='{fs::path_file(zip)}'
      ;
    ") %>% DBI::SQL() %>% {DBI::dbGetQuery(con, .)}
  if (nrow(df.filelog)!=1) {
    stop(glue::glue("Write filelog record for {fs::path_file(zip)} first."))
  }
  
  ## Add FILEID to df
  df <- df %>%
    dplyr::mutate(FILEID = df.filelog$ID) %>%
    dplyr::relocate(FILEID)
  
  ## Write data to MySQL epa.cems
  DBI::dbWriteTable(con,
                    name="cems", 
                    value=df, 
                    row.names=FALSE, append=TRUE)
}


# xwalk_write -------------------------------------------------------------------------------------
## This function writes epa id data to the MySQL epa.xwalk table.

xwalk_write <- function(con, zip, df) {
  
  ## Query MySQL epa.xwalk
  res <- DBI::dbSendQuery(con, "
      select  *
      from    epa.xwalk
    ;")
  df.xwalk <- DBI::dbFetch(res, n=-1)
  DBI::dbClearResult(res)
  
  ## Join xwalk with epa id data
  df <- df.xwalk %>%
    select(-starts_with("EPA")) %>%
    inner_join(df, by=c("CAMD_PLANT_ID","CAMD_UNIT_ID")) %>%
    relocate(starts_with("EPA"), .after=CAMD_UNIT_ID)
  
  ## Write temporary data to MySQL epa.xwalk_tmp
  DBI::dbWriteTable(con,
                    name="xwalk_tmp",
                    value=df, 
                    row.names=FALSE, overwrite=TRUE, temporary=TRUE)
  
  ## Insert data into MySQL epa.xwalk
  glue::glue("
    INSERT INTO epa.xwalk (
      ID, CAMD_STATE, CAMD_FACILITY_NAME, CAMD_PLANT_ID, CAMD_UNIT_ID, EPA_FACILITY_ID, 
      EPA_UNIT_ID, CAMD_GENERATOR_ID, CAMD_CAPACITY, CAMD_FUEL_TYPE, CAMD_LATITUDE, 
      CAMD_LONGITUDE, CAMD_STATUS, CAMD_STATUS_DATE, CAMD_RETIRE_YEAR, EIA_STATE, 
      EIA_PLANT_NAME, EIA_PLANT_ID, EIA_GENERATOR_ID, EIA_CAPACITY, EIA_BOILER_ID, 
      EIA_UNIT_TYPE, EIA_FUEL_TYPE, EIA_LATITUDE, EIA_LONGITUDE, EIA_RETIRE_YEAR 
    ) 
    SELECT  *
    FROM    epa.xwalk_tmp AS tmp
    ON DUPLICATE KEY UPDATE 
      EPA_FACILITY_ID = tmp.EPA_FACILITY_ID, 
      EPA_UNIT_ID = tmp.EPA_UNIT_ID
    ;
  ") %>% DBI::SQL() %>% {DBI::dbGetQuery(con, .)}
}


# cems_sql ----------------------------------------------------------------------------------------
## This function reads raw data and writes clean data to the MySQL epa database. 

## Read sheet, clean data and write to SQL for euse
cems_sql <- function(con, zip, filter) {
  
  ## Assertions
  stopifnot(
    DBI::dbIsValid(con),
    length(zip)==1,
    fs::is_file(zip),
    is.data.frame(filter),
    names(filter)=="ORISPL",
    is.numeric(filter$ORISPL)
  )
  
  cat("\n\n", path_file(zip), "\n", sep="")
  
  ## Read data from zip
  cat("    Read ...")
  df <- cems_read(zip)
  
  
  if (is.null(df)) {
    cat(" empty")
  } else {
    ## Process data into various tables
    cat("\n    Process ...")
    
    ## CEMS data
    df.cems <- df %>%
      mutate(DATETIME = mdy_h(paste(OP_DATE, OP_HOUR))) %>%
      select(ORISPL = ORISPL_CODE,
             UNIT = UNITID,
             DATETIME,
             DURATION = OP_TIME,
             GLOAD, SLOAD,
             HEAT = HEAT_INPUT,
             CO2_MASS, SO2_MASS, NOX_MASS) %>%
      inner_join(filter, by=c("ORISPL"))
    
    if (nrow(df.cems)==0) {
      cat(" no plants")
    } else {
      ## EPA IDs
      if (any(names(df)=="FAC_ID")) {
        df.epa_id <- df %>%
          distinct(CAMD_PLANT_ID = ORISPL_CODE,
                   CAMD_UNIT_ID = UNITID,
                   EPA_FACILITY_ID = FAC_ID,
                   EPA_UNIT_ID = UNIT_ID)
      }
      
      
      ## Write data
      cat("\n    Write ...")
      tryCatch({
        ## Commit write on success
        DBI::dbBegin(con)
        
        cat(" filelog")
        filelog_write(con, zip)
        
        cat(" ... cems")
        cems_write(con, zip, df.cems)
        
        if (exists("df.epa_id")) {
          cat(" ... xwalk")
          xwalk_write(con, zip, df.epa_id)
        }
        
        DBI::dbCommit(con)
      },
      ## Rollback on failure
      error = function(e) {
        DBI::dbRollback(con)
        stop(glue::glue("ERROR: Upload failed for {fs::path_file(zip)}\n{e}"), call.=TRUE)
      },
      finally = {
        glue::glue("
        DROP TABLE IF EXISTS epa.xwalk_tmp
        ;
      ") %>% DBI::SQL() %>% {DBI::dbExecute(con, .)}
      })
    }
  }
}


### END CODE ###

