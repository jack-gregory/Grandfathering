## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/find_newfiles.R
## Jack Gregory
## 16 April 2020
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script identifies new data files for the US Environmental Protection Agency (EPA) 
## Continuous Emissions Monitoring System (CEMS) within the <E:/My Data/EPA/CEMS/...> 
## subdirectories.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version


### START CODE ###


# find_new_files ----------------------------------------------------------------------------------

find_new_files <- function(con, dir) {
  
  ## Assertions
  stopifnot(
    DBI::dbIsValid(con),
    length(dir)==1,
    fs::is_dir(dir)
  )
  
  ## Query MySQL epa.filelog
  res <- DBI::dbSendQuery(con, "
      select  PATH
      from    epa.filelog
    ;")
  filelog <- DBI::dbFetch(res, n=-1) %>%
    dplyr::pull()
  DBI::dbClearResult(res)

  ## Import EPA CEMS raw data files
  rawfiles <- fs::dir_ls(dir, recurse=2, regexp="[a-z]{2}[0-9]{2}\\.zip$")
  
  ## Return new raw files
  dplyr::setdiff(rawfiles, filelog)
}


### END CODE ###

