## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## CEMS Query
## Jack Gregory
## 15 May 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This program queries US Environmental Protection Agency (EPA) Continuous Emissions Monitoring 
## System (CEMS) data from the MySQL epa database.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  15May2021 Jack Gregory  Initial version


### START CODE ###


# (1) PREAMBLE ------------------------------------------------------------------------------------

## (1a) Initiate 
## ... Packages
pkgs <- c(
  "here"                # File system
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

source(here::here("src/preamble.R"))

## ... Functions
source(here::here("src/find_newfiles.R"))
source(here::here("src/sql_data_transfer.R"))

## ... Data
df.gf <- read_csv(here::here("data/regression_vars.csv")) %>%
  distinct(ORISPL = plant_code) %>%
  arrange(ORISPL)


## (1b) Connect to MySQL burbank_com database
con <- DBI::dbConnect(RMySQL::MySQL(),
                      user = Sys.getenv("MYSQL_USER"),
                      password = Sys.getenv("MYSQL_PASSWORD"),
                      dbname = Sys.getenv("MYSQL_DB"),
                      host = Sys.getenv("MYSQL_HOST"))


# (2) QUERY CEMS -- YEARS -------------------------------------------------------------------------
## This section prepares CEMS yearly data.

## (2a) Create list of years
l.yrs <- seq(1995, 2021) %>%
  as.list()


## (2b) Create year iteration function
itr_year <- function(yr) {
  
  ## Assertions
  stopifnot(
    is.integer(yr),
    yr>=1995 & yr<=year(Sys.Date())
  )
  
  ## Send query
  cat("\n", yr, " ...\n  Send query\n", sep="")
  res <- glue::glue_sql("
    select 		ORISPL,
  			      UNIT,
  			      year(DATETIME) as YEAR,
              sum(if(DURATION>0, 1, 0)) AS N,
  			      sum(DURATION) as DURATION,
  		        sum(GLOAD) as GLOAD,
  			      sum(SLOAD) as SLOAD,
        			sum(HEAT) as HEAT,
        			sum(CO2_MASS) as CO2_MASS,
        			sum(SO2_MASS) as SO2_MASS,
        			sum(NOX_MASS) as NOX_MASS
    from		  epa.cems
    where		  year(DATETIME)={yr}
    group by	ORISPL, UNIT
    ;", .con=con) %>% 
    DBI::SQL() %>% 
    {DBI::dbSendQuery(con, .)}
  
  ## Fetch query
  cat("  Fetch query\n")
  df <- DBI::dbFetch(res, n=-1)
  DBI::dbClearResult(res)
  
  ## Save query
  cat("  Save query\n")
  saveRDS(df, here::here("data/epa/cems_{yr}.rds"))
  return(df)
}


## (2c) Query yearly data
## Send query
df.cems_yr <- purrr::map_dfr(l.yrs, itr_year) %>%
  arrange(ORISPL, UNIT, YEAR)


## (2d) Save dataframe as csv
readr::write_csv(df.cems_yr, here::here("data/cems_yr.csv"))


# (3) UNIT MATCHES --------------------------------------------------------------------------------
## This section produces a table for manual matching and then reimports the finalized matching.

## (3a) Access MySQL crosswalk
res <- DBI::dbSendQuery(con, "
    select distinct *
    from            epa.xwalk
  ;")
df.xwalk_epa_eia <- DBI::dbFetch(res, n=-1)
DBI::dbClearResult(res)


# ## (3b) Join GF & CEMS units
# ## Import grandfathering dataset and check ORISPL & UNIT matches
# df.gf <- read_csv(here::here("data/regression_vars.csv"), guess_max=50000) %>%
#   select(plant_code, boiler_id, year, plant_name, plt_county) %>%
#   filter(year>=1995) %>%
#   mutate_at(vars(plant_name, plt_county), str_to_upper) %>%
#   group_by(plant_code, boiler_id) %>%
#   fill(plant_name, plt_county, .direction="downup") %>%
#   ungroup() %>%
#   distinct(ORISPL = plant_code,
#            BOILER = boiler_id,
#            PLANT_NAME = plant_name,
#            COUNTY = plt_county) %>%
#   arrange(ORISPL, BOILER) %>%
#   group_by(ORISPL, BOILER, COUNTY) %>%
#   summarise_all(last) %>%
#   ungroup() %>%
#   mutate(BOILER = as.character(BOILER),
#          SOURCE = "GF") %>%
#   full_join(df.cems_yr %>% distinct(ORISPL, BOILER = UNIT) %>% mutate(SOURCE = "CEMS"),
#             by=c("ORISPL","BOILER")) %>%
#   mutate(MATCH = (!is.na(SOURCE.x) & !is.na(SOURCE.y))) %>%
#   left_join(df.xwalk_epa_eia %>%
#               select(-ID, -starts_with("EPA")) %>%
#               rename(ORISPL = CAMD_PLANT_ID, BOILER = CAMD_UNIT_ID),
#             by=c("ORISPL","BOILER")) %>%
#   arrange(ORISPL, BOILER) #%>%
#   # group_by(MATCH) %>%
#   # group_split()
# 
# 
# ## (3c) Save dataframe as csv
# readr::write_csv(df.gf, here::here("data/gf_matches_full.csv"))


## (3d) Import GF-CEMS xwalk
## NB - This step was assisted by a manual matching exercise
# df.xwalk_gf_cems <- readr::read_csv(here::here("data/xwalk/gf_cems_xwalk.csv"))
df.xwalk_gf_cems <- readxl::read_excel(here::here("data/xwalk/gf_cems_xwalk.xlsx"))


## (3e) Create GF-CEMS xwalk for Stata
## Prepare xwalk dataframe
df.xwalk <- df.xwalk_epa_eia %>%
  rename(ORISPL = CAMD_PLANT_ID,
         CEMS_UNIT = CAMD_UNIT_ID) %>%
  group_by(ORISPL, CEMS_UNIT) %>%
  summarise(CAMD_CAPACITY = sum(CAMD_CAPACITY, na.rm=TRUE),
            EIA_CAPACITY = sum(EIA_CAPACITY, na.rm=TRUE),
            .groups="drop") %>%
  ungroup() %>%
  right_join(df.xwalk_gf_cems, by=c("ORISPL","CEMS_UNIT")) %>%
  relocate(GF_BOILER, .after=ORISPL)

## Save dta
haven::write_dta(df.xwalk, 
                 here::here("data/xwalk/gf_cems_xwalk.dta"),
                 version=14.2)


## (3f) Re-create yearly CEMS data for Stata
## Merge dataframes, including the df.cems_yr built in step (2)
df.cems_yr <- readr::read_csv(here::here("data/cems_yr.csv")) %>%
  inner_join(df.xwalk_gf_cems %>% rename(UNIT = CEMS_UNIT),
             by=c("ORISPL","UNIT")) %>%
  relocate(GF_BOILER, .after=UNIT)

## Save dta
haven::write_dta(df.cems_yr, 
                 here::here("data/cems_yr.dta"),
                 version=14.2)


# (4) QUERY CEMS -- HOURS -------------------------------------------------------------------------
## This section prepares CEMS hourly data.

## (4a) Create list of years
l.hrs <- seq(0, 23) %>%
  as.list()


## (4b) Create hour iteration function
itr_hour <- function(hr) {
  
  ## Assertions
  if (!exists("df.xwalk_gf_cems")) {
    stop("Dataframe df.xwalk_gf_cems does not exist.")
  }
  stopifnot(
    is.integer(hr),
    hr>=0 & hr<=23
  )
  
  ## Send query
  cat("\nHour = ", hr, " ...\n  Send query\n", sep="")
  res <- glue::glue_sql("
    select 		ORISPL,
  			      UNIT,
  			      hour(DATETIME) as HOUR,
  			      DATETIME,
              DURATION,
  		        GLOAD,
  		        SLOAD,
        			CO2_MASS,
        			SO2_MASS,
        			NOX_MASS
    from		  epa.cems
    where     hour(DATETIME)={hr}
              and year(DATETIME)<=2017
    ;", .con=con) %>% 
    DBI::SQL() %>% 
    {DBI::dbSendQuery(con, .)}
  
  ## Fetch query
  cat("  Fetch query\n")
  df <- DBI::dbFetch(res, n=-1)
  DBI::dbClearResult(res)
  
  ## Clean query
  cat("  Clean query\n")
  df <- df %>%
    inner_join(df.xwalk_gf_cems %>% rename(UNIT = CEMS_UNIT), by=c("ORISPL","UNIT")) %>%
    relocate(GF_BOILER, .after=UNIT)
  
  ## Save query
  cat("  Save query\n")
  hr_label <- ifelse(str_length(hr)==1, paste0("0", hr), hr)
  saveRDS(df, fs::path(here::here(), glue::glue("data/epa/cems_hr{hr_label}.rds")))
  # write.csv(df, fs::path(here::here(), glue::glue("data/epa/cems_hr{hr_label}.csv")))
  return(df)
}


## (4c) Query yearly data
## Send queries - hourly files only
purrr::walk(l.hrs, itr_hour)

## Send queries - hourly files and aggregate file 
# df.cems_hr <- purrr::map_dfr(l.hrs, itr_hour) %>%
#   arrange(ORISPL, UNIT, DATETIME)

## Disconnect from MySQL
dbDisconnect(con)


# ## (4d) Save dataframe as rds & csv
# saveRDS(df.cems_hr, here::here("data/cems_hr.rds"))
# readr::write_csv(df.cems_hr, here::here("data/cems_hr.csv"))


# APPENDIX -- NETGEN ------------------------------------------------------------------------------
## This section prepares the net-gross generation ratio using EIA-923 data for the net component 
## and CEMS data for the gross.

## Query hourly data for one year
## NB - Later versions could aggregate the query to the year-month level, if they could be joined
##      with the GF-CEMS xwalk.
res <- glue::glue_sql("
    select 		ORISPL,
  			      UNIT,
  			      year(DATETIME) as YEAR,
  			      DATETIME,
              DURATION,
  		        GLOAD,
  		        SLOAD,
        			HEAT,
        			CO2_MASS,
        			SO2_MASS,
        			NOX_MASS
    from		  epa.cems
    where     year(DATETIME)=2010
    ;", .con=con) %>% 
  DBI::SQL() %>% 
  {DBI::dbSendQuery(con, .)}
df.cems <- DBI::dbFetch(res, n=-1)
DBI::dbClearResult(res)

## Filter on GF boilers
df.cems <- df.cems %>%
  inner_join(df.xwalk_gf_cems %>% rename(UNIT = CEMS_UNIT), by=c("ORISPL","UNIT")) %>%
  relocate(GF_BOILER, .after=UNIT)

## Import EIA net generation data
df.netgen <- read_csv(here::here("eia_netgen.csv")) %>%
  inner_join(df.xwalk_gf_cems %>% distinct(ORISPL), by=c("ORISPL")) %>%
  filter(YR==2010)

## Check net-gross monthly generation ratio
## NB - There appears to be quite a few instances where the ratio is either greater than one
##      or equal to zero.  These are likely a result of poor matching between boilers within
##      plants and should be checked.
df.cems_mth <- df.cems %>%
  rename(YR = YEAR) %>%
  mutate(MTH = month(DATETIME)) %>%
  group_by(ORISPL, YR, MTH) %>%
  summarise_at(vars(GLOAD, SLOAD), sum, na.rm=TRUE) %>%
  left_join(df.netgen %>% select(ORISPL, YR, MTH, NETGEN), by=c("ORISPL","YR","MTH")) %>%
  mutate(RATIO = NETGEN/GLOAD)
  

### END CODE ###

