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
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  26Aug2020 Jack Gregory  New draft; ...


### START CODE ###


# (1) PREAMBLE ------------------------------------------------------------------------------------

## (1?) Alternative package initiation
# ## Install magrittr
# if (!("magrittr" %in% installed.packages())) install.packages("magrittr")
# library(magrittr)
# 
# ## Initiate 
# ## ... source files
# fs::dir_ls(fs::path(here::here(), "src")) %>%
#   as.list() %>%
#   purrr::set_names(., nm=purrr::map_chr(., ~fs::path_ext_remove(fs::path_file(.x)))) %>%
#   .[c("preamble","def_paths", setdiff(names(.), c("preamble","def_paths")))] %>%
#   purrr::walk(source)


## (1a) Initiate 
## ... Packages
source(fs::path(here::here(), "src/preamble.R"))

## ... Paths
source(fs::path(here::here(), "src/def_paths.R"))
l.path <- append(l.path, list(cems = "E:/My Data/EPA/CEMS"))

## ... Functions
source(fs::path(l.path$src, "find_newfiles.R"))
source(fs::path(l.path$src, "sql_data_transfer.R"))

## ... Data
df.gf <- read_csv(path(l.path$data, "all_years_all_plants_and_features.csv")) %>%
  distinct(ORISPL = plant_code) %>%
  arrange(ORISPL)


## (1b) Connect to MySQL burbank_com database
con <- DBI::dbConnect(RMySQL::MySQL(), 
                      user="root", 
                      password="blackberries22-", 
                      dbname="epa", 
                      host="localhost")


# (2) QUERY CEMS ----------------------------------------------------------------------------------

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
  saveRDS(df, fs::path(here::here(), glue::glue("data/epa/cems_{yr}.rds")))
  return(df)
}


## (2c) Query yearly data
## Send query
df.cems_yr <- purrr::map_dfr(l.yrs, itr_year) %>%
  arrange(ORISPL, UNIT, YEAR)


## (2d) Save dataframe as csv
readr::write_csv(df.cems_yr, here::here("data/cems_yr.csv"))


# (3) UNIT MATCHES --------------------------------------------------------------------------------

## (3a) Access MySQL crosswalk
res <- DBI::dbSendQuery(con, "
    select distinct *
    from            epa.xwalk
  ;")
df.xwalk_epa_eia <- DBI::dbFetch(res, n=-1)
DBI::dbClearResult(res)

## Disconnect from MySQL
dbDisconnect(con)


## (3b) Join GF & CEMS units
## Import grandfathering dataset and check ORISPL & UNIT matches
df.gf <- read_csv(path(l.path$data, "gf_original/in_regression_vars_BP.csv"), guess_max=50000) %>%
  select(plant_code, boiler_id, year, plant_name, plt_county) %>%
  filter(year>=1995) %>%
  mutate_at(vars(plant_name, plt_county), str_to_upper) %>%
  group_by(plant_code, boiler_id) %>%
  fill(plant_name, plt_county, .direction="downup") %>%
  ungroup() %>%
  distinct(ORISPL = plant_code, 
           BOILER = boiler_id, 
           PLANT_NAME = plant_name, 
           COUNTY = plt_county) %>%
  arrange(ORISPL, BOILER) %>%
  group_by(ORISPL, BOILER, COUNTY) %>%
  summarise_all(last) %>%
  ungroup() %>%
  mutate(BOILER = as.character(BOILER),
         SOURCE = "GF") %>%
  full_join(df.cems_yr %>% distinct(ORISPL, BOILER = UNIT) %>% mutate(SOURCE = "CEMS"),
            by=c("ORISPL","BOILER")) %>%
  mutate(MATCH = (!is.na(SOURCE.x) & !is.na(SOURCE.y))) %>%
  left_join(df.xwalk %>% 
              select(-ID, -starts_with("EPA")) %>% 
              rename(ORISPL = CAMD_PLANT_ID, BOILER = CAMD_UNIT_ID),
            by=c("ORISPL","BOILER")) %>%
  arrange(ORISPL, BOILER) #%>%
  # group_by(MATCH) %>%
  # group_split()


## (3c) Save dataframe as csv
readr::write_csv(df.gf, here::here("data/gf_matches_full.csv"))


## (3d) Import GF-CEMS xwalk
## NB - This step was assisted by a manual matching exercise
df.xwalk_gf_cems <- readr::read_csv(here::here("data/gf_cems_xwalk.csv"))


## ------------------------------------------------------------------------------------------------

library(haven)

## Create GF-CEMS xwalk for Stata
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

haven::write_dta(df.xwalk, 
                 here::here("data/gf_cems_xwalk.dta"),
                 version=15)

## Create yearly CEMS data for Stata
df.cems_yr <- readr::read_csv(here::here("data/cems_yr.csv")) %>%
  inner_join(df.xwalk_gf_cems %>% rename(UNIT = CEMS_UNIT),
             by=c("ORISPL","UNIT")) %>%
  relocate(GF_BOILER, .after=UNIT)

haven::write_dta(df.cems_yr, 
                 here::here("data/cems_yr.dta"),
                 version=15)


### END CODE ###

