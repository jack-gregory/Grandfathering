## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## CEMS Ingestion
## Jack Gregory
## 16 April 2020
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This program synthesizes all available US Environmental Protection Agency (EPA) Continuous
## Emissions Monitoring System (CEMS) data.  It transfers zipped csv files to the local MySQL epa 
## database.  It populates the cems table with data from the EPA and updates the filelog table.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version
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


## (1b) Create MySQL epa schema & tables, if necessary
## database
source(fs::path(l.path$src, "sql_db_epa.R"))

## tables
source(fs::path(l.path$src, "sql_tbl_filelog.R"))
source(fs::path(l.path$src, "sql_tbl_cems.R"))
source(fs::path(l.path$src, "sql_tbl_xwalk.R"))


## (1d) Connect to MySQL burbank_com database
con <- DBI::dbConnect(RMySQL::MySQL(), 
                      user="root", 
                      password="blackberries22-", 
                      dbname="epa", 
                      host="localhost")


# (2) REFRESH XWALK -------------------------------------------------------------------------------

## (2a) Access latest crosswalk
## Download
url <- paste0("https://github.com/USEPA/camd-eia-crosswalk/raw/master/epa_eia_crosswalk.csv")
download.file(url, destfile=fs::path(l.path$data, "epa/epa_eia_crosswalk.csv"))

## Import
df.xwalk_new <- readr::read_csv(fs::path(l.path$data, "epa/epa_eia_crosswalk.csv"))


## (2b) Access MySQL crosswalk
## Send query
res <- DBI::dbSendQuery(con, "
    select distinct CAMD_PLANT_ID, 
                    CAMD_UNIT_ID, 
                    EPA_FACILITY_ID, 
                    EPA_UNIT_ID
    from            epa.xwalk
  ;")
df.xwalk_old <- DBI::dbFetch(res, n=-1)
DBI::dbClearResult(res)


## (2c) Clean latest crosswalk
## Reduce vars and add EPA identifiers
df.xwalk_new <- df.xwalk_new %>%
  select(starts_with("CAMD"),
         starts_with("EIA")) %>%
  rename(CAMD_CAPACITY = CAMD_NAMEPLATE_CAPACITY,
         EIA_CAPACITY = EIA_NAMEPLATE_CAPACITY) %>%
  left_join(df.xwalk_old, by=c("CAMD_PLANT_ID","CAMD_UNIT_ID")) %>%
  relocate(starts_with("EPA"), .after=CAMD_UNIT_ID)


## (2d) Write data to MySQL epa.xwalk
DBI::SQL("TRUNCATE epa.xwalk;") %>% {DBI::dbExecute(con, .)}
DBI::dbWriteTable(con,
                  name="xwalk", 
                  value=df.xwalk_new, 
                  row.names=FALSE, append=TRUE)

rm(url, res, df.xwalk_new, df.xwalk_old)


# (3) INGEST CEMS ---------------------------------------------------------------------------------

## (3a) Get list of raw cems files
l.zip <- find_new_files(con, dir=l.path$cems) %>%
  as.list()


## (3b) Read raw & write cleaned cems data
## Iterate over files
purrr::walk(l.zip, ~cems_sql(con, .x, df.gf))

## Disconnect from MySQL
dbDisconnect(con)


### END CODE ###

