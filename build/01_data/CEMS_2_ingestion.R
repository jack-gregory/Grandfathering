## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## CEMS Ingestion
## Jack Gregory
## 16 April 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This program synthesizes all available US Environmental Protection Agency (EPA) Continuous
## Emissions Monitoring System (CEMS) data.  It transfers zipped csv files to the local MySQL epa 
## database.  It populates the cems table with data from the EPA and updates the filelog table.

## NB - The EPA-EIA Crosswalk should not necessarily be used as the storage vehicle for repeating
##      CEMS characteristics.  Instead, the MySQL database should be redesigned with three tables:
##        (1) EPA facility characteristics (accessible from the EPA website)
##        (2) CEMS timeseries (which already exists and does not require additional work)
##        (3) CEMS characteristics (which exists, but merged with the xwalk)
##        (4) EPA-EIA xwalk (which exists, but merged with CEMS characteristics)
##        (5) GF-CEMS xwalk?
##      The GitHub documentation for the xwalk notes that there may be multiple generators 
##      associated with one boiler or vice versa.  The authors recommend that data users trying to 
##      match information from both data sets first decide whether to collapse on boilers or 
##      generators within the crosswalk to avoid double counting after matching the two data sets.
##      Thus, a typical workflow would full_join CEMS timeseries and characteristics, and then 
##      left_join either or both of the GF and EPA xwalks depending on the task.
##      
##      The xwalk_write() performs an inner_join() of the ORISPL_CODE & UNITID variables from CEMS
##      with the CAMD_PLANT_ID & CAMD_UNIT_ID variables from the xwalk.  As such, there is the 
##      possibility that units in CEMS were not transferred to the MySQL epa.xwalk table.  In all
##      likelihood, the code should have been written with a right_join(), so as to ensure all
##      CEMS units were present in the xwalk / "characteristics" table.
##      
##      The CEMS characteristics table should include the following (as defined in the CEMS raw 
##      data): STATE, FACILITY_NAME, ORISPL_CODE, UNITID, OP_DATE, FAC_ID, UNIT_ID.

## NB - Before running, please update the l.path$cems element to reflect your local CEMS storage 
##      location.  See step (1a) below.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version


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

## ... Paths
l.path <- list(
  # cems = fs::path("E:/My Data/EPA/CEMS")
  cems = here::here("data/epa/cems")
)

## ... Functions
source(here::here("src/find_newfiles.R"))
source(here::here("src/sql_data_transfer.R"))

## ... Data
df.gf <- read_csv(here::here("data/regression_vars.csv")) %>%
  distinct(ORISPL = plant_code) %>%
  arrange(ORISPL)


## (1b) Create MySQL epa schema & tables, if necessary
## database
source(here::here("src/sql_db_epa.R"))

## tables
source(here::here("src/sql_tbl_filelog.R"))
source(here::here("src/sql_tbl_cems.R"))
source(here::here("src/sql_tbl_xwalk.R"))


## (1c) Connect to MySQL burbank_com database
con <- DBI::dbConnect(RMySQL::MySQL(),
                      user = Sys.getenv("MYSQL_USER"),
                      password = Sys.getenv("MYSQL_PASSWORD"),
                      dbname = Sys.getenv("MYSQL_DB"),
                      host = Sys.getenv("MYSQL_HOST"))


# (2) REFRESH XWALK -------------------------------------------------------------------------------

## (2a) Access latest crosswalk
## Download
url <- paste0("https://github.com/USEPA/camd-eia-crosswalk/raw/master/epa_eia_crosswalk.csv")
download.file(url, destfile=here::here("data/epa/epa_eia_crosswalk.csv"))

## Import
df.xwalk_new <- readr::read_csv(here::here("data/epa/epa_eia_crosswalk.csv"))


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

