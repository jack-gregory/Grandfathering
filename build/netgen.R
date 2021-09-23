## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## NetGen
## Jack Gregory
## 23 September 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script calculates the the net-to-gross generation ratio (GR) for as many grandfathered
## boilers as possible.  This is necessary due to the fact that EPA CEMS reports gross
## generation at hourly intervals.  We can estimate the GR by using EIA-923 net generation
## data reported at monthly intervals.

## There are two ways to calculate the net-to-gross-generation ratio:
##  (1) EIA-923 -- Page 1 Generation and Fuel Data -- 1997-2000 & 2002-present
##      This worksheet reports net generation by ORISPL, prime mover, fuel and month.  Since 
##      some units cannot be uniquely identified by only prime mover and fuel, it is necessary 
##      to calculate the GR at the ORISPL level for these facilities across the EPA CEMS
##      and EIA-923 data.

##  (2) EIA-923 -- Page 4 Generator Data -- 2008-present
##      This worksheet report net generation by ORISPL, generator and month.  This allows units
##      to be uniquely identified across the EPA CEMS and EIA-923 datasets through the EPA-EIA 
##      crosswalk.  Nevertheless, difficulties can arise here due to non-sequential generator-
##      boiler setups.

## Below we mainly focus on option (2); although, we highlight script that is meant for option (1).


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  23Sep2021 Jack Gregory  Initial version
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  26Aug2020 Jack Gregory  New draft; ...


### START CODE ###


# (1) PREAMBLE ------------------------------------------------------------------------------------

## Initiate 
## ... Packages
source(here::here("src/preamble.R"))

install.packages(setdiff(c("stargazer"), rownames(installed.packages())))
library(stargazer)

## ... Functions
source(here("src/def_paths.R"))

## ... Files
l.file <- list(
  epa_eia_xwalk = path(l.path$data, "epa/epa_eia_crosswalk.csv"),
  gf_cems_xwalk = path(l.path$data, "gf_cems_xwalk.xlsx"),
  epa = path(l.path$data, "epa/Facility_Attributes.zip"),
  gf= path(l.path$data, "gf_original/regressions_ready_data.dta"),
  netgen = path(l.path$data, "eia_netgen.csv"),
  netgen_unit = path(l.path$data, "eia_netgen_unit.csv")
)


# (2) IMPORT --------------------------------------------------------------------------------------

## (2a) Import crosswalks
## Import GF-CEMS xwalk
df.xwalk_gf_cems <- readxl::read_excel(l.file$gf_cems_xwalk) %>%
  rename(GF_BOILER_ID = GF_BOILER,
         EPA_UNIT_ID = CEMS_UNIT)

## Import EPA-EIA xwalk
df.xwalk_epa_eia <- readr::read_csv(l.file$epa_eia_xwalk) %>%
  select(-PLANT_ID_CHANGE_FLAG, -starts_with("MOD"), -starts_with("MATCH")) %>%
  rename_with(.fn=~str_replace(., "CAMD", "EPA"))


## (2b) Import EPA facility data
## Unzip EPA facility data
epa_file <- zip::zip_list(l.file$epa) %>%
  filter(str_detect(filename, "^facility")) %>%
  pull(filename)
zip::unzip(l.file$epa, file=epa_file, exdir=path(l.path$data, "epa"))

## Import EPA coordinate data
df.epa <- read_csv(path(l.path$data, "epa", epa_file)) %>%
  select(ORISPL = `Facility ID (ORISPL)`, 
         EPA_UNIT_ID = `Unit ID`, 
         YR = Year,
         FUEL = `Fuel Type (Primary)`,
         FUEL2 = `Fuel Type (Secondary)`) %>%
  filter(YR>=1995) %>%
  arrange(ORISPL, EPA_UNIT_ID, YR)

## Remove EPA facility data
fs::file_delete(path(l.path$data, "epa", epa_file))
rm(epa_file)


## (2c) Import GF data
## Import GF data
df.gf <- read_dta(l.file$gf) %>%
  filter(!(year<1985 | year==2006 | year>2017)) %>%
  filter(plant_code!=10673) %>%                        ## Unit is in HI
  select(ORISPL = plant_code,
         GF_BOILER_ID = boiler_id,
         YR = year,
         GF = grand_NSR_const,
         OWNER_CODE = ut_type,
         INSERVICE = inservice_y,
         AGE = age,
         CAP = max_boi_nameplate,
         SULFUR = sulfur_content_tot,
         SULFUR_DIST = sulfur_dist)

## Extract utility type definitions
owner_type <- attributes(df.gf$OWNER_CODE)$labels
df.owner <- tibble(
  OWNER_CODE = owner_type,
  OWNER = attributes(owner_type)$names
)

## Clean GF data
df.gf <- df.gf %>%
  left_join(df.owner, by=c("OWNER_CODE")) %>%
  select(ORISPL, GF_BOILER_ID, YR, GF, OWNER, INSERVICE, AGE, CAP, SULFUR, SULFUR_DIST)
rm(owner_type, df.owner)

## Check GF indicator
if (!all(distinct(df.gf, GF) %>% pull() %in% c(0,1))) {
  stop("GF variable is not a zero-one indicator.  Check df.gf dataframe.")
}


## (2d) Import net generation data
## Import EIA-923 netgen data
# df.netgen <- readRDS(fs::path_ext_set(l.file$netgen, "rds"))

## Import EIA-923 netgen-unit data
df.netgen_unit <- readRDS(fs::path_ext_set(l.file$netgen_unit, "rds")) %>%
  rename(EIA_GENERATOR_ID = GENERATOR_ID)


# (3) BUILD XWALK ---------------------------------------------------------------------------------

## (3a) Crosswalk for netgen
# ## Create combined xwalk; option (1)
# df.xwalk <- df.xwalk_gf_cems %>%
#   rename(CAMD_UNIT_ID = CEMS_UNIT) %>%
#   full_join(df.xwalk_epa_eia %>%
#               select(ORISPL = CAMD_PLANT_ID, 
#                      EIA_PLANT_ID, CAMD_UNIT_ID, EIA_BOILER_ID, CAMD_GENERATOR_ID, 
#                      EIA_GENERATOR_ID, EIA_UNIT_TYPE, CAMD_FACILITY_NAME, EIA_PLANT_NAME),
#             by=c("ORISPL","CAMD_UNIT_ID")) %>%
#   relocate(EIA_PLANT_ID, .after=ORISPL) %>%
#   group_by(ORISPL) %>%
#   mutate(GF = ifelse(!is.na(GF_BOILER), 1, NA)) %>%
#   fill(GF, .direction="downup") %>%
#   filter(GF==1) %>%
#   arrange(ORISPL, GF_BOILER) %>%
# 
#   group_by(ORISPL) %>%
#   mutate(GF = ifelse(is.na(GF_BOILER), 0, NA)) %>%
#   fill(GF, .direction="downup") %>%
#   filter(GF==0) %>%
#   mutate(N = n_distinct(EIA_UNIT_TYPE, GF)) %>%
#   ungroup()


## (3b) Crosswalk for netgen-unit
## Create combined xwalk; option (2)
l.xwalk <- df.xwalk_gf_cems %>%
  left_join(df.xwalk_epa_eia %>%
              select(ORISPL = EPA_PLANT_ID, EIA_PLANT_ID, EPA_UNIT_ID, EIA_BOILER_ID,
                     EPA_GENERATOR_ID, EIA_GENERATOR_ID, EPA_FACILITY_NAME, EIA_PLANT_NAME),
            by=c("ORISPL","EPA_UNIT_ID")) %>%
  relocate(EIA_PLANT_ID, .after=ORISPL) %>%
  
  group_by(ORISPL, GF_BOILER_ID) %>%
  mutate(N_GENERATOR = n()) %>%
  group_by(ORISPL, EIA_GENERATOR_ID) %>%
  mutate(N_BOILER = n()) %>%
  group_by(ORISPL) %>%
  mutate(MISS = ifelse(is.na(EIA_PLANT_ID), 1, NA)) %>%
  fill(MISS, .direction="downup") %>%
  mutate(GROUP = case_when(MISS==1 ~ "Missing",
                           N_GENERATOR>1 | N_BOILER>1 ~ "Multiple",
                           TRUE ~ "Clean")) %>%
  group_by(GROUP) %>%
  group_split() %>%
  set_names(nm=c("Clean","Missing","Multiple"))

## Extract clean matches only
## NB -- We're being very parsimonious here in order to ensure clean matches:
##        (1) We only take "clean" matches (i.e. one-to-one boiler-to-generator); and,
##        (2) We only take years when the primary fuel is equal to "Coal".
##       These could both be relaxed in the future.
df.xwalk <- l.xwalk$Clean %>%
  select(-N_GENERATOR, -N_BOILER, -MISS, -GROUP) %>%
  left_join(df.epa %>%
              select(ORISPL, EPA_UNIT_ID, YR, FUEL), 
            by=c("ORISPL","EPA_UNIT_ID")) %>%
  filter(FUEL=="Coal")

# ## Cleaning for Multiple???
#   distinct(ORISPL, GF_BOILER, EPA_UNIT_ID, N) %>%
#   full_join(df.xwalk_epa_eia %>%
#               select(ORISPL = EPA_PLANT_ID, 
#                      EIA_PLANT_ID, EPA_UNIT_ID, EIA_BOILER_ID, EPA_GENERATOR_ID, 
#                      EIA_GENERATOR_ID, EIA_UNIT_TYPE, EPA_FACILITY_NAME, EIA_PLANT_NAME),
#             by=c("ORISPL","EPA_UNIT_ID")) %>%
#   relocate(EIA_PLANT_ID, N, .after=ORISPL) %>%
#   group_by(ORISPL) %>%
#   mutate(GF = ifelse(!is.na(GF_BOILER), 1, NA)) %>%
#   fill(GF, .direction="downup") %>%
#   filter(GF==1) %>%
#   arrange(ORISPL, GF_BOILER)


# (4) BUILD GR ------------------------------------------------------------------------------------

## (4a) Create precursors
## Build years list based on net generation data
l.yrs <- df.netgen_unit %>%
  filter(!(YR<1985 | YR==2006 | YR>2017)) %>%
  distinct(YR) %>%
  arrange(YR) %>%
  pull() %>%
  as.list()

## CEMS query function
query_cems <- function(yr, con) {
  
  ## Assertions
  ## NB - Future versions should perform assertr checks on the df.xwalk below to ensure
  ##      proper content and structure.
  if (!exists("df.xwalk")) {
    stop("The dataframe df.xwalk is missing from the global environment.")
  }
  stopifnot(
    yr>=1997 & yr<=2025,
    DBI::dbIsValid(con)
  )
  
  cat("\n", yr, " ...\n", sep="")
  
  ## Query CEMS data for single year from MySQL epa database
  ## NB - Future versions could aggregate the query to the year-month level, if they could 
  ##      be joined with the df.xwalk dataframe.  This could be achieved by using temporary 
  ##      tables, see <.../src/sql_data_transfer.R>.
  cat("  Query CEMS\n")
  res <- glue::glue_sql("
  select 		ORISPL,
			      UNIT as EPA_UNIT_ID,
			      year(DATETIME) as YR,
			      DATETIME,
			      DURATION,
		        GLOAD,
		        SLOAD,
		        HEAT
  from		  epa.cems
  where     year(DATETIME)={yr}
  ;", .con=con) %>% 
    DBI::SQL() %>% 
    {DBI::dbSendQuery(con, .)}
  df <- DBI::dbFetch(res, n=-1)
  DBI::dbClearResult(res)
  
  ## Aggregate over months
  cat("  Aggregate to yr-mths\n")
  df %>%
    
    ## Filter on GF boilers
    inner_join(df.xwalk %>% 
                 select(ORISPL, EPA_UNIT_ID, GF_BOILER_ID, EIA_GENERATOR_ID, YR), 
               by=c("ORISPL","EPA_UNIT_ID","YR")) %>%
    
    ## Generate monthly CEMS  
    mutate(MTH = month(DATETIME)) %>%
    group_by(ORISPL, EPA_UNIT_ID, GF_BOILER_ID, EIA_GENERATOR_ID, YR, MTH) %>%
    summarise_at(vars(DURATION, GLOAD, SLOAD, HEAT), sum, na.rm=TRUE)
}


## (4b) Query MySQL epa.cems table
## Connect to MySQL
con <- DBI::dbConnect(RMySQL::MySQL(), 
                      user="root", 
                      password="blackberries22-", 
                      dbname="epa", 
                      host="localhost")


## Query CEMS
df.cems <- map_dfr(l.yrs, ~query_cems(yr=.x, con=con))

## Disconnect from MySQL
DBI::dbDisconnect(con)


## (4c) Build GR dataframe
df.gr <- df.cems %>%
  
  ## Merge net generation and grandfathering data
  left_join(df.netgen_unit %>% 
              select(ORISPL, EIA_GENERATOR_ID, YR, MTH, NETGEN), 
            by=c("ORISPL","EIA_GENERATOR_ID","YR","MTH")) %>%
  left_join(df.gf, by=c("ORISPL","GF_BOILER_ID","YR")) %>%
  
  ## Fill missing rows
  arrange(ORISPL, GF_BOILER_ID, YR, MTH) %>%
  group_by(ORISPL, GF_BOILER_ID) %>%
  fill(GF, OWNER, INSERVICE, CAP, SULFUR_DIST) %>%
  mutate(AGE = ifelse(is.na(AGE), YR - INSERVICE, AGE)) %>%
  ungroup() %>%
  filter(!is.na(GF)) %>%
  
  ## Calculate GR
  ## NB - There appears to be quite a few instances where the ratio is either greater than one
  ##      or equal to zero.  These are likely a result of poor matching between boilers within
  ##      plants.
  mutate(GR = NETGEN/GLOAD,
         GR = ifelse(GR>0 & GR<1, GR, NA)) %>%
  
  ## Finalize dataframe
  select(ORISPL, GF_BOILER_ID, EPA_UNIT_ID, EIA_GENERATOR_ID, GF, YR, MTH, DURATION, 
         GLOAD, SLOAD, NETGEN, GR, OWNER, AGE, CAP, HEAT, SULFUR, SULFUR_DIST) %>%
  arrange(ORISPL, GF_BOILER_ID, YR, MTH)


# (5) ANALYSIS ------------------------------------------------------------------------------------


## (5a) Check GR weighted average by DURATION
df.gr %>%
  filter(!is.na(GR)) %>%
  group_by(GF) %>%
  summarise(N = n(),
            GR = sum(DURATION*GR)/sum(DURATION))


## (5b) Perform regressions
## List specifications
l.spec <- list(
  Base = as.formula(GR ~ GF),
  `+Chars` = as.formula(GR ~ GF + AGE + CAP),
  `+TimeFE` = as.formula(GR ~ GF + AGE + CAP + factor(YR) + factor(MTH)),
  `+Heat` = as.formula(GR ~ GF + AGE + CAP + HEAT),
  `+Sulfur` = as.formula(GR ~ GF + AGE + CAP + HEAT + SULFUR),
  `+OwnerFE` = as.formula(GR ~ GF + AGE + CAP + HEAT + SULFUR + factor(OWNER))
)

## Display regressions
map(l.spec,
    ~summary(lm(.x,
                data=df.gr,
                weights=DURATION)))

## Capture regressions
df.reg <- tibble(as.list(names(l.spec)),
                 l.spec) %>%
  select(name=`as.list(names(l.spec))`, formula=l.spec) %>%
  mutate(model = map(formula, ~lm(.x, data=df.gr, weights=DURATION)))


## (5c) Regression table
## Regression table function
regtbl <- function(df, tbl.type="text") {
  
  ## Assertions  
  if (!exists("l.path")) {
    stop("Missing data: l.path does not exist.")
  }
  stopifnot(
    is.data.frame(df),
    is.character(tbl.type),
    tbl.type %in% c("text","latex")
  )
  
  ## Define reg table vars
  if (tbl.type=="text") {
    title <- ""
    label <- ""
    out <- NULL
  } else if (tbl.type=="latex") {
    title <- "Net-to-gross generation ratio regressions"
    label <- "tbl:reg_gr"
    date <- format(Sys.Date(), "%Y%m%d")
    fs::dir_create(path(l.path$out, date))
    out <- fs::path(l.path$out, date, "tbl.reg_gr.tex")
  }
  
  add.lines <- df %>%
    transmute(age = map_chr(formula, ~ifelse(deparse(.) %>% str_detect("AGE"), "X", " ")),
              cap = map_chr(formula, ~ifelse(deparse(.) %>% str_detect("CAP"), "X", " "))) %>%
    add_row(age="Age",
            cap="Capacity",
            .before=1) %>%
    as.list()
  
  ## Build regression table
  stargazer(df$model,
            type = tbl.type,
            title = title,
            label = label,
            style = "aer",
            out = out,
            covariate.labels = c("Grandfathering"),
            dep.var.labels = NULL,
            dep.var.labels.include = FALSE,
            add.lines = add.lines,
            align = TRUE,
            column.sep.width = "0.7ex",
            df = FALSE,
            digits = 3,
            digits.extra = 3,
            font.size = "scriptsize",
            header = FALSE,
            keep = c("^GF$"),
            keep.stat = c("n", "rsq", "adj.rsq"),
            model.names = FALSE,
            model.numbers = TRUE,
            multicolumn = FALSE,
            notes = c("*** p$<$0.001;  ** p$<$0.01;  * p$<$0.05;",
                      "t-statistics in parentheses."),
            notes.append = FALSE,
            order = c("GF"),
            report = "vc*t",
            star.cutoffs = c(0.05, 0.01, 0.001),
            table.layout = "=#m-t-as=n",
            table.placement = "ht"
  )
}

## Build regtbl
regtbl(df.reg %>% slice(1:2))
regtbl(df.reg %>% slice(1:2), tbl.type="latex")


### END CODE ###

