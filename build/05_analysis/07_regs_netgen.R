## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Regs -- Netgen
## Jack Gregory
## 25 June 2024
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
##      This worksheet reports net generation by ORISPL, generator and month.  This allows units
##      to be uniquely identified across the EPA CEMS and EIA-923 datasets through the EPA-EIA 
##      crosswalk.  Nevertheless, difficulties can arise here due to non-sequential generator-
##      boiler setups.

## Below we mainly focus on option (2); although, we highlight code that is meant for option (1).


### START CODE ###


# (1) PREAMBLE ------------------------------------------------------------------------------------

## Initiate
## ... Packages
pkgs <- c(
  "sandwich","lmtest","stargazer"
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

## ... Date
date <- format(Sys.Date(), "%Y%m%d")
dir_create(here("out", date))

## ... Files
l.file <- list(
  epa_eia_xwalk = here("data/epa/epa_eia_crosswalk.csv"),
  gf_cems_xwalk = here("data/xwalk/gf_cems_xwalk.xlsx"),
  epa = here("data/epa/Facility_Attributes.zip"),
  gf = here("data/regressions_ready_data.dta"),
  netgen_unit = here("data/eia_netgen_unit.csv"),
  plant = here("data/eia_plant.csv"),
  tbl2 = here("out", date, "tbl2.tex")
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
         GF = Gf,
         OWNER_CODE = ut_type,
         INSERVICE = inservice_y,
         AGE = age,
         CAP = capacity,
         SULFUR = sulfur_content_tot,
         SULFUR_DIST = sulfur_net_iv)

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
## Import EIA-923 netgen-unit data
df.netgen_unit <- readRDS(fs::path_ext_set(l.file$netgen_unit, "rds")) %>%
  rename(EIA_GENERATOR_ID = GENERATOR_ID)


## (2e) Import plant data
## Import EIA-923 reporting frequency data (see sheet "Page 6 Plant Frame")
df.plant <- read_csv(l.file$plant) |>
  select(ORISPL, YR=YEAR, REPORTING)


# (3) BUILD XWALK ---------------------------------------------------------------------------------

## (3a) Crosswalk for netgen-unit
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


## (3b) Extract clean matches only
## NB -- We're being very parsimonious here in order to ensure clean matches:
##        (1) We only take "clean" matches (i.e. one-to-one boiler-to-generator); and,
##        (2) We only take years when the primary fuel is equal to "Coal".
##       These could both be relaxed in the future.
df.xwalk <- l.xwalk$Clean %>%
  select(-N_GENERATOR, -N_BOILER, -MISS, -GROUP) %>%
  left_join(df.epa %>%
              select(ORISPL, EPA_UNIT_ID, YR, FUEL), 
            by=c("ORISPL","EPA_UNIT_ID")) %>%
  left_join(df.plant, by=c("ORISPL","YR")) %>%
  fill(REPORTING, .direction="updown") %>%
  filter(FUEL=="Coal")


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
    mutate(YR = as.numeric(YR)) %>%
  
    ## Filter on GF boilers
    inner_join(df.xwalk %>% 
               select(ORISPL, EPA_UNIT_ID, GF_BOILER_ID, EIA_GENERATOR_ID, REPORTING, YR), 
               by=c("ORISPL","EPA_UNIT_ID","YR")) %>%
  
    ## Generate monthly CEMS  
    mutate(MTH = month(DATETIME)) %>%
    group_by(ORISPL, EPA_UNIT_ID, GF_BOILER_ID, EIA_GENERATOR_ID, REPORTING, YR, MTH) %>%
    summarise_at(vars(DURATION, GLOAD, SLOAD, HEAT), sum, na.rm=TRUE)
}


## (4b) Query MySQL epa.cems table
## Connect to MySQL
con <- DBI::dbConnect(RMySQL::MySQL(),
                      user = Sys.getenv("MYSQL_USER"),
                      password = Sys.getenv("MYSQL_PASSWORD"),
                      dbname = Sys.getenv("MYSQL_DB"),
                      host = Sys.getenv("MYSQL_HOST"))

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
  
  ## Create clustering ID
  mutate(ID = paste0(ORISPL, "|", GF_BOILER_ID)) %>%
  relocate(ID) %>%
  
  ## Finalize dataframe
  select(ID, ORISPL, GF_BOILER_ID, EPA_UNIT_ID, EIA_GENERATOR_ID, REPORTING, GF, YR, MTH, DURATION, 
         GLOAD, SLOAD, NETGEN, GR, OWNER, AGE, CAP, HEAT, SULFUR, SULFUR_DIST) %>%
  arrange(ORISPL, GF_BOILER_ID, YR, MTH)


## (4d) Save and import dataframe
## Write
# saveRDS(df.gr, here::here("data/df_gr.rds"))
## Read
# df.gr <- readRDS(here::here("data/df_gr.rds"))


# (5) MONTHLY ANALYSIS ----------------------------------------------------------------------------

## (5a) Check GR weighted average by DURATION
df.gr %>%
  filter(!is.na(GR)) %>%
  group_by(GF) %>%
  summarise(N = n(),
            GR = sum(DURATION*GR)/sum(DURATION))


## (5b) Perform naive linear regressions
## List specifications
l.spec <- list(
  Base = as.formula(GR ~ GF),
  `+Chars` = as.formula(GR ~ GF + AGE + CAP),
  `+Heat` = as.formula(GR ~ GF + AGE + CAP + HEAT),
  `+Sulfur` = as.formula(GR ~ GF + AGE + CAP + HEAT + SULFUR),
  `+OwnerFE` = as.formula(GR ~ GF + AGE + CAP + HEAT + SULFUR + factor(OWNER)),
  `+TimeFE` = as.formula(GR ~ GF + AGE + CAP + HEAT + SULFUR + factor(OWNER) + factor(YR) + factor(MTH))
)

## Regression function
## NB - The "cluster" parameter switches between normal and clustered standard errors
##    - The "print" parameter switches between printing the output and returning the model
lm_gr <- function(formula, data, cluster=FALSE, print=FALSE) {
  
  ## Assertions
  stopifnot(
    is_formula(formula),
    is.data.frame(data),
    is.logical(cluster),
    is.logical(print)
  )
  
  ## Linear model
  model <- lm(formula=formula, data=data, weights=DURATION)
  
  if (cluster) {
    vcov_cluster <- vcovHC(model, type="HC0", cluster=df.gr$ID)
    coef <- coeftest(model, vcov = vcov_cluster)
  }
  if (print) {
    if (!cluster) {
      summary(model)
    } else {
      print(coef)
    }
  } else {
    return(model)
  } 
}

## Display regressions
map(
  l.spec,
  ~lm_gr(formula=.x, data=df.gr, cluster=TRUE, print=TRUE)
)

## Capture regressions
df.reg <- tibble(as.list(names(l.spec)),
                 l.spec) %>%
  select(name=`as.list(names(l.spec))`, formula=l.spec) %>%
  mutate(model = map(formula, ~lm_gr(formula=.x, data=df.gr)),
         vcov = map(model, ~vcovHC(.x, type="HC0", cluster=df.gr$ID)))


## (5c) Regression table
## Regression table function
regtbl <- function(df, cluster=FALSE, tbl.type="text") {
  
  ## Assertions  
  if (!exists("l.path")) {
    stop("Missing data: l.path does not exist.")
  }
  stopifnot(
    is.data.frame(df),
    is.logical(cluster),
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
            cap="Size",
            .before=1) %>%
    as.list()
  
  if (cluster) {
    t_stat <- df %>%
      # Calculate clustered standard errors
      transmute(coef = map(model, ~coef(.x)),
                se_cluster = map(vcov, ~sqrt(diag(.x)))) %>%
      # Calculate t-statistics using clustered standard errors
      transmute(t = map2(coef, se_cluster, ~.x/.y)) %>%
      as.list() %>%
      flatten()
  } else {
    t_stat <- NULL
  }
  
  ## Build regression table
  stargazer(df$model,
            type = tbl.type,
            title = title,
            label = label,
            style = "aer",
            out = out,
            covariate.labels = c("Constant","Grandfathering"),
            dep.var.labels = NULL,
            dep.var.labels.include = FALSE,
            add.lines = add.lines,
            align = TRUE,
            t = t_stat,
            column.sep.width = "0.7ex",
            df = FALSE,
            digits = 3,
            digits.extra = 3,
            font.size = "scriptsize",
            header = FALSE,
            keep = c("^Constant$","^GF$"),
            keep.stat = c("n", "rsq", "adj.rsq"),
            model.names = FALSE,
            model.numbers = TRUE,
            multicolumn = FALSE,
            notes = c("*** p$<$0.01;  ** p$<$0.05;  * p$<$0.10;",
                      "t-statistics in parentheses."),
            notes.append = FALSE,
            order = c("Constant","GF"),
            report = "vc*t",
            star.cutoffs = c(0.10, 0.05, 0.01),
            table.layout = "=#m-t-as=n",
            table.placement = "ht"
  )
}

## Build regtbl
## NB -- Table 2 in paper.
# regtbl(df.reg %>% slice(1:2), cluster=TRUE)
regtbl(df.reg %>% slice(1:2), cluster=TRUE, tbl.type="latex")

## Caption
# This table reports results from two weighted least squares regressions based on 
# Equation (\ref{Eq:net-to-gross}).  The dependent variable is net-to-gross 
# generation ratio, while the weights are based on monthly durations.  Column (1)
# essentially represents the pure weighted average, while Column (2) presents one
# conditional on age and size. They rely on CEMS and EIA-923 data from 2008 to 2017.  
# Boiler-level clustered standard errors used, with *** p$<$0.01,  ** p$<$0.05,  
# * p$<$0.10 and \\textit{t}-statistics in parentheses.


### END CODE ###

