## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Plots -- Coef Hour
## Jack Gregory
## 26 September 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# DEFINE ------------------------------------------------------------------------------------------

## Create output folder
date <- format(Sys.Date(), "%Y%m%d")
dir_create(path(l.path$out, date))

## Create list of hourly CEMS files
l.cems <- path(l.path$data, "epa") %>%
  dir_ls() %>%
  keep(str_detect(., "hr\\d{2}\\.rds"))


# HOUR COEFFICIENT PLOTS --------------------------------------------------------------------------

## itr_hr ---------------------------------------------------------------------

## fml = formula as a list of stata variables concatenated by spaces
## df = dataframe containing regression data
## type = regression type in set {reg, iv}
## hr = hour as a two-digit string (i.e. "00", "01", etc.)

## Method set & use function
itr_hr <- function(fml, df, type, hr) {
  
  ## Assertions
  if (!exists("l.path")) {
    stop("The list l.path is not defined in the global environment.")
  }
  stopifnot(
    is.character(fml),
    is.data.frame(df),
    is.character(type),
    type %in% c("reg","iv"),
    is.character(hr),
    hr %in% (seq(0,23) %>% formatC(format="d", width=2, flag="0"))
  )
  
  cat("\nHOUR ", hr, " ...\n", sep="")
  
  ## Build dataframe
  fs::path(l.path$data, paste0("epa/cems_hr", hr, ".rds")) %>%
    readRDS() %>%
    mutate(YR = year(DATETIME),
           MTH = month(DATETIME),
           DOW = wday(DATETIME,  week_start=getOption("lubridate.week.start", 1))) %>%
    select(ORISPL, UNIT, DATETIME, YR, MTH, DOW, DURATION, SO2_MASS) %>%
    inner_join(df %>% select(-boiler_id, -DURATION, -survive, -SO2,
                             -contains("white"), -contains("income")), 
               by=c("ORISPL"="plant_code", "UNIT"="UNIT", "YR"="year")) %>%
    mutate(SO2 = SO2_MASS / (DURATION * max_boi_nameplate * 10^3)) %>%
    
    ## Perform regression
    {stata_reg(fml, ., type)}
}


## Build specifications -------------------------------------------------------

## Left-hand side vars
l.lhs <- list(
  "DURATION",
  "SO2"
)

## Right-hand side vars
ctrl_boiler <- "age max_boi_nameplate"
ctrl_env <- "so2_nonattain grand_NSR_in_nonnat_const applic_reg applic_reg_const ARPprice_sp"
ctrl_mkt <- "state_cap_growth coal2gas_price d_growth"
fes <- "i.DOW i.MTH i.YR i.states i.ut_type"

l.rhs <- list(
  glue("grand_NSR_const {ctrl_boiler} capacity_gf {ctrl_env} {fes}")
)

## Regression conditions
l.cond <- list(
  ""
)

## Regression type
l.type <- list(
  "iv"
)


## Perform regressions --------------------------------------------------------

## Build regression dataframe
df.reg_hr <- tibble(l.type, l.rhs, l.cond) %>%
  mutate(N = 24) %>%
  uncount(N) %>%
  rowid_to_column(var="hr") %>%
  mutate(hr = formatC(hr - 1, format="d", width=2, flag="0")) %>%
  nest(data=everything()) %>%
  bind_cols(tibble(l.lhs), .) %>%
  unnest(cols=c(l.lhs, data)) %>%
  rename_with(.fn=~str_replace(., "l\\.", "")) %>%
  
  ## Build formulae
  mutate(ctrl = ifelse(lhs!="SO2", ctrl_mkt, ""),
         cond = as.character(cond),
         fml = paste(lhs, rhs, ctrl, cond, sep=" ")) %>%
  
  ## Run regressions
  mutate_all(as.character) %>%
  # slice(2:3) %>%
  mutate(model = pmap(list(fml, type, hr),
                      ~itr_hr(..1, df.gf, ..2, ..3)),
         model = map(model, 
                     ~select(.x, var, coef, stderr, tstat, pval, ci_lower, ci_upper, 
                             N, r2, r2_a))) %>%
  unnest(model)


## Build plots ----------------------------------------------------------------

## Define plot names
l.name <- list("utilization","emissions")

## Build hourly regression plots
p.coef_hr <- map2(
  l.lhs,
  list("Hours under load [hrs]","Emissions [SO2 lbs/MWh]"),
  ~df.reg_hr %>%
    filter(lhs==.x) %>%
    filter(str_detect(var, "^grand_NSR_const$")) %>%
    mutate(hr = as.numeric(hr)) %>%
    ggplot() +
      geom_hline(yintercept=0, size=0.3, color="#1A1A1A") +
      geom_ribbon(aes(x=hr, ymin=ci_lower, ymax=ci_upper), alpha=0.2, fill="#2dCFAC") +
      geom_line(aes(x=hr, y=coef), size=0.5, color="#20A387") +
      geom_point(aes(x=hr, y=coef), size=1.5, color="#20A387") +
      labs(#title="Grandfathering yearly coefficients",
           y=.y,
           x="Hour"#,
           #caption="Shaded area represents the 95% confidence interval."
           ) +
      theme_classic() +
      theme(axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_line(color="grey85", size=0.3),
            panel.grid.minor.y = element_line(color="grey85", size=0.3),
            plot.caption = element_text(hjust=0))
  ) %>%
  set_names(nm=unlist(l.name))

## Save plots
walk(l.name, 
     ~ggsave(path(l.path$out, date, glue("fig.coefhr_{.x}.pdf")),
             plot=p.coef_hr[[.x]],
             width=8, height=4, units="in"))


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl)_"))
rm(list=ls(pattern="^ctrl_"), fes)
rm(l.lhs, l.rhs, l.cond, l.type, l.name)


### END CODE ###

