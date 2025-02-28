## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Plots -- Coef Year
## Jack Gregory
## 25 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# DEFINE ------------------------------------------------------------------------------------------

## Initiate
## ... Functions
source(here("src/stata_regs.R"))

## ... Date
date <- format(Sys.Date(), "%Y%m%d")
dir_create(here("out", date))


# YEARLY COEFFICIENT PLOTS ------------------------------------------------------------------------

## Build specifications -------------------------------------------------------
## NB - Here we use the Column (3) specification within the "main" regression 
##      table.  This is an OLS reg restricted to ut_type ∈ [2,4,5].

## Left-hand side vars
l.lhs <- list(
  "DURATION",
  "survive",
  "SO2"
)

## Right-hand side vars
ctrl_boiler <- "age capacity capacity_gf efficiency_100_pct_load"
ctrl_env <- "so2_nonattain so2_nonat_Gf applic_reg applic_reg_Gf"
fes <- "i.year 1.Gf#i.year i.states i.ut_type i.manufact"
ctrl_mkt <- "state_cap_growth coal2gas_price d_growth"

l.rhs <- list(
  glue("{ctrl_boiler} {ctrl_env} {fes}")
)

## Regression conditions
l.cond <- list(
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)"
)
cond_survive <- "& ((capacity>0.075 & ut_type==4) | ((ut_type==5|ut_type==2) & year>=1990)) & year<=2017"

## Regression type
l.type <- list(
  "reg"
)


## Perform regressions --------------------------------------------------------

## Build regression dataframe
df.reg <- tibble(l.type, l.rhs, l.cond) %>%
  rowid_to_column(var="model_id") %>%
  nest(data=everything()) %>%
  mutate(N = length(l.lhs)) %>%
  uncount(N) %>%
  bind_cols(tibble(l.lhs), .) %>%
  unnest(cols=c(l.lhs, data)) %>%
  rename_with(.fn=~str_replace(., "l\\.", "")) %>%
  
  ## Include market controls
  mutate(ctrl = ifelse(lhs!="SO2", ctrl_mkt, ""),
         cond = as.character(cond),
         sample = ifelse(lhs=="survive", cond_survive, ""),
         fml = paste(lhs, rhs, ctrl, cond, sample, sep=" ")) %>%
  
  ## Run regressions
  mutate_at(vars(-model_id), as.character) %>%
  mutate(bs = ifelse(type=="iv", TRUE, FALSE)) %>%
  mutate(model = pmap(list(fml, type, bs),
                      ~stata_reg(fml=..1, df=df.gf, type=..2, bootstrap=..3)),
         model = map(model, 
                     ~select(.x, var, coef, stderr, tstat, pval, ci_lower, ci_upper, 
                             N, r2, r2_a))) %>%
  unnest(model)


## Build plots ----------------------------------------------------------------

## Define plot names
l.name <- list("utilization","survival","emissions")

## Build yearly regression plots
p.coef_yr <- map2(
  l.lhs,
  list("Hours under load ['000 hrs]","Survival probability [pct pts]","Emissions [SO2 lbs per MW of capacity\nper hour run]"),
  ~{df.reg %>%
      filter(lhs==.x) %>%
      filter(str_detect(var, "^1.Gf.*#[0-9]{4}")) %>%
      mutate(year = str_extract(var, "[0-9]{4}") %>% as.numeric()) %>%
      mutate_at(vars(coef, starts_with("ci_")), ~ifelse(lhs=="DURATION", ./10^3, .)) %>%
      ggplot() +
        geom_hline(yintercept=0, size=0.3, color="#1A1A1A") +
        geom_ribbon(aes(x=year, ymin=ci_lower, ymax=ci_upper), alpha=0.2, fill="#2dCFAC") +
        geom_line(aes(x=year, y=coef), linewidth=0.5, color="#20A387") +
        geom_point(aes(x=year, y=coef), size=1.5, color="#20A387") +
        labs(#title="Grandfathering yearly coefficients",
             y=.y,
             x="Year"#,
             #caption="Shaded area represents the 95% confidence interval."
             ) +
        theme_classic() +
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_line(color="grey85", size=0.3),
              plot.caption = element_text(hjust=0))
  }) %>%
  set_names(nm=unlist(l.name))

## Save plots
walk2(l.name,
      list("a","b","c")
     ~ggsave(here("out", date, glue("fig4{.y}.pdf")),
             plot=p.coef_yr[[.x]],
             width=8, height=3.5, units="in"))


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl)_"))
rm(list=ls(pattern="^ctrl_"), fes)
rm(l.lhs, l.rhs, l.cond, l.type, l.name)


### END CODE ###

