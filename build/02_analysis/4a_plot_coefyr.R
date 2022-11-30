## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Plots -- Coef Year
## Jack Gregory
## 26 September 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# DEFINE ------------------------------------------------------------------------------------------

## Create output folder
date <- format(Sys.Date(), "%Y%m%d")
# date <- "20211123"
dir_create(path(l.path$out, date))


# YEARLY COEFFICIENT PLOTS ------------------------------------------------------------------------

## Build specifications -------------------------------------------------------
## NB - Here we use the Column (5) specification within the "main" regression 
##      table.  This is an IV reg restricted to ut_type ∈ [2,4,5].

## Left-hand side vars
l.lhs <- list(
  "DURATION",
  "survive",
  "SO2"
)

## Right-hand side vars
ctrl_boiler <- "age max_boi_nameplate"
ctrl_env <- "so2_nonattain grand_NSR_in_nonnat_const applic_reg applic_reg_const ARPprice_sp"
ctrl_mkt <- "state_cap_growth coal2gas_price d_growth"
fes <- "i.year 1.grand_NSR_const#i.year i.states i.ut_type"

l.rhs <- list(
  glue("grand_NSR_const {ctrl_boiler} capacity_gf {ctrl_env} {fes}")
)

## Regression conditions
l.cond <- list(
  "if (ut_type==4 | ut_type==5 | ut_type==2)"
)

## Regression type
l.type <- list(
  "iv"
)


## Perform regressions --------------------------------------------------------

## Build regression dataframe
df.reg <- tibble(l.type, l.rhs, l.cond) %>%
  rowid_to_column(var="model_id") %>%
  nest(data=everything()) %>%
  mutate(N = 3) %>%
  uncount(N) %>%
  bind_cols(tibble(l.lhs), .) %>%
  unnest(cols=c(l.lhs, data)) %>%
  rename_with(.fn=~str_replace(., "l\\.", "")) %>%
  
  ## Include market controls
  mutate(ctrl = ifelse(lhs!="SO2", ctrl_mkt, ""),
         cond = as.character(cond),
         sample = ifelse(lhs=="survive", "& max_boi_nameplate>0.075 & year<=2017", ""),
         fml = paste(lhs, rhs, ctrl, cond, sample, sep=" ")) %>%
  
  ## Exclude market controls
  # mutate(cond = as.character(cond),
  #        fml = paste(lhs, rhs, cond, sep=" ")) %>%
  
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
  ~df.reg %>%
    filter(lhs==.x) %>%
    filter(str_detect(var, "^1.grand_NSR_const.*#[0-9]{4}")) %>%
    mutate(year = str_extract(var, "[0-9]{4}") %>% as.numeric()) %>%
    mutate_at(vars(coef, starts_with("ci_")), ~ifelse(lhs=="DURATION", ./10^3, .)) %>%
    ggplot() +
      geom_hline(yintercept=0, size=0.3, color="#1A1A1A") +
      geom_ribbon(aes(x=year, ymin=ci_lower, ymax=ci_upper), alpha=0.2, fill="#2dCFAC") +
      geom_line(aes(x=year, y=coef), size=0.5, color="#20A387") +
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
            panel.grid.minor.y = element_line(color="grey85", size=0.3),
            plot.caption = element_text(hjust=0))
  ) %>%
  set_names(nm=unlist(l.name))

## Save plots
walk(l.name, 
     ~ggsave(path(l.path$out, date, glue("fig.coefyr_{.x}.pdf")),
             plot=p.coef_yr[[.x]],
             width=8, height=3.5, units="in"))


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl)_"))
rm(list=ls(pattern="^ctrl_"), fes)
rm(l.lhs, l.rhs, l.cond, l.type, l.name)


### END CODE ###


## ------------------------------------------------------------------------------------------------
## Integrated plotting function over lhs and gf vars
# stata_reg <- function(depvar, gf, return="plot") {
# 
#   if (!exists("l.path")) {
#     stop("l.path does not exist.")
#   }
#   stopifnot(
#     length(depvar)==1,
#     is.character(depvar),
#     length(gf)==1,
#     is.character(gf),
#     length(return)==1,
#     return %in% c("df","plot")
#   )
# 
#   ## Create Stata call
#   stata_do <- glue('
#     use "{l.path$data}/regressions_ready_data.dta", clear
#     drop if year<1985
#     drop if year==2006
#     drop if year==2018
#   
#     #delim ;
#     eststo: reg {depvar} so2_nonattain 1.{gf}#1.so2_nonattain applic_reg_const
#       i.ARP_subject c.sulfur_content_tot c.ARPprice_sp#c.sulfur_content_tot
#       age c.max_boi_nameplate {gf}#c.max_boi_nameplate d_growth i.ut_type
#       i.year 1.{gf}#i.year i.states, vce(robust)
#     ;
#     #delim cr
#     regsave, tstat pval ci
#   ')
# 
#   ## Perform regression
#   df <- stata(stata_do, data.out=TRUE)
#   if (return=="df") return(df)
#   
#   ## Build plot
#   if (return=="plot") {
#     
#     if (depvar=="survive") {
#       outcome <- "survival"
#       yaxis <- "Survival probability"
#     } else if (depvar=="hr_load") {
#       outcome <- "hours under load"
#       yaxis <- "Hours"
#     }
#     if (gf=="grand_NSR") {cap <- "Grandfathering varies with time."}
#     else if (gf=="grand_NSR_const") {cap <- "Grandfathering is constant."}
#     
#     df %>%
#       filter(str_detect(var, "^1.grand_NSR.*#[0-9]{4}")) %>%
#       mutate(year = str_extract(var, "[0-9]{4}") %>% as.numeric()) %>%
#       ggplot() +
#         geom_hline(yintercept=0, size=0.3, color="#1A1A1A") +
#         geom_ribbon(aes(x=year, ymin=ci_lower, ymax=ci_upper), alpha=0.5, fill="#6666FF") +
#         geom_line(aes(x=year, y=coef), size=0.3, color="#0000FF") +
#         geom_point(aes(x=year, y=coef), size=0.9, color="#0000FF") +
#         labs(title=glue("Grandfathering yearly coefficients for {outcome}"),
#              y=yaxis,
#              x="Year",
#              fill="",
#              caption=glue("{cap}\nShaded area represents 95% confidence interval.")) +
#         theme_classic() +
#         theme(axis.line.y = element_blank(),
#               axis.ticks.y = element_blank(),
#               panel.grid.major.y = element_line(color="grey85", size=0.3),
#               panel.grid.minor.y = element_line(color="grey85", size=0.3),
#               plot.caption = element_text(hjust=0))
#   }
# }
# 
# l.plot <- map2(list("survive","survive","hr_load","hr_load"),
#                list("grand_NSR","grand_NSR_const","grand_NSR","grand_NSR_const"),
#                ~stata_reg(depvar=.x, gf=.y))
# pwalk(list(list("sur","sur","ops","ops"),
#            list("grand_NSR","grand_NSR_const","grand_NSR","grand_NSR_const"),
#            l.plot),
#       ~ggsave(path(l.path$out, "20210531", glue("fig.{..1}_{..2}.png")), 
#               plot=..3, width=7, height=5, units="in"))

