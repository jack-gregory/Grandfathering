## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis
## Jack Gregory
## 30 May 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script performs data analysis for the Grandfathering project.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  30May2021 Jack Gregory  Initial version
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

## RStata --  Glue b/w R and Stata
if(!("RStata" %in% installed.packages())) install.packages("RStata")
library(RStata)
options("RStata.StataPath"="\"C:\\Program Files (x86)\\Stata14\\StataSE-64\"")
options("RStata.StataVersion"=14.2)

## ... Paths
source(fs::path(here::here(), "src/def_paths.R"))

## ... Functions
# source(fs::path(l.path$src, "find_newfiles.R"))




# (1) ??? ------------------------------------------------------------------------------------

stata_do <- glue('
  use "{l.path$data}/regressions_ready_data.dta", clear
  drop if year<1985
  drop if year==2006
  drop if year==2018

  #delim ;
  eststo: reg survive so2_nonattain 1.grand_NSR#1.so2_nonattain applic_reg
    i.ARP_subject c.sulfur_content_tot c.ARPprice_sp#c.sulfur_content_tot
    age c.max_boi_nameplate grand_NSR#c.max_boi_nameplate d_growth
    i.year 1.grand_NSR#i.year i.states, vce(robust)
  ;
  #delim cr
  regsave, tstat pval ci
')

# stata_do <- glue('
#   use "{l.path$data}/regressions_ready_data.dta", clear
#   drop if year<1985
#   drop if year==2006
#   drop if year==2018
# 
#   #delim ;
#   eststo: reg survive so2_nonattain 1.grand_NSR_const#1.so2_nonattain applic_reg_const
#     i.ARP_subject c.sulfur_content_tot c.ARPprice_sp#c.sulfur_content_tot
#     age c.max_boi_nameplate grand_NSR_const#c.max_boi_nameplate d_growth
#     i.year 1.grand_NSR_const#i.year i.states, vce(robust)
#   ;
#   #delim cr
#   regsave, tstat pval ci
# ')

df <- stata(stata_do, data.out=TRUE)


## Build plot
p <- df %>%
  filter(str_detect(var, "^1.grand_NSR.*#[0-9]{4}")) %>%
  mutate(year = str_extract(var, "[0-9]{4}") %>% as.numeric()) %>%
  ggplot() +
    geom_hline(yintercept=0, size=0.3, color="#1A1A1A") +
    geom_ribbon(aes(x=year, ymin=ci_lower, ymax=ci_upper), alpha=0.5, fill="#6666FF") +
    geom_line(aes(x=year, y=coef), size=0.3, color="#0000FF") +
    geom_point(aes(x=year, y=coef), size=0.9, color="#0000FF") +
    labs(title="Grandfathering yearly coefficients",
         y="Survival probability",
         x="Year",
         fill="",
         caption="Shaded area represents the 95% confidence interval.") +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_line(color="grey85", size=0.3),
          panel.grid.minor.y = element_line(color="grey85", size=0.3),
          plot.caption = element_text(hjust=0))


### END CODE ###



stata_reg <- function(depvar, gf, return="plot") {

  if (!exists("l.path")) {
    stop("l.path does not exist.")
  }
  stopifnot(
    length(depvar)==1,
    is.character(depvar),
    length(gf)==1,
    is.character(gf),
    length(return)==1,
    return %in% c("df","plot")
  )

  ## Create Stata call
  stata_do <- glue('
    use "{l.path$data}/regressions_ready_data.dta", clear
    drop if year<1985
    drop if year==2006
    drop if year==2018
  
    #delim ;
    eststo: reg {depvar} so2_nonattain 1.{gf}#1.so2_nonattain applic_reg_const
      i.ARP_subject c.sulfur_content_tot c.ARPprice_sp#c.sulfur_content_tot
      age c.max_boi_nameplate {gf}#c.max_boi_nameplate d_growth i.ut_type
      i.year 1.{gf}#i.year i.states, vce(robust)
    ;
    #delim cr
    regsave, tstat pval ci
  ')

  ## Perform regression
  df <- stata(stata_do, data.out=TRUE)
  if (return=="df") return(df)
  
  ## Build plot
  if (return=="plot") {
    
    if (depvar=="survive") {
      outcome <- "survival"
      yaxis <- "Survival probability"
    } else if (depvar=="hr_load") {
      outcome <- "hours under load"
      yaxis <- "Hours"
    }
    if (gf=="grand_NSR") {cap <- "Grandfathering varies with time."}
    else if (gf=="grand_NSR_const") {cap <- "Grandfathering is constant."}
    
    df %>%
      filter(str_detect(var, "^1.grand_NSR.*#[0-9]{4}")) %>%
      mutate(year = str_extract(var, "[0-9]{4}") %>% as.numeric()) %>%
      ggplot() +
        geom_hline(yintercept=0, size=0.3, color="#1A1A1A") +
        geom_ribbon(aes(x=year, ymin=ci_lower, ymax=ci_upper), alpha=0.5, fill="#6666FF") +
        geom_line(aes(x=year, y=coef), size=0.3, color="#0000FF") +
        geom_point(aes(x=year, y=coef), size=0.9, color="#0000FF") +
        labs(title=glue("Grandfathering yearly coefficients for {outcome}"),
             y=yaxis,
             x="Year",
             fill="",
             caption=glue("{cap}\nShaded area represents 95% confidence interval.")) +
        theme_classic() +
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_line(color="grey85", size=0.3),
              panel.grid.minor.y = element_line(color="grey85", size=0.3),
              plot.caption = element_text(hjust=0))
  }
}

l.plot <- map2(list("survive","survive","hr_load","hr_load"),
               list("grand_NSR","grand_NSR_const","grand_NSR","grand_NSR_const"),
               ~stata_reg(depvar=.x, gf=.y))
pwalk(list(list("sur","sur","ops","ops"),
           list("grand_NSR","grand_NSR_const","grand_NSR","grand_NSR_const"),
           l.plot),
      ~ggsave(path(l.path$out, "20210531", glue("fig.{..1}_{..2}.png")), 
              plot=..3, width=7, height=5, units="in"))
