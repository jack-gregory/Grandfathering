## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Master
## Jack Gregory
## 25 June 2025
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script performs analysis for the Grandfathering project.

## It is composed of the following scripts:
##  (1) master -- provides setup and runs all subordinate scripts
##  (2) data -- prepares necessary data for reg and plot scripts
##  (3) regs -- runs all regs and outputs coefficient tables
##  (4) plots -- builds all plots


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  30May2021 Jack Gregory  Initial version
## 1.1  26Aug2021 Jack Gregory  New draft; Update for the draft dissertation chapter
## 2.0  26Sep2021 Jack Gregory  New version; Conversion to multi-script
## 3.0  25Jun2024 Jack Gregory  New version; R&R update


### START CODE ###


# (1) SETUP --------------------------------------------------------------------------------------

## (1a) Initiate packages
pkgs <- c(
  "here"                              # File system
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

source(here("src/preamble.R"))

## (1b) Initiate RStata -- Glue between R & Stata
if(!("RStata" %in% installed.packages())) install.packages("RStata")
library(RStata)
if (Sys.info()[["user"]]=="user") {
  options("RStata.StataPath"="...")
  options("RStata.StataVersion"=16.1)
} else {
  stop("User not recognized.")
}


# (2) IMPORT DATA ---------------------------------------------------------------------------------

source(dir_ls(here("build/03_analysis")) %>% keep(str_detect(., "2_data.R")))


# (3) REGRESSIONS --------------------------------------------------------------------------------

source(dir_ls(here("build/03_analysis")) %>% keep(str_detect(., "3_regs_main.R")))
source(dir_ls(here("build/03_analysis")) %>% keep(str_detect(., "4_regs_robust.R")))
source(dir_ls(here("build/03_analysis")) %>% keep(str_detect(., "5_regs_netgen.R")))


# (4) PLOTS ---------------------------------------------------------------------------------------

source(dir_ls(here("build/03_analysis")) %>% keep(str_detect(., "6_plots_coefyr.R")))


### END CODE ###

