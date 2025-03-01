## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Master
## Jack Gregory
## 08 December 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script performs analysis for the Grandfathering project.

## It is composed of the following script types:
##  master  -- provides setup and runs all subordinate scripts
##  data    -- prepares necessary data for regression scripts
##  regs    -- runs respective regressions and outputs coefficient tables
##  plot    -- builds respective plots
##  tbl     -- builds respective tables


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  30May2021 Jack Gregory  Initial version
## 1.1  26Aug2021 Jack Gregory  New draft; Update for the draft dissertation chapter
## 2.0  26Sep2021 Jack Gregory  New version; Conversion to multi-script
## 3.0  25Jun2024 Jack Gregory  New version; R&R update
## 4.0  08Dec2024 Jack Gregory  New version; Final update


### START CODE ###


# SETUP -------------------------------------------------------------------------------------------

## Initiate
## ... Packages
pkgs <- c(
  "fs","here",              # File system
  "knitr","rmarkdown"       # Reproducible reporting
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

source(here::here("src/preamble.R"))

## ... Stata
if(!("RStata" %in% installed.packages())) install.packages("RStata")
library(RStata)
if (Sys.info()[["user"]]=="ajrg2") {
  options("RStata.StataPath"="\"C:\\Program Files (x86)\\Stata14\\StataSE-64\"")
  options("RStata.StataVersion"=14.2)
} else if (Sys.info()[["user"]]=="Bialek-S") {
  options("RStata.StataPath"="\"C:\\Program Files\\Stata16\\StataSE-64\"")
  options("RStata.StataVersion"=16.1)
} else {
  stop("User not recognized.")
}


# CODE --------------------------------------------------------------------------------------------

## Run scripts in order
fs::dir_ls(here::here("build/05_analysis")) |>
  purrr::discard(\(x) stringr::str_detect(x, "master")) |>
  purrr::keep(\(x) fs::path_ext(x) %in% c("R","Rmd","do")) |>
  purrr::walk(~{
    if (fs::path_ext(.x)=="R") {
      
      ## Run code
      source(.x)
      
    } else if (fs::path_ext(.x)=="Rmd") {
      
      ## Run with output rendering
      # rmarkdown::render(.x)
      
      ## Run without output rendering
      out_file <- fs::path_ext_set(.x, "R")
      knitr::purl(input=.x, output=out_file)
      source(out_file)
      fs::file_delete(out_file)
      
    } else if (fs::path_ext(.x)=="do") {
      
      ## Import do file
      stata_do <- readLines(.x)
      
      ## Add paths
      stata_do <- stringr::str_replace_all(stata_do, "your_path_here", here::here())
      
      ## Run code
      RStata::stata(stata_do)
      
    }
  })


### END CODE ###

