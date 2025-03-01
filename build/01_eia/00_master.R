## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## EIA -- Master
## Jack Gregory
## 08 December 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script performs EIA data collection and cleaning for the Grandfathering project.

## NB - The files are independent from one another and do not need to be run in a particular order.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  08Dec2024 Jack Gregory  Initial version


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


# CODE --------------------------------------------------------------------------------------------

## Run scripts in order
fs::dir_ls(here::here("build/01_eia")) |>
  purrr::discard(\(x) stringr::str_detect(x, "master")) |>
  purrr::keep(\(x) fs::path_ext(x)=="Rmd") |>
  purrr::walk(~{

      ## Run with output rendering
      # rmarkdown::render(.x)
      
      ## Run without output rendering
      out_file <- fs::path_ext_set(.x, "R")
      knitr::purl(input=.x, output=out_file)
      source(out_file)
      fs::file_delete(out_file)
      
  })


### END CODE ###

