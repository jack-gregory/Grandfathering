## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Preprocessing -- Master
## Jack Gregory
## 08 December 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This program performs preprocessing for the Grandfathering project.

## It is composed of the following scripts:
##  (1) sulfur.Rmd -- prepares the sulfur instrumental variable
##  (2) regression_data.do -- prepares necessary data for reg and plot scripts


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  08Dec2024 Jack Gregory  Initial version


### START CODE ###


# SETUP -------------------------------------------------------------------------------------------

## Initiate
## ... Packages
pkgs <- c(
  "here",                   # File system
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

## 01_sulfur.Rmd --------------------------------------------------------------
## NB: This script requires substantial resources and time.

## Run with output rendering
# rmarkdown::render(here::here("build/03_preprocessing/sulfur.Rmd"))

## Run without output rendering
knitr::purl(input=here::here("build/03_preprocessing/01_sulfur.Rmd"),
            output=here::here("build/03_preprocessing/sulfur.R"))
source(here::here("build/03_preprocessing/sulfur.R"))


## 02_regression_data.do ------------------------------------------------------

## Unzip files
l.zips <- list(
  here::here("data/eia/EIA electricity demand data.zip"),
  here::here("data/eia/1989_1998_Nonutility_Power_Producer.zip")
)
l.zip_contents <- purrr::map(
  l.zips, 
  \(x) {zip::zip_list(x) |> dplyr::pull(filename)}
)
purrr::walk(l.zips, \(x) zip::unzip(x, exdir=fs::path_dir(x)))

## Import do file
stata_do <- readLines(here::here("build/03_preprocessing/02_regression_data.do"))

## Add paths
stata_do <- stringr::str_replace_all(stata_do, "your_path_here", here::here())

## Run code
Rstata::stata(stata_do)

## Remove unzipped files
purrr::map2_chr(l.zips, l.zip_contents, \(x,y) fs::path(fs::path_dir(x), y)) |>
  fs::file_delete()


### END CODE ###

