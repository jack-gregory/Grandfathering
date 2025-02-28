## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Boiler -- 00_Master
## Jack Gregory
## 08 December 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This program performs boiler data cleaning for the Grandfathering project.


### START CODE ###


# SETUP -------------------------------------------------------------------------------------------

## Initiate
## ... Packages
pkgs <- c(
  "fs","here","zip",                                # File system
  "readxl",                                         # Data reading
  "dplyr","lubridate","purrr","stringr","tidyr",    # Tidyverse
  "data.table","foreign","Hmisc","reshape2","zoo",  # Other data wrangling
  "sf"                                              # Spatial data
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

## ... Functions
source(here::here("src/boilers.R"))

## ... Definitions
coal <- c("COL", "BIT", "LIG", "SUB", "ANT", "RC", "WC")
datasets <- c("boilers","plants","fgds","stackflues","generators")
scrubbers <- c("MA", "PA", "SP", "TR", "VE", "CD", "SD")
xwalk <- c("util","gen","plant","boiler_generator","boiler_fgd_id",
           "boiler_ctrl_info","boiler_sf_id","boiler_info","emission_stand",
           "fgd","sf")

months <- c("january", "february", "march", "april", "may", "june", "july", "august",
            "september", "october", "november", "december")
shortmonths <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
monthnum <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

date <- format(Sys.Date(), "%Y%m%d")
fs::dir_create(here::here("out", date))

## ... Crosswalks
l.fs <- purrr::map(
  seq(1,11),
  \(x) read_excel(here::here("data/xwalk/eia860_file_structure.xlsx"), sheet=x)
) %>%
  purrr::set_names(nm=xwalk)
l.vn <- purrr::map(
  seq(1,11),
  \(x) read_excel(here::here("data/xwalk/varname_xwalks.xlsx"), sheet=x)
) %>%
  purrr::set_names(nm=xwalk)


# CODE --------------------------------------------------------------------------------------------

## Run scripts in order
fs::dir_ls(here::here("build/02_boilers")) |>
  purrr::discard(\(x) stringr::str_detect(x, "master")) |>
  purrr::walk(source)


### END CODE ###

