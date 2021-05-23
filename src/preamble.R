## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/preamble.R
## Jack Gregory
## 16 April 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This program provides necessary packages for the Grandfathering repository.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  17Jul2017 Jack Gregory  New draft; ...
## ...


### START CODE ###


# (1) CLEAR ENVIRONMENT & CONSOLE -----------------------------------------------------------------
#rm(list=ls())   # Clear environment (i.e. all datasets)
#cat("\014")     # Clear console (i.e. all output)


# (2) INITIALIZE PACKAGES -------------------------------------------------------------------------

## (2a) Data Import & Export
## fs
## File system management
if (!("fs" %in% installed.packages())) install.packages("fs")
library(fs)

## here
## R project home directory
if (!("here" %in% installed.packages())) install.packages("here")
library(here)

## MySQL
## Interfacing with MySQL
install.packages(setdiff(c("DBI","RMySQL"), rownames(installed.packages())))
library(DBI)
library(RMySQL)

## readxl
## Reading/writing xls & xlsx file data
if (!("here" %in% installed.packages())) install.packages("here")
library(readxl)


## (2b) Data Wrangling
## assertr
## Verify data assumptions
if (!("assertr" %in% installed.packages())) install.packages("assertr")
library(assertr)

## tidyverse
## Comprehensive data science package
## Includes tibble, readr, tidyr, dplyr, stringr, forcats, purrr, ggplot2
install.packages(setdiff(c("tidyverse","lubridate","glue"), 
                         rownames(installed.packages())))
library(tidyverse)
library(lubridate)
library(glue)


### END CODE ###

