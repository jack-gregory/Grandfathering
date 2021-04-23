## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## CEMS scrape
## Jack Gregory
## 10 April 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## INTRODUCTION -----------------------------------------------------------------------------------
## This program scrapes Continuous Emissions Montitoring Systems (CEMS) data from the US 
## Environmental Protection Agency (EPA).  

## EPA websites:
##  - Description:
##    <https://www.epa.gov/emc/emc-continuous-emission-monitoring-systems>
##  - Database:
##    <https://gaftp.epa.gov/dmdnload/emissions>


## VERSION HISTORY --------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  10Apr2021 Jack Gregory  Initial version


### START CODE ###


## (1) PREAMBLE -----------------------------------------------------------------------------------

## (1a) ## (1a) Clear Environment & Console
# rm(list=ls())   # Clear environment (i.e. all datasets)
# cat("\014")     # Clear console (i.e. all output)


## (1b) Initiate packages
## File System
## fs
if (!("tidyverse" %in% installed.packages())) install.packages("tidyverse")
library(fs)

## Web Scraping
## See <https://github.com/yusuzech/r-web-scraping-cheat-sheet/blob/master/README.md#rselenium3.2>
install.packages(setdiff(c("httr","hvest"), rownames(installed.packages())))
library(httr)       ## See <https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html>
library(rvest)
# library(RSelenium)  ## See <https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html>
# library(downloader)

## Data Wrangling
## tidyverse
## Comprehensive data science package
## Includes tibble, readr, tidyr, dplyr, stringr, forcats, purrr, ggplot2
if (!("tidyverse" %in% installed.packages())) install.packages("tidyverse")
library(tidyverse)


## (1c) Define lists
## paths
l.path <- list(
  home = fs::path("C:/Users/Jack/Dropbox/Grandfathering"),
  data = fs::path("E:/My Data/EPA/CEMS")
)

## urls
l.url <- list(
  home = "https://gaftp.epa.gov/dmdnload/emissions",
  root = "https://gaftp.epa.gov/dmdnload/emissions/hourly/monthly/"
)

## (1d) Set working directory
# setwd(l.path$home)


## (2) FUNCTIONS ----------------------------------------------------------------------------------

## (2a) Iterate over years
itr_yr <- function(yr) {
  
  stopifnot(
    length(yr)==1,
    is.character(yr),
    exists("l.url"),
    exists("l.path")
  )
  
  cat("\n", yr, " ...\n", sep="")

  ## Scrape list of zips
  l.zip <- paste0(l.url$root, yr) %>%
    httr::RETRY("GET", url=., times = 30) %>%
    read_html() %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    as.list() %>%
    purrr::keep(stringr::str_detect(., "\\.zip$"))
  
  ## Create year directory, if necessary
  if (length(l.zip)>0) {
    fs::dir_create(fs::path(l.path$data, yr))  
  }
  
  cat("Iterate over ", length(l.zip), " zipfiles\n", sep="")
  
  ## Download all data by iterating over years
  purrr::walk2(as.list(rep(yr, length(l.zip))),
               l.zip,
               ~itr_zip(.x, .y))
}


## (2b) Iterate over zipfiles
itr_zip <- function(yr, zip) {
  
  stopifnot(
    length(yr)==1,
    is.character(yr),
    length(zip)==1,
    is.character(zip)
  )
  
  cat(zip, "\n", sep="")
  
  download.file(url=paste0(l.url$root, yr, zip), 
                destfile=fs::path(l.path$data, yr, zip),
                quiet=TRUE)
}


## (3) SCRAPE DATA --------------------------------------------------------------------------------

## (3a) Scrape vector of data years from EPA website
v.yrs_web <- l.url$root %>%
  httr::RETRY("GET", url=., times = 30) %>%
  read_html() %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href") %>%
  as.list() %>%
  purrr::keep(stringr::str_detect(., "^[0-9]{4}/$")) %>%
  unlist()


## (3b) Import vector of data years from external hard drive
## NB - The last year is removed to ensure download of all missing data
v.yrs_ehd <- fs::dir_ls(fs::path(l.path$data)) %>%
  fs::path_file() %>%
  paste("/", sep="") %>%
  sort() %>%
  .[1:(length(.)-1)]


## (3c) Create list of data years
l.yrs <- setdiff(v.yrs_web, v.yrs_ehd) %>%
  as.list()


## (3d) Scrape CEMS data
purrr::walk(l.yrs, ~itr_yr(.x))


### END CODE ###

