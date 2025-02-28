## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/boilers.R
## Bridget Pals
## 27 July 2020
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION -----------------------------------------------------------------------------------
## This script provides helper functions related to the boiler dataset prepared in 
## <build/02_boilers>.

## It has the following dependencies:
##  - dplyr


### START CODE ###


# unique_id ---------------------------------------------------------------------------------------
## Identify whether a group of variables create a unique id.
## Credit to: devtools::install.github("edwinth/thatssorandom")

## x    = Dataframe
## ...  = Column names

unique_id <- function(x, ...) {
  id_set <- x %>% select(...)
  id_set_dist <- id_set %>% distinct
  if (nrow(id_set) == nrow(id_set_dist)) {
    TRUE
  } else {
    non_unique_ids <- id_set %>% 
      filter(id_set %>% duplicated()) %>% 
      distinct()
    suppressMessages(
      inner_join(non_unique_ids, x) %>% arrange(...)
    )
  }
}


# mysum & mymean ----------------------------------------------------------------------------------
## Summation and averaging after converting the input vector to numeric.

## x = Any vector

mysum <- function(x) {
  sum(as.numeric(x), na.rm = TRUE)
}

mymean <- function(x) {
  mean(as.numeric(x), na.rm = TRUE)
}

mymax <- function(x) {
  max(as.numeric(x), na.rm = TRUE)
}


### END CODE ###

