## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## GII Data Audits
## lib/dir_class.R
## Jack Gregory
## 20 February 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION -----------------------------------------------------------------------------------
## This script provides helper functions for <def_paths.R>, namely to (1) extract a path's base 
## folder name and (2) add that name as a class type.


# VERSION HISTORY --------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  20Feb2020 Jack Gregory  Initial version


### START CODE ###


# dir_name --------------------------------------------------------------------------------------

dir_name <- function(dir) {
  
  ## Check inputs
  stopifnot(
    fs::is_dir(dir)
  )
  
  ## Extract name (i.e. data type) from dir
  name <- fs::path_split(dir) %>%
    unlist()
  name <- name[length(name)]
  
  return(name)
}


# dir_class --------------------------------------------------------------------------------------

dir_class <- function(dir) {
  
  ## Check inputs
  stopifnot(
    fs::is_dir(dir)
  )
  
  ## Extract data type from dir
  type <- dir_name(dir)
  
  ## Set class of dir using data type
  dir <- structure(dir, class=c(class(dir), type))
  
  return(dir) 
}


### END CODE ###

