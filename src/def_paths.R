## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/def_paths.R
## Jack Gregory
## 16 April 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION -----------------------------------------------------------------------------------
## This program provides directory paths within the Grandfathering repository.


# VERSION HISTORY --------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  16Apr2021 Jack Gregory  Initial version; Based on BWP Project
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  17Jul2017 Jack Gregory  New draft; ...
## ...


### START CODE ###


# (1) AUTOMATED PATHS ----------------------------------------------------------------------------

## Get the repository home directory
path.home <- here::here()

## Run the dir_name.R library
dir_name <- fs::dir_ls(path.home, recurse=2) %>%
  as.list() %>%
  purrr::keep(stringr::str_detect(., "dir_class.R$")) %>%
  as.character()
source(dir_name)

## Create path list
l.path <- fs::dir_ls(path.home, type="directory") %>%
  as.list() %>%
  purrr::set_names(nm=purrr::map_chr(., dir_name))


# (2) MANUAL PATHS -------------------------------------------------------------------------------
## These are manually entered paths, some examples follow.

# ## <.../raw_data/bwp/misc>
# path.bwp <- fs::path("/home/burbank/Burbank_Com/raw_data/bwp/misc")


# (3) CONVERT TO LIST ----------------------------------------------------------------------------

## Create a path.home list
l.path_home <- as.list(path.home) %>%
  purrr::set_names(nm=c("home"))
rm(path.home)

## Combine all paths into a named list
l.path_append <- mget(ls(pattern="^path\\.")) %>%
  purrr::set_names(nm=map_chr(., dir_name))

## Append supplemental paths
l.path <- l.path %>%
  append(l.path_append) %>%
  append(l.path_home)
l.path <- l.path[order(names(l.path))]
rm(list=ls(pattern="^path\\."))
rm(list=ls(pattern="^l\\.path_"))

# ## Set path classes to include final dir segment
# l.path <- purrr::map(l.path, dir_class)

## Remove dir_class functions
rm(list=ls(pattern="^dir_"))


### END CODE ###

