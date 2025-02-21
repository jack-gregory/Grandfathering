## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 02_eia860_2007-2018
## Bridget Pals
## 27 July 2020
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script combines data from form EIA-860 between 2007 and 2018.


### START CODE ###


# PREAMBLE ----------------------------------------------------------------------------------------

## Initiate
## ... Packages
pkgs <- c(
  "fs","here","zip",                                # File system
  "readxl",                                         # Data reading
  "dplyr","purrr","data.table","foreign","zoo"      # Data wrangling
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

## ... Functions
source(here::here("src/boilers.R"))

## ... Definitions
scrubbers <- c("MA", "PA", "SP", "TR", "VE", "CD", "SD")
xwalk <- c("util","gen","plant","boiler_generator","boiler_fgd_id",
           "boiler_ctrl_info","boiler_sf_id","boiler_info","emission_stand",
           "fgd","sf")

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


# EIA-860 -----------------------------------------------------------------------------------------

# initialize empty dataframes
utils <- data.frame()
plants <- data.frame()
generators <- data.frame()
boilers <- data.frame()
fgds <- data.frame()
sfs <- data.frame()

for (i in c(2007:2018)) {

  ## Unzip --------------------------------------------------------------------
  
  zip_file <- here::here("data/eia/f860", paste0("f860_", i, ".zip"))
  l.zip <- zip::zip_list(zip_file) %>%
    dplyr::pull(filename) %>%
    as.list()
  zip::unzip(zip_file, exdir=fs::path_dir(zip_file))
  
  ## Utilities ----------------------------------------------------------------

  filename <- l.fs$util[l.fs$util$year == i, 2]
  tab <- as.numeric(l.fs$util[l.fs$util$year == i, 3])
  skip <- as.numeric(l.fs$util[l.fs$util$year == i, 4])

  util_sub <- read_excel(here::here("data/eia/f860", filename),
                         col_types = "text", skip = skip, sheet = tab) %>% unique()

  orig_name <- as.data.frame(names(util_sub), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"

  newname <- left_join(orig_name, l.vn$util)

  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))

  #print(newname)

  names(util_sub) <- newname$newname

  ## Plants -------------------------------------------------------------------
  ## Use plants as the base
  
  print(paste0("----------------  ", i, "  ----------------"))
  
  filename <- l.fs$plant[l.fs$plant$year == i, 2]
  tab <- as.numeric(l.fs$plant[l.fs$plant$year == i, 3])
  skip <- as.numeric(l.fs$plant[l.fs$plant$year == i, 4])
  
  plant_sub <- read_excel(here::here("data/eia/f860", filename), 
                         skip = skip, sheet = tab, col_types = "text") %>% unique()
  
  orig_name <- as.data.frame(names(plant_sub), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"
  
  newname <- left_join(orig_name, l.vn$plant)
  
  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
  
  #print(newname)
  
  names(plant_sub) <- newname$newname
  
  print(plant_sub %>% unique_id(utility_code, plant_code))
  
  ## Generators ---------------------------------------------------------------
  ## Make a generator subfile

  filename <- l.fs$gen[l.fs$gen$year == i, 2]
  tab <- as.numeric(l.fs$gen[l.fs$gen$year == i, 3])
  skip <- as.numeric(l.fs$gen[l.fs$gen$year == i, 4])
  
  gen_sub <- read_excel(here::here("data/eia/f860", filename), 
                          skip = skip, sheet = tab, col_types = "text") %>% unique()
  
  if (i >= 2009) {
    gen_ret <- read_excel(here::here("data/eia/f860", filename), 
                          skip = skip, sheet = 3, col_types = "text") %>% unique()
    
    gen_sub <- bind_rows(gen_ret, gen_sub)
  }
  
  orig_name <- as.data.frame(names(gen_sub), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"
  
  newname <- left_join(orig_name, l.vn$gen)
  
  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
  
  #print(newname)
  
  names(gen_sub) <- newname$newname
  
  print(gen_sub %>% unique_id(utility_code, plant_code, generator_id))

  ## Boiler xwalk -------------------------------------------------------------
  ## we follow the same process to read in the xwalk to boiler level
  
  filename <- l.fs$boiler_generator[l.fs$boiler_generator$year == i, 2]
  tab <- as.numeric(l.fs$boiler_generator[l.fs$boiler_generator$year == i, 3])
  skip <- as.numeric(l.fs$boiler_generator[l.fs$boiler_generator$year == i, 4])
  
  boil_gen_xwalk <- read_excel(here::here("data/eia/f860", filename), 
                        skip = skip, sheet = tab, col_types = "text") %>% unique()
  
  orig_name <- as.data.frame(names(boil_gen_xwalk), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"
  
  newname <- left_join(orig_name, l.vn$boiler_generator)
  
  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))

  #print(newname)
  
  names(boil_gen_xwalk) <- newname$newname
  
  print(boil_gen_xwalk %>% unique_id(utility_code, plant_code, boiler_id, generator_id))

  ## For consistency, we'll drop these two columns if they're in 
  ## boil_gen_xwalk as well as gen_sub (just in case there's a data
  ## entry error that could throw off our match)
  if ("utility_name" %in% names(boil_gen_xwalk)) {
    boil_gen_xwalk <- boil_gen_xwalk %>% select(-utility_name)
  }
  if ("plant_name" %in% names(boil_gen_xwalk)) {
    boil_gen_xwalk <- boil_gen_xwalk %>% select(-plant_name)
  }
  
  gen_sub <- left_join(gen_sub, boil_gen_xwalk)
  
  print(gen_sub %>% unique_id(utility_code, plant_code, boiler_id, generator_id))
  
  ## Stackflue ----------------------------------------------------------------
  ## Make a stackflue subfile

  filename <- l.fs$sf[l.fs$sf$year == i, 2]
  tab <- as.numeric(l.fs$sf[l.fs$sf$year == i, 3])
  skip <- as.numeric(l.fs$sf[l.fs$sf$year == i, 4])
  
  sf_sub <- read_excel(here::here("data/eia/f860", filename), 
                        skip = skip, sheet = tab, col_types = "text") %>% unique()

  ## nb for 2012-2017, there are two vars named "Exit Velocity 100% 
  ## (Feet per Second)." In each case, the second instance is meant to
  ## read 50%.
  names(sf_sub)
  
  rep <- grep("Exit Velocity 100%", names(sf_sub))
  
  print(rep)
  
  if (length(rep) > 1) {
    names(sf_sub)[rep[1]] <- "Exit Velocity 100% (Feet per Second)"
    names(sf_sub)[rep[2]] <- "Exit Velocity 50% (Feet per Second)" 
  }
  
  orig_name <- as.data.frame(names(sf_sub), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"
  
  newname <- left_join(orig_name, l.vn$sf)
  
  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
  
  #print(newname)
  
  names(sf_sub) <- newname$newname
  
  ## after 2013, there are between 2 and 8 duplicates in each set.
  ## because this is such an infrequent occurrence, I just keep the 
  ## first record
  if ("stackflue_id" %in% names(sf_sub)) {
    #print(sf_sub %>% unique_id(utility_code, plant_code, stackflue_id))
    
    sf_sub <- sf_sub %>% 
      group_by(utility_code, plant_code, stackflue_id) %>%
      mutate(n = 1:n()) %>% ungroup() %>%
      filter(n == 1) %>% select(-n)
    
  } else {
    #print(sf_sub %>% unique_id(utility_code, plant_code, stack_id, flue_id))
    
    sf_sub <- sf_sub %>% 
      group_by(utility_code, plant_code, stack_id, flue_id) %>%
      mutate(n = 1:n()) %>% ungroup() %>%
      filter(n == 1) %>% select(-n)
    
  }
  
  ## Stackflue xwalk ----------------------------------------------------------
  ## Adopt the same process to read in the stackflue xwalk to boiler level
  
  filename <- l.fs$boiler_sf_id[l.fs$boiler_sf_id$year == i, 2]
  tab <- as.numeric(l.fs$boiler_sf_id[l.fs$boiler_sf_id$year == i, 3])
  skip <- as.numeric(l.fs$boiler_sf_id[l.fs$boiler_sf_id$year == i, 4])
  
  boil_sf_xwalk <- read_excel(here::here("data/eia/f860", filename), 
                               skip = skip, sheet = tab, col_types = "text") %>% unique()
  
  orig_name <- as.data.frame(names(boil_sf_xwalk), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"
  
  newname <- left_join(orig_name, l.vn$boiler_sf_id)
  
  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
  
  #print(newname)
  
  names(boil_sf_xwalk) <- newname$newname
  
  if ("stackflue_id" %in% names(boil_sf_xwalk)) {
    print(boil_sf_xwalk %>% unique_id(utility_code, plant_code, boiler_id, stackflue_id))
  } else {
    print(boil_sf_xwalk %>% unique_id(utility_code, plant_code, boiler_id, stack_id, flue_id))
  }
  
  ## for consistency, we'll drop these two columns if they're in 
  ## boil_sf_xwalk as well as sf_sub (just in case there's a data
  ## entry error that could throw off our match)
  if ("utility_name" %in% names(boil_sf_xwalk)) {
    boil_sf_xwalk <- boil_sf_xwalk %>% select(-utility_name)
  }
  if ("plant_name" %in% names(boil_sf_xwalk)) {
    boil_sf_xwalk <- boil_sf_xwalk %>% select(-plant_name)
  }
  
  sf_sub <- left_join(sf_sub, boil_sf_xwalk)
  
  if ("stackflue_id" %in% names(sf_sub)) {
    print(sf_sub %>% unique_id(utility_code, plant_code, boiler_id, stackflue_id))
  } else {
    print(sf_sub %>% unique_id(utility_code, plant_code, boiler_id, stack_id, flue_id))
  }

  ## Boilers ------------------------------------------------------------------
  ## Read boiler-level information

  filename <- l.fs$emission_stand[l.fs$emission_stand$year == i, 2]
  tab <- as.numeric(l.fs$emission_stand[l.fs$emission_stand$year == i, 3])
  skip <- as.numeric(l.fs$emission_stand[l.fs$emission_stand$year == i, 4])
  
  emission_stand_sub <- read_excel(here::here("data/eia/f860", filename), 
                                   skip = skip, sheet = tab, col_types = "text") %>% unique()
  
  orig_name <- as.data.frame(names(emission_stand_sub), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"
  
  newname <- left_join(orig_name, l.vn$emission_stand)
  
  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
  
  names(emission_stand_sub) <- newname$newname
  
  print(emission_stand_sub %>% unique_id(utility_code, plant_code, boiler_id))
  
  if (i %in% c(2009:2018)) {
    
    print(i)
    
    filename <- l.fs$boiler_info[l.fs$boiler_info$year == i, 2]
    tab <- as.numeric(l.fs$boiler_info[l.fs$boiler_info$year == i, 3])
    skip <- as.numeric(l.fs$boiler_info[l.fs$boiler_info$year == i, 4])
    
    boiler_info_sub <- read_excel(here::here("data/eia/f860", filename), 
                                  skip = skip, sheet = tab, col_types = "text") %>% unique()
    
    if (i == 2011) {
      print(table(boiler_info_sub$STANDARD_PARTICULATE))
      print(table(boiler_info_sub$STANDARD_SULFUR))
      print(table(boiler_info_sub$STANDARD_NITROGEN))
      
      boiler_info_sub <- boiler_info_sub %>%
        select(-STANDARD_PARTICULATE, -STANDARD_SULFUR, -STANDARD_NITROGEN)
    } else if (i == 2012) {
      print(table(boiler_info_sub$'Standard Particulate'))
      print(table(boiler_info_sub$'Standard Sulfur'))
      print(table(boiler_info_sub$'Standard Nitrogen'))
      
      boiler_info_sub <- boiler_info_sub %>%
        select(-'Standard Particulate', -'Standard Sulfur', -'Standard Nitrogen')
    }
    
    orig_name <- as.data.frame(names(boiler_info_sub), stringsAsFactors = FALSE)
    names(orig_name) <- "orig_name"
    
    newname <- left_join(orig_name, l.vn$boiler_info)
    
    print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
    
    names(boiler_info_sub) <- newname$newname
    
    print(boiler_info_sub %>% unique_id(utility_code, plant_code, boiler_id))
    
    emission_stand_sub <- left_join(emission_stand_sub, boiler_info_sub,
                                    by = c("utility_code", "plant_code", "boiler_id"))
    
    drop <- grep("\\.y", names(emission_stand_sub))
    
    if (length(drop) > 0) {
      emission_stand_sub <- emission_stand_sub[-drop]
    }
    
    names(emission_stand_sub) <- gsub("\\.x", "", names(emission_stand_sub))
  }
  
  print(emission_stand_sub %>% unique_id(utility_code, plant_code, boiler_id))
  
  ## FGD ----------------------------------------------------------------------
  ## Read FGD-specific information
  
  print(i)
  ## First, we generate our crosswalk, boilers to FGD
  
  filename <- l.fs$boiler_fgd_id[l.fs$boiler_fgd_id$year == i, 2]
  tab <- as.numeric(l.fs$boiler_fgd_id[l.fs$boiler_fgd_id$year == i, 3])
  skip <- as.numeric(l.fs$boiler_fgd_id[l.fs$boiler_fgd_id$year == i, 4])
  
  boiler_fgd_xwalk <- read_excel(here::here("data/eia/f860", filename), 
                                 skip = skip, sheet = tab, col_types = "text") %>% unique()
  
  orig_name <- as.data.frame(names(boiler_fgd_xwalk), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"
  
  newname <- left_join(orig_name, l.vn$boiler_fgd_id)
  
  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
  
  names(boiler_fgd_xwalk) <- newname$newname
  
  print(boiler_fgd_xwalk %>% unique_id(utility_code, plant_code, boiler_id, fgd_id))
  
  ## now, we read in FGD information
  filename <- l.fs$fgd[l.fs$fgd$year == i, 2]
  tab <- as.numeric(l.fs$fgd[l.fs$fgd$year == i, 3])
  skip <- as.numeric(l.fs$fgd[l.fs$fgd$year == i, 4])
  
  fgd_sub <- read_excel(here::here("data/eia/f860", filename), 
                        skip = skip, sheet = tab, col_types = "text") %>% unique()
  
  orig_name <- as.data.frame(names(fgd_sub), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"
  
  newname <- left_join(orig_name, l.vn$fgd)
  
  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
  
  names(fgd_sub) <- newname$newname
  
  ## there is one duplicate record in 2011. We just keep the first one
  print(fgd_sub %>% unique_id(utility_code, plant_code, fgd_id))
  
  fgd_sub <- fgd_sub %>% group_by(utility_code, plant_code, fgd_id) %>%
    mutate(n = 1:n()) %>% ungroup() %>% filter(n == 1) %>% select(-n)
  
  ## now, we combine the crosswalk and FGD dataset so that we can crosswalk
  ## to the boiler-level information
  fgd_sub <- left_join(boiler_fgd_xwalk, fgd_sub, 
                       by = c("utility_code", "plant_code", "fgd_id"))
  
  drop <- grep("\\.y", names(fgd_sub))
  
  if (length(drop) > 0) {
    fgd_sub <- fgd_sub[-drop]
  }  
  
  names(fgd_sub) <- gsub("\\.x", "", names(fgd_sub))
  
  print(fgd_sub %>% unique_id(utility_code, plant_code, boiler_id, fgd_id))

  # from 2013-2018, certain variables that had previously been in the fgd dataset
  # were kept in a different dataset. We read in that dataset (which deals with all
  # control technologies), limit it to FGD only, and rename vars for consistency.

  if (i %in% 2013:2018) {
    
    filename <- l.fs$boiler_ctrl_info[l.fs$boiler_ctrl_info$year == i, 2]
    tab <- as.numeric(l.fs$boiler_ctrl_info[l.fs$boiler_ctrl_info$year == i, 3])
    skip <- as.numeric(l.fs$boiler_ctrl_info[l.fs$boiler_ctrl_info$year == i, 4])
    
    boiler_ctrl_info_sub <- read_excel(here::here("data/eia/f860", filename), 
                          skip = skip, sheet = tab, col_types = "text") %>% unique()
    
    orig_name <- as.data.frame(names(boiler_ctrl_info_sub), stringsAsFactors = FALSE)
    names(orig_name) <- "orig_name"
    
    newname <- left_join(orig_name, l.vn$boiler_ctrl_info)
    
    print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
    
    names(boiler_ctrl_info_sub) <- newname$newname
    
    boiler_ctrl_info_sub <- boiler_ctrl_info_sub %>%
      filter(!is.na(so2_ctrl_id)) %>% 
      rename(fgd_id = so2_ctrl_id,
             fgd_type1 = ctrl_equip_type,
             fgd_status = ctrl_equip_status,
             fgd_inservice_m = ctrl_inservice_m,
             fgd_inservice_y = ctrl_inservice_y,
             cost_total = ctrl_total_cost) %>%
      select(-pm_cntrl_id, -nox_ctrl_id, -hg_ctrl_id, -acid_gas_ctrl) %>%
      filter(fgd_type1 %in% scrubbers) %>%
      unique()
    
    if ("ctrl_retirement_y" %in% names(boiler_ctrl_info_sub)) {
      boiler_ctrl_info_sub <- rename(boiler_ctrl_info_sub, fgd_retirement_y = ctrl_retirement_y)
    }
    
    if ("ctrl_retirement_m" %in% names(boiler_ctrl_info_sub)) {
      boiler_ctrl_info_sub <- rename(boiler_ctrl_info_sub, fgd_retirement_m = ctrl_retirement_m)
    }
    
    ## there are several records with multiple scrubbers with the same FGD ID. 
    ## per our discussion at check-in, we will SUM the costs, but otherwise
    ## keep the data on the earlier FGD
    print(boiler_ctrl_info_sub %>% unique_id(utility_code, plant_code, fgd_id))
    
    boiler_ctrl_info_sub <- boiler_ctrl_info_sub %>%
      group_by(utility_code, plant_code, fgd_id) %>%
      mutate(cost_total = sum(as.numeric(cost_total))) %>%
      arrange(desc(fgd_inservice_y)) %>%
      mutate(n = 1:n()) %>%
      ungroup() %>% filter(n == 1) %>% select(-n)
    
    print(boiler_ctrl_info_sub %>% unique_id(utility_code, plant_code, fgd_id))
    
    ## now, we add this extra dataset into all the rest of our data
    
    fgd_sub <- left_join(fgd_sub, boiler_ctrl_info_sub, 
                         by = c("utility_code", "plant_code", "fgd_id"))
    
    drop <- grep("\\.y", names(fgd_sub))
    
    if (length(drop) > 0) {
      fgd_sub <- fgd_sub[-drop]
    }  
    
    names(fgd_sub) <- gsub("\\.x", "", names(fgd_sub))
  }
    
  ## Append -------------------------------------------------------------------
  ## Append datasets year over year

  util_sub <- mutate(util_sub, year = i)
  plant_sub <- mutate(plant_sub, year = i)
  gen_sub <- mutate(gen_sub, year = i)
  emission_stand_sub <- mutate(emission_stand_sub, year = i)
  fgd_sub <- mutate(fgd_sub, year = i)
  sf_sub <- mutate(sf_sub, year = i)
  
  utils <- bind_rows(utils, util_sub)
  plants <- bind_rows(plants, plant_sub)
  generators <- bind_rows(generators, gen_sub)
  boilers <- bind_rows(boilers, emission_stand_sub)
  fgds <- bind_rows(fgds, fgd_sub)
  sfs <- bind_rows(sfs, sf_sub)
  
  ## Remove -------------------------------------------------------------------
  
  l.zip %>%
    purrr::map_chr(\(x) fs::path(fs::path_dir(zip_file), x)) %>%
    fs::file_delete()
}

## Export ---------------------------------------------------------------------
write.csv(utils, here::here("data/utils_2007_2018.csv"))
write.csv(plants, here::here("data/plants_2007_2018.csv"))
write.csv(generators, here::here("data/generators_2007_2018.csv"))
write.csv(boilers, here::here("data/boilers_2007_2018.csv"))
write.csv(fgds, here::here("data/fgds_2007_2018.csv"))
write.csv(sfs, here::here("data/stackflues_2007_2018.csv"))


### END CODE ###

