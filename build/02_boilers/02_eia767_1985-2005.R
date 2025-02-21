## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 02_eia767_1985-2005
## Bridget Pals
## 27 July 2020
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script combines data from form EIA-767 on coal plants, in particular location, heat rate, 
## nameplate, and scrubbers.


### START CODE ###


# EIA-767 -----------------------------------------------------------------------------------------

## First, we read the EIA-767 data, which covers 1985-2005 and contains critical information such 
## as whether a facility has a scrubber, year installed, lat/long of facility, etc. We will begin 
## by retaining all data and pare down later (and/or generate further features for our model). We 
## then use EIA-920 (and predecessors) to fill out 1970-1984 and 2006-present.

## we generate empty data.frames() which will fill in on the loops
plants <- data.frame()
fgds <- data.frame()
boilers <- data.frame()
stackflues <- data.frame()
generators <- data.frame()

for (i in 1985:2005){
 
  print("~~~~~~~~~~~~~~~~~~~~~~~~~")
  print(paste0("~~~~      ", i, "      ~~~~"))
  print("~~~~~~~~~~~~~~~~~~~~~~~~~")
  
  ## Unzip --------------------------------------------------------------------
  
  zip_file <- here::here("data/eia/f767", paste0("f767_", i, ".zip"))
  l.zip <- zip::zip_list(zip_file) %>%
    dplyr::pull(filename) %>%
    as.list()
  zip::unzip(zip_file, exdir=fs::path_dir(zip_file))
  
  ## Plant information --------------------------------------------------------
  
  if (i %in% c(1985:2000)) {
    plant_sub <- read_excel(here::here("data/eia/f767/Plant.xls"), col_types = "text") %>% unique()
  } else if (i %in% c(2001:2003, 2005)) {
    plant_sub <- read_excel(here::here("data/eia/f767/F767_Plant.xls"), col_types = "text") %>% unique()
  } else if (i == 2004) {
    plant_sub <- read_excel(here::here("data/eia/f767/2004 F767_Plant.xls"), col_types = "text") %>% unique()
  }

  names(plant_sub) <- tolower(names(plant_sub))

  plant_sub <- mutate(plant_sub, year = i)
  
  ## utility_code and plant_code uniquely identify
  print(unique_id(plant_sub, utility_code, plant_code))

  # in 1992, plant 3238 is duplicated 3 times with minor misentries
  # in utility town/zip. We remove these duplicates
  if (i == 1992) {
    print(nrow(plant_sub))
    
    plant_sub <- plant_sub %>% 
      group_by(utility_code, plant_code) %>%
      mutate(num = 1:n()) %>% ungroup() %>%
      filter(num == 1) %>% select(-num)
    print(nrow(plant_sub))
    print(unique_id(plant_sub, utility_code, plant_code))
  }
  
  ## utility_code and plant_code uniquely identify
  print(unique_id(plant_sub, utility_code, plant_code))
  
  names(plant_sub)
  
  plant_sub <- plant_sub %>% select(year, utility_code, plant_code, everything())

  ## Generator information ----------------------------------------------------
  
  if (i %in% c(1985:2000)) {
    gen_sub <- read_excel(here::here("data/eia/f767/Generator.xls"), col_types = "text") %>% unique()
  } else if (i %in% c(2001:2003)) {
    gen_sub <- read_excel(here::here("data/eia/f767/F767_Generator.xls"), col_types = "text") %>% unique()
  } else if (i == 2004) {
    gen_sub <- read_excel(here::here("data/eia/f767/2004 F767_Generator.xls"), col_types = "text") %>% unique()
  } else if (i == 2005) {
    gen_sub <- read_excel(here::here("data/eia/f767/2005 EIA-767 Master Files/F767_GENERATOR.xls"), col_types = "text") %>% unique()
  }

    if (i %in% c(1985:2000)) {
      gen_xwalk <- read_excel(here::here("data/eia/f767/Boiler_Generator.xls"), col_types = "text") %>% unique()
    } else if (i %in% c(2001:2003)) {
      gen_xwalk <- read_excel(here::here("data/eia/f767/F767_Boiler_Generator.xls"), col_types = "text") %>% unique()
    } else if (i == 2004) {
      gen_xwalk <- read_excel(here::here("data/eia/f767/2004 F767_Boiler_Generator.xls"), col_types = "text") %>% unique()
    } else if (i == 2005) {
      gen_xwalk <- read_excel(here::here("data/eia/f767/2005 EIA-767 Master Files/F767_BOILER_GENERATOR.xls"), col_types = "text") %>% unique()
    }
  
  names(gen_sub) <- tolower(names(gen_sub))
  names(gen_sub)
  
  gen_sub %>% unique_id(utility_code, plant_code, generator_id)
  
  names(gen_xwalk) <- tolower(names(gen_xwalk))
  
  ## in 1999 and 2000, every other observation has NA for "generator association"
  if (i %in% c(1999:2000)){
    print(nrow(gen_xwalk))
    gen_xwalk <- filter(gen_xwalk, !is.na(generator_association))
    print(nrow(gen_xwalk))
  }
  
  print(gen_xwalk %>% unique_id(utility_code, plant_code, generator_id, boiler_id))  
  
  if ("year" %in% names(gen_sub)) {
    gen_sub <- select(gen_sub, -year)
  }
  
  if ("year" %in% names(gen_xwalk)) {
    gen_xwalk <- select(gen_xwalk, -year)
  }
  
  ## join to the boiler level - this will be merged on to boiler, later
  gen_sub <- left_join(gen_xwalk, gen_sub, 
                       by = c("plant_code", "utility_code", "generator_id"))

  gen_sub <- mutate(gen_sub, year = i)
  
  # this is a perfect unique identifer
  print(gen_sub %>% unique_id(plant_code, utility_code, boiler_id, generator_id, year))

  ## Flue-stack information ---------------------------------------------------
  
  if (i %in% c(1985:2000)) {
    flu_sub <- read_excel(here::here("data/eia/f767/Stack_Flue.xls"), col_types = "text") %>% unique()
  } else if (i %in% c(2001:2003)) {
    flu_sub <- read_excel(here::here("data/eia/f767/F767_Stack_Flue.xls"), col_types = "text") %>% unique()
  } else if (i == 2004) {
    flu_sub <- read_excel(here::here("data/eia/f767/2004 F767_Stack_Flue.xls"), col_types = "text") %>% unique()
  } else if (i == 2005) {
    flu_sub <- read_excel(here::here("data/eia/f767/2005 EIA-767 Master Files/F767_STACK_FLUE.xls"), col_types = "text") %>% unique()
  }

  if (i %in% c(1985:2000)) {
    flu_xwalk <- read_excel(here::here("data/eia/f767/Boiler_Stackflue.xls"), col_types = "text") %>% unique()
  } else if (i %in% c(2001:2003)) {
    flu_xwalk <- read_excel(here::here("data/eia/f767/F767_Boiler_Stackflue.xls"), col_types = "text") %>% unique()
  } else if (i == 2004) {
    flu_xwalk <- read_excel(here::here("data/eia/f767/2004 F767_Boiler_Stackflue.xls"), col_types = "text") %>% unique()
  } else if (i == 2005) {
    flu_xwalk <- read_excel(here::here("data/eia/f767/2005 EIA-767 Master Files/F767_BOILER_STACKFLUE.xls"), col_types = "text") %>% unique()
  }

  names(flu_sub) <- tolower(names(flu_sub))
  names(flu_sub)
  
  print(flu_sub %>% unique_id(utility_code, plant_code, flue_id, stack_id))
  
  names(flu_xwalk) <- tolower(names(flu_xwalk))
  print(flu_xwalk %>% unique_id(utility_code, plant_code, flue_id, stack_id, boiler_id))  

  if ("year" %in% names(flu_xwalk)) {
    flu_xwalk <- select(flu_xwalk, -year)
  }
  
  if ("year" %in% names(flu_sub)) {
    flu_sub <- select(flu_sub, -year)
  }
  
  ## join to the boiler level - this will be merged on to boiler, later
  flu_sub <- left_join(flu_xwalk, flu_sub, 
                       by = c("plant_code", "utility_code", "stack_id", "flue_id"))

  print(flu_sub %>% unique_id(plant_code, utility_code, boiler_id, stack_id, flue_id))

  flu_sub <- mutate(flu_sub, year = i)
  
  ## Basic boiler information -------------------------------------------------

  if (i %in% c(1985:2000)) {
    boiler <- read_excel(here::here("data/eia/f767/Boiler.xls"), col_types = "text") %>% unique()
  } else if (i %in% c(2001:2005)) {
    boiler <- read_excel(here::here("data/eia/f767", paste0(i, " F767_BOILER.xls")), col_types = "text") %>% unique()
  }
  
  names(boiler) <- tolower(names(boiler))
  # boiler$inservice_date <- as.Date(as.numeric(boiler$inservice_date), origin = "1899-12-30")
  # boiler$retirement_date <- as.Date(as.numeric(boiler$retirement_date), origin = "1899-12-30")
  
  if ('type_of boiler_std' %in% names(boiler)) {
    boiler <- boiler %>% rename(type_of_boiler = 'type_of boiler_std')
  }
  
  boiler <- mutate(boiler, year = i)
  
  ## this dataset is uniquely identified at the boiler level
  print(boiler %>% unique_id(utility_code, plant_code, boiler_id))
  
  ## Scrubber -----------------------------------------------------------------
  
  if (i %in% c(1985:2000)) {
    fgd <- read_excel(here::here("data/eia/f767/FGD.xls")) %>% unique()
  } else if (i %in% c(2001:2003)) {
    fgd <- read_excel(here::here("data/eia/f767", paste0(i, " F767_FGD.xls"))) %>% unique()
  } else if (i == 2004) {
    fgd <- read_excel(here::here("data/eia/f767/2004_F767_FGD.xls")) %>% unique()
  } else if (i == 2005) {
    fgd <- read_excel(here::here("data/eia/f767/F767_FGD.xls")) %>% unique()
  }
  
  names(fgd) <- tolower(names(fgd))
  print(fgd %>% unique_id(utility_code, plant_code, fgd_id))
  fgd$utility_code <- as.character(fgd$utility_code)
  fgd$plant_code <- as.character(fgd$plant_code)
  
  if (i == 2003) {
    fgd$eff_test_date <- as.Date(tolower(fgd$eff_test_date), "%d-%b-%y")
    fgd$inservice_date <- as.Date(tolower(fgd$inservice_date), "%d-%b-%y")
  } else {
    fgd$eff_test_date <- as.Date(fgd$eff_test_date)
    fgd$inservice_date <- as.Date(fgd$inservice_date)
  }
  
  print(paste("% dates NA: ", 100*sum(is.na(fgd$eff_test_date))/nrow(fgd)))

  names(fgd)
  if("sulfur_emision_rate" %in% names(fgd)){
    fgd <- rename(fgd, sulfur_emission_rate = sulfur_emision_rate)
  }
  
  if (i %in% c(1985:2000)) {
    boiler_xwalk <- read_excel(here::here("data/eia/f767/boiler_FGD.xls")) %>% unique()
  } else if (i %in% c(2001:2003)) {
    boiler_xwalk <- read_excel(here::here("data/eia/f767/F767_BOILER_FGD.xls")) %>% unique()
  } else if (i == 2004) {
    boiler_xwalk <- read_excel(here::here("data/eia/f767/2004 F767_BOILER_FGD.xls")) %>% unique()
  } else if (i == 2005) {
    boiler_xwalk <- read_excel(here::here("data/eia/f767/2005 EIA-767 Master Files/F767_BOILER_FGD.xls")) %>% unique()
  }

  names(boiler_xwalk) <- tolower(names(boiler_xwalk))
  #boiler_xwalk %>% unique_id(utility_code, plant_code, boiler_id)
  boiler_xwalk %>% unique_id(utility_code, plant_code, boiler_id, fgd_id)

  if ("year" %in% names(boiler_xwalk)) {
    boiler_xwalk <- boiler_xwalk %>% select(-year)
  }
    
  boiler_xwalk$utility_code <- as.character(boiler_xwalk$utility_code)
  boiler_xwalk$plant_code <- as.character(boiler_xwalk$plant_code)
  
  fgd_rows <- nrow(boiler_xwalk)

  fgd <- left_join(boiler_xwalk, fgd, by = c("utility_code", "plant_code", "fgd_id"))

  fgd <- fgd %>% mutate(year = i)
  
  print(fgd %>% unique_id(utility_code, plant_code, fgd_id, boiler_id))
    
  print(paste("Same # rows Boiler ID and FGD info", fgd_rows == nrow(fgd)))
  
  print(boiler %>% unique_id(utility_code, plant_code, boiler_id))
    
  ## we want everything at the boiler_id level  
  boilers <- bind_rows(boilers, boiler)
  fgds <- bind_rows(fgds, fgd)
  plants <- bind_rows(plant_sub, plants)
  stackflues <- bind_rows(stackflues, flu_sub) 
  generators <- bind_rows(generators, gen_sub)
  
  ## Remove -------------------------------------------------------------------
  
  l.zip %>%
    purrr::map_chr(\(x) fs::path(fs::path_dir(zip_file), x)) %>%
    fs::file_delete()
  
}

## Export ---------------------------------------------------------------------
boilers %>% unique_id(plant_code, utility_code, boiler_id, year)
fgds %>% unique_id(plant_code, utility_code, fgd_id, boiler_id, year)
plants %>% unique_id(plant_code, utility_code, year)
stackflues %>% unique_id(plant_code, utility_code, stack_id, flue_id, boiler_id, year)
generators %>% unique_id(plant_code, utility_code, boiler_id, generator_id, year)

write.csv(boilers, here::here("data/boilers_1985_2005.csv"))
write.csv(plants, here::here("data/plants_1985_2005.csv"))
write.csv(fgds, here::here("data/fgds_1985_2005.csv"))
write.csv(stackflues, here::here("data/stackflues_1985_2005.csv"))
write.csv(generators, here::here("data/generators_1985_2005.csv"))

## the "type of boiler" variable lets us know which NSPS standards the boiler is subject to
## this can allow us to identify boilers that have been modified.
## I create a crosswalk based on the information given on pg 8 of the EIA767 information:
#     D: Fossil-Fuel Steam Generators where construction began after Aug. 17, 1971
#     Da: Fossil-fuel fired steam generators for which construction began after Sept. 18, 1978
#     Db: Fossil-fuel fired steam generators for which construction began after June 19, 1984
#     Dc: Small industrial-commercial-institutional steam generating units
#     N:  Not covered under NSPS
# 
# boilers <- boilers %>% mutate(inservice_year = year(inservice_date)) %>%
#   mutate(before1978 = ifelse(inservice_year < 1971, 1, 0))
# 
# table(boilers$before1978, boilers$b_type_of_boiler)


### END CODE ###

