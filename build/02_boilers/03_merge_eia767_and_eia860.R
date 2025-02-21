## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 03_merge_eia767_and_eia860
## Bridget Pals
## 27 July 2020
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script merges EIA forms 767 and 860.


### START CODE ###


# PREAMBLE ----------------------------------------------------------------------------------------

## Initiate
## ... Packages
pkgs <- c(
  "fs","here",                                      # File system
  "readxl",                                         # Data reading
  "dplyr","tidyr","lubridate","purrr","zoo"         # Data wrangling
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
datasets <- c("boilers","plants","fgds","stackflues","generators")

## ... Crosswalks
l.vn <- purrr::map(
    seq(1,11),
    \(x) read_excel(here::here("data/xwalk/varname_xwalks.xlsx"), sheet=x)
  ) %>%
  purrr::set_names(nm=xwalk)


# IMPORT ------------------------------------------------------------------------------------------
## Read in early and recent data

l.old <- purrr::map(
    as.list(datasets), 
    \(x) read.csv(here::here("data", paste0(x, "_1985_2005.csv")),
                  stringsAsFactors = FALSE, colClasses = "character") %>%
      select(-X)
  ) %>%
  purrr::set_names(nm=datasets)
l.new <- purrr::map(
    as.list(datasets), 
    \(x) read.csv(here::here("data", paste0(x, "_2007_2018.csv")),
                  stringsAsFactors = FALSE, colClasses = "character") %>%
      select(-X)
  ) %>%
  purrr::set_names(nm=datasets)


# MERGE -------------------------------------------------------------------------------------------

## Combine related xwalks -----------------------------------------------------

l.vn$boiler <- bind_rows(l.vn$boiler_info, l.vn$emission_stand) %>% unique()
l.vn$fgd <- bind_rows(l.vn$fgd, l.vn$boiler_fgd_id) %>% unique()
l.vn$gen <- bind_rows(l.vn$gen, l.vn$boiler_generator) %>% unique()
l.vn$sf <- bind_rows(l.vn$sf, l.vn$boiler_sf_id) %>% unique()

## Combine plant data ---------------------------------------------------------

names(l.old$plants) <- toupper(names(l.old$plants))

orig_name <- as.data.frame(names(l.old$plants), stringsAsFactors = FALSE)
names(orig_name) <- "orig_name"

newname <- left_join(orig_name, l.vn$plant)

newname$newname[newname$orig_name == "NEW_EXP_ABATE_WASTE"] <- "new_exp_abate_waste"
newname$newname[newname$orig_name == "NEW_EXP_ABATE_WATER"] <- "new_exp_abate_water"

print(paste("Number of NA in Name XWalk", sum(is.na(newname))))

names(l.old$plants) <- newname$newname

plants <- bind_rows(l.new$plants, l.old$plants)

### Clean up plant state data -----
## Some plants are assigned to multiple states

test <- plants %>% select(utility_code, plant_code, plt_state) %>% unique()
unique_id(test, utility_code, plant_code)

plants$plt_state[plants$plant_code == 1241 & plants$utility_code == 10000] <- "KS"
plants$plt_state[plants$plant_code == 2122 & plants$utility_code == 3486] <- "MO"
plants$plt_state[plants$plant_code == 7889 & plants$utility_code == 18454] <- "FL"
plants$plt_state[plants$plant_code == 8003 & plants$utility_code == 4362] <- "IL"
plants$plt_state[plants$plant_code == 55340 & plants$utility_code == 18533] <- "AR"

## The remaining plants with multiple states are wind/solar farms that will
## drop out when we merge onto the boiler dataset
test <- plants %>% select(utility_code, plant_code, plt_state) %>% unique()
unique_id(test, utility_code, plant_code)

### Clean up plant county data -----
## There are differences in spelling and such over time for counties.
## For ease, we simply take the most recent value.
plant_id <- plants %>% 
  filter(!is.na(plt_county)) %>%
  group_by(plant_code) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(plt_county, plant_code) %>%
  unique()

## Some quick cleaning of the county vars
plant_id$plt_county <- trimws(plant_id$plt_county)
plant_id$plt_county <- gsub(" County", "", plant_id$plt_county)
plant_id$plt_county <- gsub(" county", "", plant_id$plt_county)
plant_id$plt_county <- gsub("&", "and", plant_id$plt_county)
plant_id$plt_county <- gsub("[[:punct:]]", "", plant_id$plt_county)
plant_id$plt_county <- toupper(plant_id$plt_county)

nrow(plants)  
plants <- plants %>% select(-plt_county) %>%
  left_join(plant_id)
nrow(plants)
sum(is.na(plants$plt_county))

## There is an extra set of lat/long vars
plants$latitude_degrees <- as.numeric(plants$latitude_degrees)
plants$longitude_degrees <- as.numeric(plants$longitude_degrees)
plants$latitude_minutes <- as.numeric(plants$latitude_minutes)
plants$longitude_minutes <- as.numeric(plants$longitude_minutes)

plants <- plants %>%
  mutate(lat2 = latitude_degrees + (latitude_minutes)/60,
         lon2 = longitude_degrees + (longitude_minutes)/60) %>%
  mutate(plt_latitude = ifelse(is.na(plt_latitude), lat2, plt_latitude),
         plt_longitude = ifelse(is.na(plt_longitude), lon2, plt_longitude)) %>%
  select(-lat2, -lon2, -latitude_degrees, -longitude_degrees, -latitude_minutes, -longitude_minutes)

## We drop a few excess vars to make this more manageable (e.g. we will never
## need the contact information)
names(plants)
drop1 <- grep("contact", names(plants))

plants <- plants[-drop1]

names(plants)

## Similarly, we drop info about submission dates/survey dates
names(plants)
drop1 <- grep("survey", names(plants))
plants <- plants[-drop1]

drop2 <- grep("date", names(plants))
plants <- plants[-drop2]
names(plants)

plants <- plants %>%
  select(-first_submit_by, -accept_by, -submit_fail_by, -musthave,
         -data_type, -plt_post_office_name, -post_office_zip4, -post_office_zip5)

## Combine boiler data --------------------------------------------------------

names(l.old$boilers) <- toupper(names(l.old$boilers))

orig_name <- as.data.frame(names(l.old$boilers), stringsAsFactors = FALSE)
names(orig_name) <- "orig_name"

newname <- left_join(orig_name, l.vn$boiler)

print(paste("Number of NA in Name XWalk", sum(is.na(newname))))

names(l.old$boilers) <- newname$newname

boilers <- bind_rows(l.new$boilers, l.old$boilers)

names(boilers)

## The plant data will be more reliable for state/county, so we
## drop those vars here
sum(is.na(boilers$state))
sum(is.na(plants$plt_state))
sum(is.na(boilers$county))
sum(is.na(plants$plt_county))

boilers <- boilers %>% select(-state, -county)
    
## Additional vars to drop (don't want merge on plant_name
## to get messed up, similarly we have capacity information from
## the generators so we drop plant_sum_cap). We also don't worry about
## lab procedures, generally, so we drop those.
boilers <- boilers %>%
  select(-plant_name, -plant_sum_cap, -sampling_procedure, 
         -method_analysis, -lab_performing_analysis)

## We manage some additional cleaning, below, including cleaning
## up inservice dates
table(boilers$inservice_date_code)
boilers <- boilers %>% select(-inservice_date_code)

boilers$boiler_id[boilers$boiler_id %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")] <- 
  paste0("0", boilers$boiler_id[boilers$boiler_id %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")])

## Combine FGD data -----------------------------------------------------------

names(l.old$fgds) <- toupper(names(l.old$fgds))

orig_name <- as.data.frame(names(l.old$fgds), stringsAsFactors = FALSE)
names(orig_name) <- "orig_name"

newname <- left_join(orig_name, l.vn$fgd)

print(paste("Number of NA in Name XWalk", sum(is.na(newname))))

names(l.old$fgds) <- newname$newname

fgds <- bind_rows(l.new$fgds, l.old$fgds)

fgds$energy_consumption_kwh <- as.numeric(fgds$energy_consumption_kwh)
fgds$energy_consumption_mwh <- as.numeric(fgds$energy_consumption_mwh)
  
fgds <- fgds %>% 
  mutate(energy_consumption = ifelse(!is.na(energy_consumption_mwh), energy_consumption_mwh, energy_consumption_kwh),
         energy_units = ifelse(!is.na(energy_consumption_mwh), "MWH", "KWH")) %>%
  select(-energy_consumption_mwh, -energy_consumption_kwh)

fgds$boiler_id[fgds$boiler_id %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")] <- 
  paste0("0", fgds$boiler_id[fgds$boiler_id %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")])

## Combine stackflues data ----------------------------------------------------

names(l.old$stackflues) <- toupper(names(l.old$stackflues))

orig_name <- as.data.frame(names(l.old$stackflues), stringsAsFactors = FALSE)
names(orig_name) <- "orig_name"

newname <- left_join(orig_name, l.vn$sf)

print(paste("Number of NA in Name XWalk", sum(is.na(newname))))

names(l.old$stackflues) <- newname$newname

stackflues <- bind_rows(l.new$stackflues, l.old$stackflues)

stackflues$boiler_id[stackflues$boiler_id %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")] <- 
  paste0("0", stackflues$boiler_id[stackflues$boiler_id %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")])

## Combine generator data ------------------------------------------------------

names(l.old$generators) <- toupper(names(l.old$generators))

orig_name <- as.data.frame(names(l.old$generators), stringsAsFactors = FALSE)
names(orig_name) <- "orig_name"

newname <- left_join(orig_name, l.vn$gen)

print(paste("Number of NA in Name XWalk", sum(is.na(newname))))

names(l.old$generators) <- newname$newname

generators <- bind_rows(l.new$generators, l.old$generators)

print(names(generators))

## Note, generator info is all in MW right now. We change to mmbtu/hr
## since that aligns with all of our regulatory data
generators <- generators %>%
  mutate(nameplate_mmbtu = 3.412*as.numeric(nameplate),
         summer_cap_mmbtu = 3.412*as.numeric(summer_cap),
         winter_cap_mmbtu = 3.412*as.numeric(winter_cap))

generators <- generators %>%
  select(-nameplate, -summer_cap, -winter_cap)

## We do not use monthly generation data and it takes up an 
## enormous amount of space, so we drop those vars.
## First, we check that there is no additional non-missing values
## compared to total_gen.  We determine this is not the case.
sum(is.na(generators$feb_gen))
sum(is.na(generators$total_gen))
sum(is.na(generators$total_gen) & !is.na(generators$feb_gen))

generators <- generators %>%
  select(-jan_gen, -feb_gen, -mar_gen, -apr_gen, -may_gen,
         -jun_gen, -jul_gen, -aug_gen, -sept_gen, -oct_gen,
         -nov_gen, -dec_gen)

unique_id(generators, plant_code, boiler_id, generator_id, year)

nrow(unique_id(generators, plant_code, boiler_id, generator_id, year))

generators <- filter(generators, !is.na(plant_code))

unique_id(generators, plant_code, boiler_id, generator_id, year)

generators$boiler_id[generators$boiler_id %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")] <- 
  paste0("0", generators$boiler_id[generators$boiler_id %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9")])

## Additional cleaning --------------------------------------------------------

### Inservice dates -----
## We start with boilers.
names(boilers)

boilers2003 <- boilers %>%
  filter(year == 2003) %>%
  mutate(retire_clean = dmy(tolower(retirement_date)),
         inservice_clean = dmy(tolower(inservice_date)))

boilersnot2003 <- boilers %>% 
  filter(year != 2003) %>%
  mutate(retire_clean = as.Date(as.numeric(retirement_date), origin = "1899-12-30"),
         inservice_clean = as.Date(as.numeric(inservice_date), origin = "1899-12-30"))

boilers <- bind_rows(boilers2003, boilersnot2003)

boilers <- boilers %>%
  mutate(inservice_m2 = month(inservice_clean),
         inservice_y2 = year(inservice_clean),
         retirement_m2 = month(retire_clean),
         retirement_y2 = year(retire_clean)) 

boilers <- boilers %>%
  mutate(inservice_m = ifelse(is.na(inservice_m), inservice_m2, inservice_m),
         inservice_y = ifelse(is.na(inservice_y), inservice_y2, inservice_y),
         retirement_m = ifelse(is.na(retirement_m), retirement_m2, retirement_m),
         retirement_y = ifelse(is.na(retirement_y), retirement_y2, retirement_y)) %>%
  select(-inservice_m2, -inservice_y2, -retirement_m2, -retirement_y2)

## In 2003, we only have a 2-digit year. We do some cleaning, where we know if the 
## inservice year is in the future than it should be in the prior century and we 
## know if the retirement year is in the future but the status is retired, then it 
## must be that the retirement happened in the prior century.
boilers$inservice_y <- as.numeric(boilers$inservice_y)
boilers$retirement_y <- as.numeric(boilers$retirement_y)

boilers <- boilers %>%
  mutate(inservice_y = ifelse(year == 2003 & inservice_y > year & boiler_status == "OP", 
                              inservice_y - 100, inservice_y),
         retirement_y = ifelse(year == 2003 & retirement_y > year & boiler_status == "RE",
                               retirement_y - 100, retirement_y))

retire <- boilers %>%
  select(plant_code, boiler_id, year, boiler_status) %>%
  filter(boiler_status != "RE") %>%
  group_by(plant_code, boiler_id) %>%
  summarise(calc_retirement_y = max(as.numeric(year))) %>% ungroup()

boilers <- boilers %>% left_join(retire)

hist(boilers$calc_retirement_y)
  
sum(is.na(boilers$calc_retirement_y))

sum(is.na(boilers$inservice_y))/nrow(boilers)

### Boiler types -----
## We clean boiler types.
boilers$boiler_status <- toupper(boilers$boiler_status)

### Coal as primary fuel -----
## See reference table 28 in any recent years' EIA 860 data 
coal <- c("ANT", "BIT", "LIG", "SUB", "SGC", "RC", "WC", "COL")

sum(is.na(boilers$primary_fuel1))

for (i in c(1985:2018)) {
  print(paste0(i, " - ", sum(boilers$primary_fuel1 %in% coal & boilers$year == i)))
}

boilers <- boilers %>% 
  mutate(full_boi_id = paste(utility_code, plant_code, boiler_id),
         full_id = paste(utility_code, plant_code))

## We keep any plants that use coal as a primary at any time in our period
list <- boilers %>% 
  filter(primary_fuel1 %in% coal | primary_fuel2 %in% coal | 
           primary_fuel3 %in% coal | primary_fuel4 %in% coal | 
           primary_fuel5 %in% coal | 
           primary_fuel6 %in% coal | 
           primary_fuel7 %in% coal | 
           primary_fuel8 %in% coal)

length(unique(list$full_boi_id))
length(unique(list$full_id))

test <- boilers %>% filter(full_id %in% list$full_id)
test2 <- boilers %>% filter(full_boi_id %in% list$full_boi_id)

length(unique(test$full_boi_id))
length(unique(test2$full_boi_id))

length(unique(test$full_id))
length(unique(test2$full_id))

# ## we compare to our old way of filtering.
# test <- boilers %>% filter(primary_fuel1 %in% coal)
# 
# length(unique(test$full_id))
# length(unique(list$full_id))

## Filter to only include boilers that fired coal at one point in history.
## We then redefine full_id to make it easier to subset plants.
boilers <- boilers %>% filter(full_boi_id %in% list$full_boi_id) %>%
  select(-full_boi_id) %>%
  mutate(fuel1_is_coal = ifelse(primary_fuel1 %in% coal, 1, 0),
         fuel2_is_coal = ifelse(primary_fuel2 %in% coal, 1, 0),
         fuel3_is_coal = ifelse(primary_fuel3 %in% coal, 1, 0),
         fuel4_is_coal = ifelse(primary_fuel4 %in% coal, 1, 0),
         fuel5_is_coal = ifelse(primary_fuel5 %in% coal, 1, 0),
         fuel6_is_coal = ifelse(primary_fuel6 %in% coal, 1, 0),
         fuel7_is_coal = ifelse(primary_fuel7 %in% coal, 1, 0),
         fuel8_is_coal = ifelse(primary_fuel8 %in% coal, 1, 0)) %>%
  mutate(any_fuel_is_coal = max(fuel1_is_coal, fuel2_is_coal, fuel3_is_coal,
                                fuel4_is_coal, fuel5_is_coal, fuel6_is_coal,
                                fuel7_is_coal, fuel8_is_coal, na.rm = TRUE))

boilers$fuel1_is_coal[is.na(boilers$primary_fuel1)] <- NA
boilers$fuel2_is_coal[is.na(boilers$primary_fuel2)] <- NA
boilers$fuel3_is_coal[is.na(boilers$primary_fuel3)] <- NA
boilers$fuel4_is_coal[is.na(boilers$primary_fuel4)] <- NA
boilers$fuel5_is_coal[is.na(boilers$primary_fuel5)] <- NA
boilers$fuel6_is_coal[is.na(boilers$primary_fuel6)] <- NA
boilers$fuel7_is_coal[is.na(boilers$primary_fuel7)] <- NA
boilers$fuel8_is_coal[is.na(boilers$primary_fuel8)] <- NA

rm(list)
rm(test)
rm(test2)

## A quarter of our sample has something in the NSR dummy.
## We need to think about how to clean this.
sum(!is.na(boilers$nsr_d))/nrow(boilers)

### Steam flow by boiler/plant/utility -----
## Occasionally, a boiler has NA for steam flow.
sum(is.na(boilers$max_steam_flow))

test <- boilers %>% select(utility_code, plant_code, boiler_id, max_steam_flow) %>%
  unique()

## Sometimes a boiler id is associated with multiple max_steam_flows. Sometimes
## it is a small variance, sometimes the other available value is NA or ".".
## We replace NAs using locf.

boilers$max_steam_flow <- as.numeric(boilers$max_steam_flow)

boilers <- boilers %>% arrange(year) %>%
  group_by(plant_code, boiler_id) %>%
  fill(max_steam_flow) %>%
  fill(max_steam_flow, .direction = "up") %>% ungroup()

boilers <- boilers %>% group_by(utility_code, year) %>%
  mutate(utility_steam_flow = sum(max_steam_flow)) %>% ungroup() %>%
  group_by(utility_code, plant_code, year) %>%
  mutate(plant_steam_flow = sum(max_steam_flow)) %>% ungroup()

## We drop state from boiler and bind from plant later (since those values are clean)
nrow(boilers)
boilers <- boilers %>% 
  left_join(select(plants, utility_code, plant_code, plt_state) %>% unique())
nrow(boilers)  

## Inservice date boilers/plants ----------------------------------------------

names(plants)
names(boilers)

table(boilers$type_of_boiler)
sum((is.na(boilers$type_of_boiler)))
hist(as.numeric(boilers$year))
hist(boilers$inservice_y)
sum(is.na(boilers$inservice_y))

## Seven boilers with a future inservice year listed as operating, we assume
## those are meant to be 100 years earlier.
boilers <- boilers %>% 
  mutate(inservice_y = ifelse(inservice_y >= 2025 & boiler_status == "OP", inservice_y - 100, inservice_y))

unique_id(boilers, plant_code, boiler_id, year)

## inservice_y takes multiple values for same boiler_id.
dups <- boilers %>% select(-utility_code) %>%
  group_by(plant_code, boiler_id, inservice_y) %>%
  summarise(n = n()) %>% ungroup()

# test <- boilers %>% filter(boiler_status == "OP") %>%
#   group_by(utility_code, plant_code, boiler_id, inservice_y) %>%
#   summarise(n = n()) %>% ungroup()
# 
# View(unique_id(test, utility_code, plant_code, boiler_id))
 
## This also does not uniquely identify.  From manual inspection, it does not
## appear to be the case that these are retired boilers that.
unique_id(dups, plant_code, boiler_id)

dups <- unique_id(dups, plant_code, boiler_id) %>%
  mutate(id = paste(plant_code, boiler_id))

# View(boilers %>% mutate(id = paste(utility_code, plant_code, boiler_id)) %>%
#        filter(id %in% dups$id) %>% select(-id))

## This problem plagues about 12% of boilers, many of which only appear in the 
## most recent data.
length(unique(dups$id))/length(unique(paste(boilers$utility_code, boilers$plant_code, boilers$boiler_id)))

## Lacking a better option, I choose the mode inservice date (after dropping NAs)
dups <- dups %>% arrange(desc(n)) %>% filter(!is.na(inservice_y)) %>%
  group_by(plant_code, boiler_id) %>%
  mutate(num = 1:n()) %>% ungroup() %>%
  filter(num == 1) %>% select(-num, -n) 

inservice_boi <- boilers %>% 
  select(plant_code, boiler_id, inservice_y) %>%
  group_by(plant_code, boiler_id) %>%
  unique() %>% ungroup() %>%
  mutate(id = paste(plant_code, boiler_id)) %>%
  filter(!(id %in% dups$id))

unique_id(inservice_boi, plant_code, boiler_id)

inservice_boi <- bind_rows(inservice_boi, dups)

unique_id(inservice_boi, plant_code, boiler_id)

inservice <- inservice_boi %>%
  group_by(plant_code) %>%
  mutate(plt_inservice_y = min(inservice_y, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-id)

boilers <- boilers %>% select(-inservice_y)
boilers <- left_join(boilers, inservice)

sum(is.na(boilers$inservice_y))
sum(is.na(boilers$plt_inservice_y))

test <- boilers %>% group_by(utility_code, plant_code) %>%
  mutate(plt_inservice_m = ifelse(inservice_y == plt_inservice_y, inservice_m, NA)) %>%
  fill(plt_inservice_m, .direction = "downup") %>% ungroup()

test <- test %>% select(utility_code, plant_code, boiler_id, plt_inservice_y, plt_inservice_m) %>%
  unique() %>% group_by(utility_code, plant_code) %>%
  mutate(n = n()) %>% ungroup() %>% filter(n != 1)

## we see only a handful of plants have potentially problematic
## splits in month.
# View(test %>% 
#        filter(plt_inservice_y >= 1968 & plt_inservice_y <= 1974) %>% 
#        select(utility_code, plant_code, boiler_id, plt_inservice_y, plt_inservice_m) %>% 
#        arrange(utility_code, plant_code, boiler_id))
# 
# rm(test)
## we will take the earliest month associated with the plant

## First, a tiny bit of cleanup.
boilers$inservice_m[boilers$inservice_m == "."] <- NA

month <- boilers %>% 
  filter(inservice_y == plt_inservice_y) %>%
  group_by(utility_code, plant_code) %>%
  mutate(plt_inservice_m = min(inservice_m, na.rm = TRUE)) %>% ungroup() %>%
  select(utility_code, plant_code, plt_inservice_m) %>% unique()

unique_id(month, utility_code, plant_code)

boilers <- left_join(boilers, month)

## Now, we investigate the NSR dummy.
## 7486 boiler-year pairs with "Y" for NSR dummy  
table(boilers$nsr_d)

nsrtest <- boilers %>% 
  mutate(nsr = ifelse(nsr_d == "Y", 1, 0)) %>%
  group_by(utility_code, plant_code, boiler_id) %>%
  summarise(nsr = max(nsr, na.rm = TRUE))

table(nsrtest$nsr)

table(boilers$type_of_boiler)

## At least 1610 boilers are under some type of NSR and built before 1979.
sum(boilers$type_of_boiler %in% c("d", "D", "da", "Da", "db", "Db", "DB", "Dc") &
      as.numeric(boilers$inservice_y) < 1979, na.rm = TRUE)

hist(as.numeric(plants$year))

## From 1985-2000, data entry adopted "A" instead of "Da" and "B" instead of "Db".
## We recode for consistency.
table(boilers$year, boilers$type_of_boiler)

boilers$type_of_boiler[boilers$type_of_boiler == "A"] <- "Da"
boilers$type_of_boiler[boilers$type_of_boiler == "B"] <- "Db"
boilers$type_of_boiler[boilers$type_of_boiler == "d"] <- "D"
  
## we quickly make our character vars all caps
boilers$type_of_boiler <- toupper(boilers$type_of_boiler)
boilers$boiler_status <- toupper(boilers$boiler_status)

write.csv(boilers, here::here("data/boilers_1985_2018.csv"))

## FGDs -----------------------------------------------------------------------

names(fgds)

fgds$fgd_inservice_date <- ymd(fgds$fgd_inservice_date)

fgds <- fgds %>%
  mutate(fgd_inservice_y2 = year(fgd_inservice_date),
         fgd_inservice_m2 = month(fgd_inservice_date)) %>%
  mutate(fgd_inservice_y = ifelse(is.na(fgd_inservice_y), fgd_inservice_y2, fgd_inservice_y),
         fgd_inservice_m = ifelse(is.na(fgd_inservice_m), fgd_inservice_m2, fgd_inservice_m)) %>%
  select(-fgd_inservice_y2, -fgd_inservice_m2)

## Subset plants in line with boilers -----------------------------------------
plants <- plants %>%
  mutate(full_id = paste(utility_code, plant_code)) %>%
  filter(full_id %in% boilers$full_id)

## Clean ----------------------------------------------------------------------

write.csv(plants, here::here("data/plants_1985_2018.csv"))
write.csv(boilers, here::here("data/boilers_1985_2018.csv"))
write.csv(stackflues, here::here("data/stackflues_1985_2018.csv"))
write.csv(generators, here::here("data/generators_1985_2018.csv"))
write.csv(fgds, here::here("data/fgds_1985_2018.csv"))


# INVESTIGATE -------------------------------------------------------------------------------------
## Preliminary look at plants

names(plants)
plants$year <- as.numeric(plants$year)

## In first half of data, we have fewer obs than in second half, which
## is worrying.  We expect plants to be decreasing, not increasing.
table(as.numeric(plants$year))

## For this analysis, we only consider plants that appear in the
## first half of the data (but we need to come up with a more precise)
## way to manage this.
retired_plants <- plants %>% group_by(utility_code, plant_code) %>%
  summarise(first_year = min(year), ret_year = max(year)) %>%
  filter(first_year < 2005)

hist(retired_plants$ret_year)

table(retired_plants$ret_year)

sum(is.na(plants$naics) & as.numeric(plants$year >= 2007))/sum(as.numeric(plants$year >= 2007))

table(plants$naics)

sum(is.na(plants$plt_county))

names(generators)
names(fgds)
names(stackflues)

hist(as.numeric(stackflues$latitude_degree))
hist(as.numeric(stackflues$longitude_degree))

## We look at na values in stackflues for lat/long.
sum(is.na(stackflues$latitude_degree))/nrow(stackflues)
sum(is.na(stackflues$longitude_degree))/nrow(stackflues)

## Plants latlong has much better coverage.
sum(is.na(plants$plt_latitude))/nrow(plants)
sum(is.na(plants$plt_longitude))/nrow(plants)


### END CODE ###

