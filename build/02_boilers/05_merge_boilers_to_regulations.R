#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Merging!                                          #
# Author: Bridget Pals                              #
# Date: 12/23/2020 updated 02/23/2024               #
# Purpose: Merge plant and reg data                 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# libraries needed
library(dplyr)    # basic data cleaning
library(readxl)   # read in excel files
library(reshape2) # reshape long
library(tidyr)    # "separate" function
 
#setwd("/Users/palsb1/Documents/law school/research/research_revesz")
setwd("~/Documents/law school/research/research_revesz/clone-2023")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -- Functions -------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# credit to: devtools::install.github("edwinth/thatssorandom")
## function to identify whether a group of variables create
## a unique id

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -- Load in Necessary Datasets --------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

aqcr2cty <- read.csv("use_data/crosswalks/aqcrs_to_cty_xwalk.csv", stringsAsFactors = FALSE, skip = 2)
names(aqcr2cty)

## boiler-specific data
boilers <- read.csv("use_data/boilers_1985-2018.csv", stringsAsFactors = FALSE)

## read in fuel data to get early period plants that might have fallen out
fuel <- read.csv("use_data/all_fuel_1970_2018.csv", stringsAsFactors = FALSE)
fuel <- fuel %>% select(-X)

## generators (for capacity data)
gens <- read.csv("use_data/generators_1985-2018.csv", stringsAsFactors = FALSE)

## plants (and add in non abbrev states)
plants <- read.csv("use_data/plants_1985-2018.csv", stringsAsFactors = FALSE)
states <- read.csv("use_data/crosswalks/state_code_xwalk.csv", stringsAsFactors = FALSE)
plants <- left_join(plants, states, by = "plt_state")

## regulations workbook
regs <- read_excel("use_data/regulations/20240213_regulations_by_state.xlsx", skip = 1)

indiana <- read_excel("use_data/regulations/indiana.xlsx", skip = 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -- Clean up Regulations Data ---------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

regs <- regs[,-1]

names(regs) <- c("state", "geo_area", "aqcr", "plt_county", 
                 "town", "manual", "min_cap_any_unit", "max_cap_any_unit",
                 "min_cap", "max_cap", "cap_level", 
                 "pre_post", "year_cutoff", "plant_name", "boiler_id",
                 "utility_code", "plant_code", "other_limits", 
                 "scrubber_est", "std_so2_lbs_mmbtu", 
                 "std_fuel_content", "std_so2_ppm", "std_lbs_so2_hr", 
                 "std_other", "year_adopted", "year_std_start", 
                 "year_std_end", "time_period", "code_section", "notes")

regs <- regs %>% 
  select(-code_section, -notes, -year_adopted, -utility_code) %>% 
  filter(!state %in% c("California", "Ohio"))

## we do some date cleaning next
regs$month_cutoff <- substr(regs$year_cutoff, 1, 2)
regs$month_cutoff <- gsub("/", "", regs$month_cutoff)
regs$month_cutoff <- as.numeric(regs$month_cutoff)
regs$month_cutoff[regs$month_cutoff > 12] <- NA

regs$year_cutoff <- substr(regs$year_cutoff, nchar(regs$year_cutoff)-3,nchar(regs$year_cutoff))

## quick cleaning on Regs
regs <- regs %>%
  mutate(all_ages = ifelse(year_cutoff == "all", 1, 0))

table(regs$all_ages)
sum(is.na(regs$all_ages)) == sum(is.na(regs$year_cutoff))

regs$year_cutoff <- as.numeric(regs$year_cutoff)

## note that there are a handful of regs that have a range of years instead of just pre/post
table(regs$pre_post)

## we see that the regs with NA in year_cutoff map to those
## that were for "all" boilers regardless of inservice year
sum(is.na(regs$year_cutoff)) == sum(regs$all_ages == 1)

## if we don't know when started --> pre 1972 --> just say year 0
## same if we don't have an end date --> persisted to present day
regs$year_std_start <- as.numeric(regs$year_std_start)
regs$year_std_start[is.na(regs$year_std_start)] <- 0

regs$year_std_end <- as.numeric(regs$year_std_end)
regs$year_std_end[is.na(regs$year_std_end)] <- 9999

## clean up cap vars
regs$max_cap[regs$max_cap == "max"] <- 9999999

regs$max_cap <- as.numeric(regs$max_cap)
regs$min_cap <- as.numeric(regs$min_cap)

## we will have to reshape long! This is going to be a bit tricky, but for now
## we go by county or aqcr and flag regs that rely on towns (since we do
## not have that information)
regs$plt_county <- gsub(";", ",", regs$plt_county)
regs$aqcr <- gsub(";", ",", regs$aqcr)

## we have to bind in our indiana data, which we managed separately:
nrow(indiana)
names(indiana) <- c("plant_name", "plant_code", "boiler_id", "generator_id", "std_so2_lbs_mmbtu",
                    "std_other", "year_std_start", "year_std_end",
                    "page", "indiana_county", "addl_sources")

indiana <- indiana %>% select(-page, -addl_sources) %>%
  mutate(state = "Indiana")

regs <- bind_rows(regs, indiana)

regs <- regs %>% mutate(id = 1:n())

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## -- Regs that require handmatching --------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# we had originally dropped plants in Cal, Ohio, Penn and Maine. We only lose one plant
# from Maine and one from California, but 33 from Pennsylvania and 29 from Ohio. 
# Ohio has extraordinarily granular county-level regs that would be infeasible to
# trace to time, so dropping it is necessary. PA is worth further investigation
# test <- plants %>%
#   filter(state %in% c("California", "Ohio", "Pennsylvania", "Maine")) %>%
#   select(plant_code, state) %>% unique()
# 
# table(test$state)
# rm(test)

## New Mexico designates its AQCRs depending on whether a plant is on the east or west
## side of the continental divide. We do this manually, as well. Masschusetts designates
## regulations by town/city, so that matching is also done by hand.
handmatch <- plants %>% 
  filter(state %in% c("Pennsylvania", "Maine", "Massachusetts", "New Mexico")) %>%
  select(plant_code, plant_name, plt_state, state, plt_county, plt_st_address, plt_city) %>% unique()

write.csv(handmatch, "use_data/to_match_pa_maine_nm_mass.csv")

## I handmatched these plants to the correct region
## for Pennsylvania, counties are not coterminous with AQCRs. 
## I relied on the map available at the below link and the provided addresses to determine
## if a given plant was within an air basin (and, if so, which one). 
## ## https://files.dep.state.pa.us/air/AirQuality/AQPortalFiles/Regulations%20and%20Clean%20Air%20Plans/attain/PM25Des/FigureB17-PA%20Air%20Basins.pdf
## Certain regions changed names over time - for example, Beaver Valley was broken out into
## Upper and Lower Beaver Valley - so I added in a T1 and T2 distinction in the file to rename these
##
## for Maine, AQCRs may include only part of a county, delineated by township. For the one Maine
## powerplant in our sample, I was able to determine the town was in the Androscoggin AQCR
##
## for Massachusetts, AQCRs were based in part on township, and I matched the township
## of these plants to the correct AQCR
##
## for New Mexico, the AQCRs are defined in part with reference to geographical features
## (e.g. the border of one of the AQCRs include only the part of a county east of the continental divide)

handmatch <- read.csv("use_data/handmatched_pa_maine_nm_mass.csv") %>%
  select(-plt_county, -plt_st_address, -plt_city)

## we have to fix something in Pennsylvania for our merge to work
pat1 <- handmatch %>% filter(match_name == "Nonair basin area") %>%
  mutate(time_period = "T1")

pat2 <- handmatch %>% filter(match_name == "Nonair basin area") %>%
  mutate(time_period = "T2")

pa <- bind_rows(pat1, pat2)

handmatch <- handmatch %>% filter(match_name != "Nonair basin area") %>%
  bind_rows(pa)

plants_handmatch <- plants %>% 
  filter(state %in% c("Pennsylvania", "Maine", "Massachusetts", "New Mexico")) %>%
  left_join(handmatch)
  
## we check our handmatch guys for uniqueness:
plants_handmatch_id <- plants_handmatch %>% 
  select(state, plant_code, time_period, match_name, plt_county) %>% unique()

## we get a unique ID based on this
unique_id(plants_handmatch_id, plant_code, time_period)

## we do the same for the rest of the dataset
plants <- plants %>%
  filter(!state %in% c("California", "Ohio"))

plant_id <- plants %>% 
  select(state, plt_county, plant_code) %>% unique()

unique_id(plant_id, plant_code)

## next, we manage our plants that involved handmatching (PA, ME, NM, MA)

hm_regs_wide <- regs %>% 
  filter(state %in% c("Pennsylvania", "Maine", "New Mexico", "Massachusetts"))  %>%
  select(id, state, geo_area, time_period) %>%
  separate(geo_area, c(paste0("geo_area", 1:30)), c(","))

hm_regs_long <- hm_regs_wide %>% 
  melt(id.vars = c("id", "state", "time_period")) %>%
  filter(!is.na(value)) %>% rename(match_name = value) %>%
  select(-variable, -state)

hm_regs_long$match_name <- trimws(hm_regs_long$match_name)

## we also need to clean up the handmatched plants file
plants_handmatch_id$match_name <- trimws(plants_handmatch_id$match_name)

hm_plts <- left_join(plants_handmatch_id, hm_regs_long, by = c("match_name", "time_period")) %>%
  select(id, state, match_name, time_period, plt_county, plant_code) 

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## specific plants --------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Some states choose to regulate specific plants, sometimes by individual boiler. 
# We link these up here. Because states named the plants, rather than providing 
# a plant code, manual review was necessary.

# I used the code below to allow for manual review to link the utility and plant
# codes to the regulations, based off the name of the plant.
#
# There are 195 such constraints in our dataset, including 129 in Indiana.
nrow(indiana)
nrow(regs %>% filter(!is.na(plant_code)))

names_plts <- regs %>% filter(!is.na(plant_code), plant_code != "") %>%
  select(id, state, geo_area, plt_county, plant_code, boiler_id, generator_id)

## NOTE: when we merge these onto boilers, we need to be careful - some of these map to
## a specific boiler, one onto a generator, and many to an entire plant

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# regs with no geo restrictions -------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## we look at regulations that are not tied to a specific geographic area
regs_nongeo <- filter(regs, geo_area == "all") %>%
  filter(is.na(plant_name)) %>%
  filter(!state %in% c("New Mexico", "Pennsylvania", "Maine", "Massachusetts")) %>%
  select(id, state, geo_area)

nongeo_plts <- left_join(plant_id, regs_nongeo, by = "state")

## about 64% of plants are associated with one or more of these regs (we have not
## yet filtered based on size or time, so that number will change)
nrow(nongeo_plts %>% filter(!is.na(id)) %>% select(plant_code) %>% unique())/
  nrow(nongeo_plts %>% select(plant_code) %>% unique())

nongeo_plts <- filter(nongeo_plts, !is.na(id)) %>%
  select(id, state, geo_area, plant_code) 

rm(regs_nongeo)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## -- Link County-Based Regs ----------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## first, we start by making the county data cleaner to allow for joining
regs_cty_wide <- regs %>% 
  select(id, state, geo_area, plt_county) %>%
  filter(!is.na(plt_county), plt_county != "all") %>%
  ## we take out our handmatched guys - PA, ME, MA, and NM   
  filter(!state %in% c("Pennsylvania", "Maine", "Massachusetts", "New Mexico")) %>%
  separate(plt_county, c(paste0("county", 1:30)), c(","))

regs_cty_long <- regs_cty_wide %>% 
  reshape2::melt(id.vars = c("id", "state", "geo_area")) %>%
  filter(!is.na(value)) %>% rename(plt_county = value) %>%
  select(-variable)

regs_cty_long$plt_county <- trimws(regs_cty_long$plt_county)
regs_cty_long$plt_county <- gsub(" County", "", regs_cty_long$plt_county)
regs_cty_long$plt_county <- gsub(" county", "", regs_cty_long$plt_county)
regs_cty_long$plt_county <- gsub("&", "and", regs_cty_long$plt_county)
regs_cty_long$plt_county <- gsub("[[:punct:]]", "", regs_cty_long$plt_county)
regs_cty_long$plt_county <- toupper(regs_cty_long$plt_county)

cty_plts <- left_join(plant_id, regs_cty_long, by = c("plt_county", "state")) %>%
  select(id, state, geo_area, plt_county, plant_code) 

## about 21% of plants in our sample may be associated with one of these regs (again,
## we don't know for sure until we filter on plant size/vintage/year/etc)
nrow(cty_plts %>% filter(!is.na(id)) %>% select(plant_code) %>% unique())/
  nrow(cty_plts %>% select(plant_code) %>% unique())

cty_plts <- cty_plts %>% filter(!is.na(id))

## we also want to check how good of "coverage" we get of the regs
cty_regs <- left_join(regs_cty_long, plant_id, by = c("plt_county", "state")) %>%
  select(id, state, plt_county, plant_code) %>%
  filter(!is.na(id))

## there are many counties (229) that don't contain a single powerplant in our
## dataset (perhaps contain smaller powerplants that we don't have or industrial sources)
sum(is.na(cty_regs$plant_code))

## about a quarter of these regs are regs that apply to "other" counties, 
## which we must treat separately
sum(cty_regs$plt_county %in% c("OTHER") & is.na(cty_regs$plant_code))

rm(regs_cty_wide)
rm(cty_regs)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## -- AQCR ----------------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## next, we reshape AQCR long (as we did for county) and also crosswalk
## AQCR data to counties so that we will be able to join by county
regs_aqcr_wide <- regs %>% select(id, state, geo_area, aqcr) %>%
  filter(!is.na(aqcr), aqcr != "all") %>%
  ## we take out our handmatched guys - PA, ME, MA, and NM 
  filter(!state %in% c("Pennsylvania", "Maine", "Massachusetts", "New Mexico")) %>%
  separate(aqcr, c(paste0("aqcr", 1:30)), c(","))

regs_aqcr_long <- regs_aqcr_wide %>% 
  reshape2::melt(id.vars = c("id", "state", "geo_area")) %>%
  filter(!is.na(value)) %>% rename(aqcr = value)

regs_aqcr_long$aqcr <- gsub(" ", "", regs_aqcr_long$aqcr)

aqcr2cty_wide <- separate(aqcr2cty, counties, c(paste0("county", 1:100)), ",")

aqcr2cty_long <- aqcr2cty_wide %>% select(-towns, -other, -cfr_section, -history) %>%
  reshape2::melt(id.vars = c("aqcr", "aqcr_name", "state",
                   "year_start", "year_end"))

aqcr2cty_long <- aqcr2cty_long %>% filter(!is.na(value), value != "", value != " ") %>% 
  rename(plt_county = value) %>%
  select(-variable)

## the only counties with AQCRs that change back and forth over time are in New Mexico and Texas.
## for the Texas counties, the AQCRs changed in 1991 and a new rule - that 
## does not consider AQCR - took effect in 1992. I will assume these actions
## were paired.
## the New Mexico redesignation took effect in 1980, but New Mexico stopped
## basing its standards off AQCR in 1977, so it is not material.

## we can therefore filter out these later changes, and say 
## "keep if start year (of the AQCR designation) is 1971)
#View(aqcr2cty_long %>% filter(year_start != 1971 | year_end != "present"))

aqcr2cty_long <- filter(aqcr2cty_long, year_start == 1971) %>% unique()

# we have to do a little string cleaning
aqcr2cty_long$plt_county <- gsub(" County", "", aqcr2cty_long$plt_county)
aqcr2cty_long$plt_county <- gsub(" county", "", aqcr2cty_long$plt_county)
aqcr2cty_long$plt_county <- trimws(aqcr2cty_long$plt_county)
aqcr2cty_long$plt_county <- gsub("&", "and", aqcr2cty_long$plt_county)
aqcr2cty_long$plt_county <- gsub("[[:punct:]]", "", aqcr2cty_long$plt_county)
aqcr2cty_long$plt_county <- toupper(aqcr2cty_long$plt_county)

unique_id(aqcr2cty_long, aqcr, plt_county, state)

names(aqcr2cty_long)

regs_aqcr_long <- mutate(regs_aqcr_long, aqcr = ifelse(aqcr == "other", 9999, aqcr))

aqcr2cty_long$aqcr <- as.numeric(aqcr2cty_long$aqcr)
regs_aqcr_long$aqcr <- as.numeric(regs_aqcr_long$aqcr)

regs_aqcr <- left_join(regs_aqcr_long, aqcr2cty_long, by = c("state", "aqcr"))

## now we merge the reg information onto the plant information 
aqcr_plts <- plant_id %>% filter(!state %in% c("Pennsylvania", "Maine", "Massachusetts", "New Mexico")) %>%
  left_join(regs_aqcr, by = c("state", "plt_county")) %>%
  select(id, state, geo_area, plt_county, aqcr, plant_code)

## lets check how many plants this reaches - it looks like just shy of 10%
nrow(aqcr_plts %>% filter(!is.na(id)) %>% select(plant_code) %>% unique())/
  nrow(aqcr_plts %>% select(plant_code) %>% unique())

## upon inspection, something is off in Virginia, because the AQCRs are identified by
## city and county, and some plants have listed their city instead of their county.
## we fix this manually.
aqcr_va <- aqcr_plts %>% filter(state == "Virginia" & is.na(aqcr))

aqcr_va$aqcr[aqcr_va$plant_code == 10071] <- 223
aqcr_va$aqcr[aqcr_va$plant_code == 10377] <- 225
aqcr_va$aqcr[aqcr_va$plant_code == 3788] <- 47
aqcr_va$aqcr[aqcr_va$plant_code == 3803] <- 223
aqcr_va$aqcr[aqcr_va$plant_code == 50900] <- 226
aqcr_va$aqcr[aqcr_va$plant_code == 54081] <- 225 # match to the city "Richmond" (AQCR 225), not the county (AQCR 224), although it doens't matter since both are subject to the same regs

regs_va <- regs_aqcr %>% filter(state == "Virginia") %>%
  select(-plt_county) %>% unique()

aqcr_va <- aqcr_va %>% select(state, plant_code, aqcr) %>% 
  left_join(regs_va)

## we add back in these Virginia plants and remove unmatched plants
aqcr_plts <- aqcr_plts %>%
  filter(!is.na(aqcr)) %>%
  bind_rows(aqcr_va)

rm(aqcr2cty)
rm(aqcr2cty_wide)
rm(aqcr2cty_long)
rm(regs_aqcr_long)
rm(regs_aqcr_wide)
rm(regs_aqcr)
rm(aqcr_va)

## the plants by AQCR and county are now linked properly by geography!!
## note that not all plants have been joined in - some, for
## example only apply if they are in an "other" geographic
## area. This is going to take a bit of work to make sure
## we are applying "other" to the correct time frame (e.g. if
## in t1 the reg applies to counties A, B, C, and other and in
## t2 the reg applies to counties A, B, and other, a plant in
## county D would be "other" in both time frames, but county C
## would need to be "other" in some cases and not in others). We do this next.

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## "other" geo areas ------------------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## we look at regs in "other" county or "other" AQCR

# we had to wait to do "other" geo areas until after everything
# else is linked up since the "other" can vary by year
## we catch all "others" by subsetting on plt_county

sum(regs$plt_county %in% c("Other", "other", "OTHER") & !regs$aqcr %in% c("Other", "other", "OTHER"))
sum(!regs$plt_county %in% c("Other", "other", "OTHER") & regs$aqcr %in% c("Other", "other", "OTHER"))

regs_other <- regs %>% filter(plt_county %in% c("Other", "other", "OTHER"))

regs_other <- regs_other %>%
  select(state, id, geo_area, plt_county, time_period) %>%
  ## clean up var for easier linkage
  mutate(plt_county = "OTHER")

# we're going to have to go by state. It's a bit messy, but bear with me.

# Alabama
plts_al <- plant_id %>% 
  filter(state == "Alabama",
         ## we take out our "Category I Counties" - Colbert County has a separate regulation
         ## for certain plants, but the default would be that a plant is covered by the "other" 
         ## or "Category II County" rule
         !plt_county %in% c("JACKSON", "JEFFERSON", "MOBILE")) %>%
  mutate(plt_county = "OTHER", time_period = "T2")

# Delaware
plts_del <- plant_id %>%
  filter(state == "Delaware", plt_county != "NEW CASTLE") %>%
  mutate(plt_county = "OTHER")

# Florida
plts_fl <-plant_id %>%
  filter(state == "Florida", !plt_county %in% c("ESCAMBIA", "HILLSBOROUGH", "DUVAL")) %>%
  mutate(plt_county = "OTHER")

# Illinois

## n.b. McHenry and Kankakee are in the IL Metros, but when we wrote out
## the regulations, we kept them on a separate line for ease of coding
## because they sometimes follow the IL Metro rule and sometimes don't,
## but they are never within the "other" group
il_metros <- c("Cook", "Lake", "Will", "DuPage", "McHenry", "Kane", "Grundy", 
               "Kendall", "Kankakee", "St Clair", "Saint Clair", "Madison", "Peoria", "Tazewell")
il_metros <- toupper(il_metros)

plts_il <- plant_id %>%
  filter(state == "Illinois",
         !plt_county %in% il_metros) %>%
  mutate(plt_county = "OTHER")

# Iowa
plts_ia <- plant_id %>%
  filter(state == "Iowa", 
         !plt_county %in% c("BLACK HAWK", "CLINTON", "DES MOINES",
                            "DUBUQUE", "JACKSON", "LEE", "LINN", "LOUISA", "MUSCATINE", "SCOTT")) %>%
  mutate(plt_county = "OTHER")

# Kentucky
# these counties are always separately regulated. Only after 1982, is Boyd also
# separately regulated
ky_inc <- c("Jefferson", "McCracken", "Bell", "Clark", "Woodford", 
            "Pulaski", "Webster", "Hancock", "Muhlenberg")
ky_inc <- toupper(ky_inc)

# as such, we will "save" Boyd for now, and recode when we are actually merging
plts_ky <- plant_id %>%
  filter(state == "Kentucky",
         !plt_county %in% ky_inc)

boyd <- plts_ky %>% 
  mutate(plt_county = ifelse(plt_county == "BOYD", "BOYD", "OTHER"),
         time_period = "T2")

plts_ky <- plts_ky %>%
  mutate(plt_county = "OTHER", time_period = "T1") %>%
  bind_rows(boyd)

# # Massachusetts
# we switched to a handmatching approach for Massachusetts, so we no longer need this
# plts_ma <- plant_id %>%
#   filter(state == "Massachusetts", 
#          !plt_county %in% c("SUFFOLK", "NORFOLK", "MIDDLESEX", 
#                             "ESSEX", "PLYMOUTH")) %>%
#   mutate(plt_county = "OTHER")

# Minnesota

## we exclude counties in accoutned for AQCRs and Duluth
minn_inc <- c("Anoka County", "Carver County", "Dakota County", 
              "Hennepin County", "Ramsey County", "Scott County", 
              "Washington County", "Duluth")
minn_inc <- gsub(" County", "", minn_inc)
minn_inc <- toupper(minn_inc)

plts_minn <- plant_id %>%
  filter(state == "Minnesota",
         !plt_county %in% minn_inc) %>%
  mutate(plt_county = "OTHER")

# Michigan
plts_mi <- plant_id %>%
  filter(state == "Michigan",
         plt_county != "WAYNE") %>%
  mutate(plt_county = "OTHER")

# Nevada
plts_nev <- plant_id %>%
  filter(state == "Nevada",
         !plt_county %in% c("WASHOE", "CLARK")) %>%
  mutate(plt_county = "OTHER")

# New Jersey
# In 1981, New Jersey modified their rules and each county was 
# explicitly subject to a particular set of rules.
# as such, we only want to recode for plants prior to 1981.

nj_inc <- c("Atlantic", "Cape May", "Cumberland", "Hunterdon", 
            "Ocean", "Sussex", "Warren")
nj_inc <- toupper(nj_inc)

plts_nj <- plant_id %>%
  filter(state == "New Jersey", !plt_county %in% nj_inc) %>%
  mutate(plt_county = "OTHER") %>% mutate(time_period = "T1")

# New Mexico

# we no longer need this code as I switched to a handmatch strategy for NM
# New Mexico has irregular borders, so it was actually easier
# to track down which counties are not in AQCR 12 and 14, rather than
# the other way around
# nm_inc <- c("Colfax", "Union", "Mora", "Harding", "San Miguel", "Guadalupe", 
#             "Torrance", "Quay", "Curry", "DeBaca", "Roosevelt", "Chaves", "Lea", 
#             "Eddy", "Lincoln", "Otero", "Sierra", "Dona Ana", "Socerro", "Catron", 
#             "Cibola", "Sandoval", "Valencia", "Bernalillo", "Santa Fe", "Los Alamos", "Taos")
# nm_inc <- toupper(nm_inc)
# 
# plts_nm <- plant_id %>%
#   filter(state == "New Mexico", plt_county %in% nm_inc) %>%
#   mutate(plt_county = "OTHER")

# New York
ny_inc <- c("Bronx", "Kings", "Queens", "New York", "Richmond", "Nassau", 
            "Rockland", "Westchester", "Suffolk", "Erie", "Niagara")
ny_inc <- toupper(ny_inc)

plts_ny <- plant_id %>%
  filter(state == "New York", !plt_county %in% ny_inc) %>%
  mutate(plt_county = "OTHER")

# South Carolina
plts_sc <- plant_id %>%
  filter(state == "South Carolina", 
         !plt_county %in% c("CHARLESTON", "AIKEN", "ANDERSON"))

unique(plts_sc$plt_county)

plts_sc <- plts_sc %>% 
  # this line is unnecessary
  #  filter(plt_county != "NOT IN FILE") %>%
  mutate(plt_county = "OTHER")

# Tennessee
tn1 <- c("Polk", "Maury", "Sullivan", "Roane", "Humphreys")
tn1 <- toupper(tn1)

# note T2 and T3 have same counties regulated, so we'll lean on that
tn2 <- c("Polk", "Humphreys", "Maury", "Roane", "Sullivan", "Shelby", 
         "Anderson", "Davidson", "Hamilton", "Hawkins", "Knox", "Rhea")
tn2 <- toupper(tn2)

tn_plts1 <- plant_id %>%
  filter(state == "Tennessee", 
         !plt_county %in% tn1) %>%
  mutate(plt_county = "OTHER", time_period = "T1")

tn_plts2 <- plant_id %>%
  filter(state == "Tennessee", 
         !plt_county %in% tn2) %>%
  mutate(plt_county = "OTHER", time_period = "T2")

tn_plts3 <- tn_plts2 %>% mutate(time_period = "T3")

plts_tn <- bind_rows(tn_plts1, tn_plts2) %>% bind_rows(tn_plts3)

# Texas
plts_tx <- plant_id %>%
  filter(state == "Texas", 
         plt_county != "MILAM") %>%
  mutate(plt_county = "OTHER")

# West Virginia
## we might worry about how best to code Fayette County, because different parts of the
## county are regulated by different rules, however, conveniently, there are no plants in our sample in 
## Fayette County, so it does not matter.
wv_inc <- c("Brooke", "Hancock", "Marshall", "Ohio", "Grant", "Mineral", "Jackson", 
            "Pleasants", "Tyler", "Wetzel", "Wood", "Kanawha", "Putnam", "Fayette")
wv_inc <- toupper(wv_inc)

plts_wv <- plant_id %>%
  filter(state == "West Virginia", 
         !plt_county %in% wv_inc) %>%
  mutate(plt_county = "OTHER")

# Missouri
mo_inc <- c("Franklin", "Jefferson", "St Charles", "St Louis City", "St Louis")
mo_inc <- toupper(mo_inc)

plts_mo <- plant_id %>%
  filter(state == "Missouri",
         !plt_county %in% mo_inc) %>%
  mutate(plt_county = "OTHER")

# combine all states
other_plts <- bind_rows(plts_al, plts_del) %>%
  bind_rows(plts_fl) %>%
  bind_rows(plts_il) %>%
  bind_rows(plts_ia) %>%
  bind_rows(plts_ky) %>%
#  bind_rows(plts_ma) %>%
  bind_rows(plts_mi) %>%
  bind_rows(plts_minn) %>%
  bind_rows(plts_nev) %>%
  bind_rows(plts_nj) %>%
#  bind_rows(plts_nm) %>%
  bind_rows(plts_ny) %>%
  bind_rows(plts_sc) %>%
  bind_rows(plts_tn) %>%
  bind_rows(plts_tx) %>%
  bind_rows(plts_wv) %>%
  bind_rows(plts_mo)

## we merge the regs onto these plants
other_plts <- left_join(other_plts, regs_other)

unique_id(other_plts, id, plant_code)
sum(is.na(other_plts$id))
sum(is.na(other_plts$plant_code))

## there are 151 (non-handmatched) plants that are in "other" geographical areas at some point in time.
## some of these may be plants that also have a plant/boiler/generator specific regulation
nrow(other_plts %>% select(plant_code) %>% unique())

## combine the above groups ----
names(names_plts)
names(cty_plts)
names(aqcr_plts)
names(other_plts)

## we can drop the time_period variable because we now have the regulation ID, which
## is already tied to the correct time period
other_plts <- select(other_plts, -time_period)

names_plts <- names_plts %>% mutate(geo_type = "targeted reg")
cty_plts <- cty_plts %>% mutate(geo_type = "county")
aqcr_plts <- aqcr_plts %>% mutate(geo_type = "AQCR")
nongeo_plts <- nongeo_plts %>% mutate(geo_type = "all")
hm_plts <- hm_plts %>% mutate(geo_type = "handmatch")
other_plts <- other_plts %>% mutate(geo_type = "catch-all designation")
  
plt_regs <- bind_rows(names_plts, cty_plts) %>%
  bind_rows(aqcr_plts) %>%
  bind_rows(nongeo_plts) %>%
  bind_rows(hm_plts) %>%
  bind_rows(other_plts) %>%
  select(id, state, plt_county, plant_code, boiler_id, generator_id, geo_type)

## we have no duplicates!
unique_id(plt_regs, plant_code, id)

## these are only MATCHED plants
sum(is.na(plt_regs$id))
sum(!is.na(plt_regs$id))

## we have linked up 473 plants (out of 472) to at least one possible regulation.
## some of this will change after we eliminate plants outside the regulations' parameters
## for size or date built, but it shows we had good initial coverage.
length(unique(plt_regs$plant_code))
length(unique(plants$plant_code))

## you'll note that we have matched one more plant here than is in our underlying plants dataset,
## which seems, well, not good. 
## The reason for this is that the Village of Winnetka Plant (plant code 972) did not report to 
## the EIA form we use as its base (fuel consumption in EIA 767), but, nonetheless, is separately regulated
## by name and is, in fact, a coal plant. We leave it in for now.
plt_regs$plant_code[!plt_regs$plant_code %in% plants$plant_code]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -- Capacity Data for Boilers/Plants --------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names(gens)

# 5% of data is missing for nameplate mmbtu, 21% for each summer and winter capacity
sum(is.na(gens$nameplate_mmbtu))/nrow(gens)
sum(is.na(gens$summer_cap_mmbtu))/nrow(gens)
sum(is.na(gens$winter_cap_mmbtu))/nrow(gens)

sum(is.na(gens$state))/nrow(gens) ## state is missing 20% of the time
sum(is.na(gens$county))/nrow(gens) ## county is missing 32% of the time
sum(is.na(gens$boiler_id))/nrow(gens) ## boiler ID is missing 58% of the time
sum(is.na(gens$plant_code))/nrow(gens) ## plant code is always present

gen_by_boi <- gens %>%
  select(year, state, plant_code, boiler_id, 
         generator_id, nameplate = nameplate_mmbtu)

## only uniquely identifies less than a third of obs
#unique_id(gen_by_boi, year, plant_code, boiler_id)
nrow(unique_id(gen_by_boi, year, plant_code, boiler_id))/nrow(gen_by_boi)

## uniquely identifies 75% of data
nrow(unique_id(gen_by_boi, year, plant_code, generator_id))/nrow(gen_by_boi)

## uniquely identifies (some boilers must go to multiple generators and vice versa)
unique_id(gen_by_boi, year, plant_code, boiler_id, generator_id)

## very few obs are 0, so we are in good shape there
sum(gen_by_boi$nameplate == 0, na.rm = TRUE)

## about 5% of nameplate obs are NA, however, we would not expect the nameplate to
## dramatically change from one year to another, so we may be able to work around this
sum(is.na(gen_by_boi$nameplate))
sum(is.na(gen_by_boi$nameplate))/nrow(gen_by_boi)

hist(gen_by_boi$nameplate)

gen_by_boi <- gens %>%
  select(year, plant_code, boiler_id, generator_id, nameplate_mmbtu) %>%
  rename(nameplate = nameplate_mmbtu) %>%
  filter(!is.na(boiler_id)) %>%
  group_by(year, plant_code, boiler_id) %>%
  summarise(max_boi_nameplate = sum(nameplate),
         min_boi_nameplate = sum(nameplate, na.rm = TRUE)) %>%
  ungroup() 

## the difference between max and min, is just that MAX recognizes it could be larger, but 
## for the NA. This affects 10% of obs
test <- gen_by_boi %>% mutate(diff = max_boi_nameplate - min_boi_nameplate)

  hist(test$diff)
  sum(test$diff == 0, na.rm = TRUE)/nrow(test)
  sum(is.na(test$diff))/nrow(test)
  sum(test$diff == 0, na.rm = TRUE) - sum(!is.na(test$diff))

rm(test)

gen_by_plant <- gens %>%
  select(year, plant_code, generator_id, nameplate_mmbtu) %>%
  rename(nameplate = nameplate_mmbtu) %>% unique() 

## we quickly check that this uniquely identifies (e.g. we don't randomly have
## multiple of the same generator in a given year)
nrow(unique_id(gen_by_plant, year, plant_code, generator_id))

## we sum by plant_code and year, to get the total capacity by plant
gen_by_plant <- gen_by_plant %>% 
  group_by(year, plant_code) %>%
  summarise(plant_capacity = sum(nameplate),
         min_plant_capacity = sum(nameplate, na.rm = TRUE)) %>%
  ungroup()

gen_summary <- left_join(gen_by_plant, gen_by_boi)

## we are properly identified at the boiler/year level
nrow(unique_id(gen_summary, year, plant_code, boiler_id))

## we check to see if the boiler nameplate is consistent over time
test <- gen_by_boi %>% group_by(plant_code, boiler_id) %>%
  summarise(diff = max(min_boi_nameplate) - min(min_boi_nameplate))

## we see there are a fair amount of changes over the course of this period
hist(test$diff)
sum(test$diff == 0)/nrow(test)
sum(test$diff <= 50)/nrow(test)
sum(test$diff > 250)/nrow(test)

## NOTE, WE END UP NOT USING ANY PRE 1985 DATA, SO I HAVE COMMENTED THIS SECTION OUT

## we are going to take the avg nameplate for each generator/boiler, too, for use
## in periods wehre we do not have generator data
# gen_ids <- gens %>%
#   select(plant_code, generator_id, boiler_id, nameplate_mmbtu, gen_op_y, gen_retire_y) %>%
#   rename(nameplate = nameplate_mmbtu) %>%
#   group_by(plant_code, generator_id, boiler_id) %>%
#   summarise(gen_op_y = min(gen_op_y, na.rm = TRUE), ## we take min start operating year in case
#             gen_retire_y = max(gen_retire_y, na.rm = TRUE), ## we take max retirement in case retirement was pushed forward
#             nameplate_avg_gen = mean(nameplate, na.rm = TRUE)) %>% 
#   ungroup() %>%
#   mutate(join = 1)
# 
# gen_ids$gen_retire_y[is.na(gen_ids$gen_retire_y)] <- 9999
# 
# unique_id(gen_ids, plant_code, boiler_id, generator_id)
# 
# years <- data.frame(year = c(1972:1984), join = 1)
# 
# ## we create skeleton dataset based on what we know
# ## about generators operating at the time. For a given year
# ## we only include generators that were already up and running
# gen_pre1985 <- full_join(gen_ids, years) %>% select(-join) %>%
#   filter(gen_op_y <= year, gen_retire_y >= year)
# 
# gen_by_boi_pre1985 <- gen_pre1985 %>%
#   group_by(year, plant_code, boiler_id) %>%
#   summarise(max_boi_nameplate = sum(nameplate),
#          min_boi_nameplate = sum(nameplate, na.rm = TRUE)) %>%
#   ungroup() 
# 
# gen_by_plant_pre1985 <- gen_pre1985 %>%
#   select(year, plant_code, generator_id, nameplate) %>% unique() %>% 
#   group_by(year, plant_code) %>%
#   summarise(plant_capacity = sum(nameplate),
#             min_plant_capacity = sum(nameplate, na.rm = TRUE)) %>%
#   ungroup()
# 
# gen_summary_pre1985 <- left_join(gen_by_plant_pre1985, gen_by_boi_pre1985)
# 
# gen_summary <- bind_rows(gen_summary, gen_summary_pre1985)

rm(gen_by_boi)
#rm(gen_by_boi_pre1985)
rm(gen_by_plant)
#rm(gen_by_plant_pre1985)
#rm(gen_summary_pre1985)

## note, the NAs in utility code are from a random set of
## descriptive notes that were carried into the dataset - no worries
gen_summary$plant_code <- as.numeric(gen_summary$plant_code)

unique_id(gen_summary, year, plant_code, boiler_id)

names(gen_summary)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -- Combine above to get accurate merge!!! --------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
linked <- unique(plt_regs$plant_code)
unlinked <- unique(plants$plant_code)
unlinked <- unlinked[!unlinked %in% linked]

## all of our guys are linked up (again, this will change as we take into account
## vintage and size and time period)
length(linked)/(length(unlinked) + length(linked))

## first, a tiny bit more cleaning on the regulations files
merge_regs <- plt_regs %>% 
  select(id, state, plant_code, boiler_id, generator_id, geo_type)

table(merge_regs$geo_type)
unique_id(merge_regs, plant_code, id)

## 150 links (from targeted constraints) contain a boiler ID (these are individually regulated boilers)
sum(!is.na(merge_regs$boiler_id))

# get rid of boiler-specific regs, to be merged on later
merge_b <- merge_regs %>% filter(!is.na(boiler_id))
merge_p <- merge_regs %>% filter(is.na(boiler_id) & is.na(generator_id)) %>% select(-boiler_id)
merge_g <- merge_regs %>% filter(!is.na(generator_id))

## now, we are going to add in the other data (e.g. capacity
## and inservice information) and then limit by capacity,
## year, etc.

# plant ids are in regs because of the indiv. plants that are
# regulated, but the plt_regs are tied to a reg ID so we can just
# drop those vars to avoid any mess
regs <- regs %>% select(-boiler_id, -plant_code)

# first, let's get all the boiler data together BUT lets start with
# the inservice dataset, since if we don't have an inservice year
# it is kind of useless for our purposes
plt_data <- boilers %>% 
  select(plant_code, boiler_id, plt_state, inservice_y, inservice_m, plt_inservice_y, plt_inservice_m) %>%
  filter(!is.na(inservice_y) | !is.na(plt_inservice_y)) %>% 
  group_by(plant_code) %>%
  ## there were some duplicate plt_inservice_m -- we keep earliest. There are also
  ## some NA plt_inservice_m, which throws an error, but doesn't matter.
  mutate(plt_inservice_m = min(plt_inservice_m, na.rm = TRUE),
         inservice_m = min(inservice_m, na.rm = TRUE)) %>%
  ungroup() %>% unique() %>%
  left_join(states) %>% mutate(join = 1)

## if no non-NA plt_inservice_m, the above makes it -Inf
## we replace with 6 - (middle of the year)
plt_data$plt_inservice_m[is.na(plt_data$plt_inservice_m)] <- 6

retirement <- boilers %>% 
  select(plant_code, boiler_id, calc_retirement_y) %>%
  unique()

unique_id(retirement, plant_code, boiler_id)

## if the calculated retirement year is 2018, we assume persists forward
retirement$calc_retirement_y[retirement$calc_retirement_y == 2018] <- 9999

plt_data <- left_join(plt_data, retirement)

## check for dups
unique_id(plt_data, plant_code, boiler_id)

## there's one plant where the state is NA in one year. We manually fix
plt_data <- plt_data[!(plt_data$plant_code == 8066 & plt_data$boiler_id == "BW73" & is.na(plt_data$plt_state)),]

## check for dups
unique_id(plt_data, plant_code, boiler_id)

## we want to create a universe across all years, so we do that here
years <- data.frame(year = c(1985:2018), join = 1)

## we create skeleton dataset based on ALL plant info and all years
## but we want to filter to only boilers that theoretically existed
## in a given year

## we triple check that, post-cleaning, each boiler has only one inservice year 
## (it holds true - phew!)
boilers %>% select(plant_code, boiler_id, inservice_y) %>% 
  unique() %>% group_by(plant_code, boiler_id) %>% 
  summarise(n = n()) %>% ungroup() %>% group_by(n) %>% summarise(num = n())

plt_data <- full_join(plt_data, years) %>% select(-join) %>%
  filter(inservice_y <= year, calc_retirement_y >= year)

plt_data <- left_join(plt_data, gen_summary) %>%
  left_join(select(boilers, plant_code, boiler_id, year, max_steam_flow, plant_steam_flow)
            %>% unique())

unique_id(plt_data, state, plant_code, boiler_id, year)
nrow(plt_data) ## from 1985 to 2018, we have 43,340 obs

# now we join on the regs, and then we'll have to filter (since
# based purely on geography, plants will match to multiple regs)
plts <- left_join(plt_data, merge_p) %>%
  left_join(regs) %>% filter(!is.na(id))

table(plts$geo_type)
unique_id(plts, plant_code, year, boiler_id, id)

## our boilers in the merge_b daset are doubles but they're string in plt_data. We replace
## the merge_b boilers with strings that match those in plt_data
merge_b$boiler_id[merge_b$boiler_id == "1"] <- "01"
merge_b$boiler_id[merge_b$boiler_id == "2"] <- "02"
merge_b$boiler_id[merge_b$boiler_id == "3"] <- "03"
merge_b$boiler_id[merge_b$boiler_id == "4"] <- "04"
merge_b$boiler_id[merge_b$boiler_id == "5"] <- "05"
merge_b$boiler_id[merge_b$boiler_id == "6"] <- "06"
merge_b$boiler_id[merge_b$boiler_id == "7"] <- "07"
merge_b$boiler_id[merge_b$boiler_id == "8"] <- "08"
merge_b$boiler_id[merge_b$boiler_id == "9"] <- "09"

bois <- left_join(plt_data, merge_b) %>% 
  left_join(regs) %>% filter(!is.na(id))

unique_id(bois, plant_code, year, boiler_id, id)

## now we do the same for our (very few) generator regulations
genregs <- gens %>% select(plant_code, boiler_id, generator_id) %>% unique() %>%
  left_join(merge_g %>% select(-boiler_id)) %>% filter(!is.na(id)) %>% filter(!is.na(boiler_id)) %>%
  select(-generator_id)

geo_regs <- bind_rows(plts, bois) %>% bind_rows(genregs)
table(geo_regs$geo_type)

## we add in the primary_fuel data, because some states base their regs on fuel type
## we note, again, that our sample is all boilers that used coal in a given year over
## our time period. It is possible some boilers used coal for only a short period, or
## primarily used a different fuel.
geo_regs <- left_join(geo_regs, 
                      select(boilers, plant_code,
                             boiler_id, year, primary_fuel1, primary_fuel2, primary_fuel3,
                             primary_fuel4, primary_fuel5, primary_fuel6, primary_fuel7,
                             primary_fuel8))

## we check that this worked
table(boilers$primary_fuel1)

## 93% of boiler/year/reg obs have a primary fuel of coal
nrow(boilers %>% filter(primary_fuel1 %in% c("ANT", "BIT", "LIG", "SUB", "SGC", "RC", "WC", "COL")))/nrow(boilers)

## another 1% have a secondary fuel of coal
nrow(boilers %>% filter(primary_fuel2 %in% c("ANT", "BIT", "LIG", "SUB", "SGC", "RC", "WC", "COL") & 
       !primary_fuel1 %in% c("ANT", "BIT", "LIG", "SUB", "SGC", "RC", "WC", "COL")))/nrow(boilers)

# note that we require reg ID for identification - plants are mapped
# to multiple possible regs
#unique_id(geo_regs, state, utility_code, plant_code, boiler_id, year, id)

## -----------------------------------------------------------------------
## cleaning interlude
## -----------------------------------------------------------------------

## we have to clean up the SO2 limits variables - certain states calculate the variable based
## on the heat capacity of the boiler or plant, or by the stack flue.

geo_regs$std_other[is.na(geo_regs$std_other)] <- "none"

geo_regs <- geo_regs %>%
  mutate(calc_std_so2 = ifelse(std_other == "17Q^(-0.33) lbs of SO2/mmBTU", 17*(plant_capacity^(-0.33)), std_so2_lbs_mmbtu)) %>%
  mutate(calc_std_so2 = ifelse(std_other == "13.8781*(HR)^(-0.4434)", 13.8781*(plant_capacity^(-0.4434)), calc_std_so2)) %>%
  mutate(calc_std_so2 = ifelse(std_other == "1.7Q^(-0.14) lbs SO2/mmBTU", 1.7 * (min_boi_nameplate^(-0.14)), calc_std_so2)) %>%
  mutate(calc_std_so2 = ifelse(std_other == "5.1Q^(-0.14) lbs SO2/mmBTU", 5.1 * (min_boi_nameplate^(-0.14)), calc_std_so2))

## we have to manage Illinois separately on account of needing to add in stack/flue information:
stacks <- read.csv("use_data/stackflues_1985-2018.csv") %>% select(-X)
names(stacks)

## we note the state data in here is not reliable, so we will drop it and fill it
## in from elsewhere
table(stacks$state)
sum(is.na(stacks$state))

stacks <- stacks %>% select(-state)

stacks <- stacks %>% left_join(plants %>% select(plant_code, state) %>% unique())
stacks <- filter(stacks, state == "Illinois")

table(stacks$state)
table(stacks$year)

test <- stacks %>% 
  mutate(stack = ifelse(!is.na(stack_height), 1, 0), 
         flue = ifelse(!is.na(flue_height), 1, 0))

## we see stack picks up where flue leaves off
table(test$stack, test$year)
table(test$flue, test$year)

## histograms of the two vars are also incredibly similar
hist(as.numeric(test$stack_height))
hist(as.numeric(test$flue_height))

stacks <- stacks %>%
  mutate(height = ifelse(year >= 2013, stack_height, flue_height))

test <- stacks %>% 
  mutate(stack = ifelse(!is.na(height), 1, 0))

## we see stack picks up where flue leaves off
table(test$stack, test$year)

## for Illinois, we need:
##      average actual height of stack
##      effective height of stack, which we calculate using the weighted average
##      of the following:
##          -- diameter of stack
##          -- exit temp stack gases (in Rankine or Kelvin)
##          -- exit velocity (feet/sec or meters/sec)
##          -- % total emissions expressed as equivalents emitted from each source
##          -- actual stack height
##          -- heat emission rate in btu/sec or kcal/sec

nrow(unique_id(stacks, plant_code, year, stack_id))

## we see that multiple boilers serve the same stack, which is
## causing duplicate observations. So we start by removing boiler id
stacks <- stacks %>% select(-boiler_id) %>% unique()

## we also notice that from years 2013 onward, we have one ID ("stackflue ID")
## rather than a separate stack and separate flue ID.
test <- stacks %>% mutate(sf_id = ifelse(!is.na(stackflue_id), 1, 0),
                          s_id = ifelse(!is.na(stack_id), 1, 0))

table(test$sf_id, test$year)
table(test$s_id, test$year)
remove(test)

stacks <- stacks %>%
  mutate(id = ifelse(is.na(stack_id), stackflue_id, paste(stack_id, flue_id))) %>%
  select(year, id, plant_code, height, sf_area_at_top, exit_rate_100per, 
         exit_temp_100per, exit_velocity_100per)

stacks$sf_area_at_top <- as.numeric(stacks$sf_area_at_top)
stacks$exit_rate_100per <- as.numeric(stacks$exit_rate_100per)
stacks$exit_temp_100per <- as.numeric(stacks$exit_temp_100per)
stacks$exit_velocity_100per <- as.numeric(stacks$exit_velocity_100per)
stacks$height <- as.numeric(stacks$height)

stacks <- stacks %>% unique()

sum(is.na(stacks$id))

unique_id(stacks, plant_code, year, id)

stacks <- stacks %>%
  group_by(plant_code, year) %>%
  mutate(tot = sum(exit_rate_100per)) %>%
  ungroup() %>%
  mutate(per_exit_rate = exit_rate_100per/tot,
         est_diam = 2*(sqrt(sf_area_at_top/3.14))) %>%
  mutate(wt_height = per_exit_rate * height,
         wt_diam = per_exit_rate * est_diam,
         wt_exit_temp_100per = per_exit_rate * exit_temp_100per,
         wt_exit_velocity_100per = per_exit_rate * exit_velocity_100per)

stacksbyplant <- stacks %>%
  group_by(plant_code, year) %>%
  mutate(twt_diam = sum(wt_diam),
         twt_height = sum(wt_height),
            twt_exit_temp_100per = sum(wt_exit_temp_100per),
            twt_exit_velocity_100per = sum(wt_exit_velocity_100per)) %>%
  ungroup() %>%
  select(plant_code, year, twt_diam, twt_height, twt_exit_temp_100per, twt_exit_velocity_100per) %>%
  unique()

## we have to convert the temp into Rankine, which is the absolute scale for Fahrenheit
## and is equivalent to T_f + 459.67.
stacksbyplant <- stacksbyplant %>%
  mutate(twt_exit_temp_100per = twt_exit_temp_100per + 459.67)

## now we can calculate Q_h, as defined by Illinois. Qh = 7.54 * D^2 * v (T-515)/%

stacksbyplant <- stacksbyplant %>%
  mutate(qh = 7.54 * ((twt_diam^2)/twt_exit_velocity_100per) * (twt_exit_temp_100per-515)/twt_exit_temp_100per)

## the "height effective" calculation used by Illinois varies for Qh > and < 6000 btu/sec
stacksbyplant <- stacksbyplant %>%
  mutate(qh_6000 = ifelse(qh >= 6000, 1, 0)) %>%
  mutate(delta_height = ifelse(qh_6000 == 1, 2.58 * (qh^0.6)/(twt_height^0.11), 0.718*(qh^0.75)/(twt_height & 0.11))) %>%
  mutate(height_eff = twt_height + delta_height) %>%
  mutate(calc_plant_std_lbs_so2_hr = (1/128) * (twt_height^0.11) * (height_eff^2))

ill_merge <- stacksbyplant %>%
  select(plant_code, year, calc_plant_std_lbs_so2_hr) %>% unique()

nrow(geo_regs)
geo_regs <- left_join(geo_regs, ill_merge) %>%
  mutate(calc_plant_std_lbs_so2_hr = ifelse(std_other == "(avg stack height)^(0.11) * (effective height effluent release)^2", calc_plant_std_lbs_so2_hr, std_lbs_so2_hr))
nrow(geo_regs)

## we want to keep if:
##      cutoff is "all" OR 
##      pre/post = "pre" and year_cutoff > plt_inservice_y
##      pre/post = "post" and year_cutoff < plt_inservice_y
##   cutoff being non-numeric could make a mess. I make
##   a separate column

geo_regs$pre_post[geo_regs$pre_post == "11/01/1967"] <- "1967"
geo_regs$pre_post[geo_regs$pre_post == "12/31/1976"] <- "1976"
#geo_regs$pre_post[geo_regs$pre_post == "4/1/1972"] <- "1972"

geotime_regs <- geo_regs %>%
  filter(year >= year_std_start, year <= year_std_end)

specific <- geotime_regs %>% filter(geo_type == "targeted reg") %>%
  mutate(linked_state_reg = "YES") %>%
  select(-geo_area, -aqcr, -town, -manual, -min_cap_any_unit, -max_cap_any_unit,
         -min_cap, -max_cap, -cap_level, -pre_post, -year_cutoff,
         -time_period, -all_ages, -primary_fuel1, -primary_fuel2, -primary_fuel3, -primary_fuel4, 
         -primary_fuel5, -primary_fuel6, -primary_fuel7, -primary_fuel8)

specific <- specific %>%
  group_by(year, plant_code, boiler_id) %>%
  mutate(n = n()) %>% ungroup() %>%
  mutate(co_drop = ifelse(n == 2 & ((plant_code == 465 & boiler_id == "04") | 
                                      (plant_code == 469 & boiler_id %in% c("01", "04"))) & 
                            is.na(other_limits), 1, 0)) %>%
  filter(co_drop != 1)

geotime_regs <- geotime_regs %>%
  filter(geo_type != "targeted reg") %>%
  mutate(after_start_range = ifelse(inservice_y > as.numeric(pre_post), 1, 0)) %>%
  mutate(before_cutoff = ifelse(inservice_y < year_cutoff, 1, 
                                ifelse((inservice_y == year_cutoff) & (inservice_m <= month_cutoff), 1, 0))) %>%
  mutate(in_range = ifelse(after_start_range == 1 & before_cutoff == 1, 1, 0))

## New Mexico has one set of regulations that considers not only the age of the boiler, but also the age of the rest
## of the plant. (If a boiler is part of a plant containing pieces from between 1976 and 1982 as well as from before 1976,
## it is subject to a 0.55 lbs/mmbtu standard).
nm_sub <- geotime_regs %>%
  filter(state == "New Mexico") %>%
  mutate(between1976_1982 = ifelse(inservice_y <= 1982 & inservice_y >= 1976, 1, 0)) %>%
  group_by(plant_code, year) %>%
  mutate(between1976_1982 = max(between1976_1982)) %>%
  ungroup() %>%
  mutate(newandexistingreg = ifelse(plt_inservice_y <= 1972 & between1976_1982 == 1 & 
                                   other_limits == "if plant includes equipment from before 1972 and between 1976 to 1982", 1, 0))

## there are no plants in our sample to which this New Mexico reg applies
table(nm_sub$newandexistingreg)

## we drop this reg from our dataset
nm_sub <- nm_sub %>%
  filter(other_limits != "if plant includes equipment from before 1972 and between 1976 to 1982") %>%
  select(-newandexistingreg, -between1976_1982)

geotime_regs <- geotime_regs %>%
  filter(state != "New Mexico") %>%
  bind_rows(nm_sub) %>%
  filter((before_cutoff == 1 & pre_post == "pre") |
           (before_cutoff == 0 & pre_post == "post") |
           (all_ages == 1) | (in_range == 1))

# new_mexico_middle <- geotime_regs %>%
#   filter(pre_post == "12/31/1976" & year_cutoff == 1982) %>%
#   mutate(in_time = ifelse(plt_inservice_y >= 1977 & plt_inservice_y <= 1982, 1, 0)) %>%
#   filter(in_time == 1) %>% select(-in_time)
# 
# wisconsin_mid <- geotime_regs %>%
#   filter(pre_post == "4/1/1972" & year_cutoff == 1985) %>%
#   mutate(in_time = ifelse(plt_inservice_y >= 1972 & plt_inservice_y < 1985)) %>%
#   filter(in_time == 1) %>% select(-in_time)
# 
# geotime_regs <- bind_rows(geotime_regs, new_mexico_middle) %>% bind_rows(wisconsin_mid)

## now we filter by capacity
geotimecap <- geotime_regs %>%
  mutate(plant = ifelse(cap_level == "Plant", 1, 0),
         boiler = ifelse(cap_level == "Boiler", 1, 0)) %>%
  mutate(boil_max = ifelse(boiler == 1 & min_cap <= min_boi_nameplate & max_cap >= min_boi_nameplate, 1, 0),
         boil_min = ifelse(boiler == 1 & min_cap <= max_boi_nameplate & max_cap >= max_boi_nameplate, 1, 0)) %>%
  mutate(plantcap250 = ifelse(plant_capacity >= 250, 1, 0))

# there are only 130 edge cases out of 7802 obs. Sylwia - do you prefer we use
# the max or min?
sum(geotimecap$boil_max == 1, na.rm = TRUE)
sum(geotimecap$boil_min == 1, na.rm = TRUE)
sum(geotimecap$boil_max == 1 & geotimecap$boil_min == 1, na.rm = TRUE)

geotimecap <- geotimecap %>%
  filter(boil_min == 1 |
           (plant == 1 & plant_capacity >= min_cap & plant_capacity <= max_cap) |
           (min_cap == 0 & max_cap == 9999999))

## Minnesota considers both plant heat and boiler heat, depending, so we separate it out
minnesota <- geotimecap %>%
  filter((other_limits != "total heat input location >250mmBTU" &
           boil_min == 1 & plant_capacity > 250) |
           (other_limits == "total heat input at location <= 250mmBTU" &
            boil_min == 1 &  plant_capacity <= 250))

# michigan (alone) uses steam instead of mmbtu, so we treat it separately
michigan <- geotime_regs %>% 
  filter(min_cap_any_unit == "500k lbs steam" & plant_steam_flow > 500 |
         max_cap_any_unit == "500k lbs steam" & plant_steam_flow <= 500)

## nj has requirements based on fuel:
nj <- geotime_regs %>%
  filter(primary_fuel1 == "ANT" & other_limits == "anthracite coal" |
           primary_fuel1 == "BIT" & other_limits == "bituminous coal" |
           primary_fuel1 != "ANT" & other_limits == "not anthracite")

geotimecap <- geotimecap %>%
  filter(!other_limits %in% c("total heat input location >250mmBTU", 
                              "total heat input at location <= 250mmBTU")) %>%
  bind_rows(minnesota) %>%
  filter(!min_cap_any_unit == "500k lbs steam", !max_cap_any_unit == "500k lbs steam") %>%
  bind_rows(michigan) %>%
  filter(!other_limits %in% c("anthracite coal", "bituminous coal", "not anthracite")) %>%
  bind_rows(nj)

## we sometimes have multiple regs assigned because we have failed to consider
## other features. For example, in New Mexico, plants are regulated more stringently
## if they contain pieces from both 1972 to 1983 and post 1983 (as opposed to only
## pieces from before 1983). There is also a regulation in Florida affecting Hillsborough
## County for which we were unable to isolate the year end date, however, the two plants in our
## dataset in Hillsborough County are separately regulated, so we can filter out that regulation.
geotimecap <- geotimecap %>% 
  mutate(drop_hillsborough = ifelse(plt_county == "Hillsborough" & state == "Florida" & std_so2_lbs_mmbtu == 1.5, 1, 0)) %>%
  filter(drop_hillsborough != 1) %>%
  select(-drop_hillsborough)

## in New Jersey, boilers over 200mmBTU and plants over 400 mmBTU are subject to a 1.5 lbs/mmBTU standard,
## but there are, of course, boilers over 200mmBTU that serve plants over 400mmBTU. We filter out the
## extra regulation match, as it doesn't matter.
geotimecap <- geotimecap %>%
  mutate(drop_nj = ifelse(state == "New Jersey" & min_cap == 200 & plant_capacity >= 400, 1, 0)) %>%
  filter(drop_nj != 1) %>%
  select(-drop_nj)

## Massachusetts has a separate standard for PLANTS that emitted over 500 tons each of SO2 and NOx
## in either 1997, 1998, or 1999 and also contains a boiler that:
##        -- was built before 1977, 
##        -- is over 100MW capacity
##        -- and is regulated by the Federal Acid Rain Program.
## 1606 has a boiler built before 1977 that is above 100MW (341 MMBTU) (Boiler 01)
## 1613 also has a boiler built before 1977 that is equal to 100MW (341 MMBTU) (Boiler 08)
## 1619 has three boilers, built befoer 1977, that are big enough (01, 02, and 03)
## 1626 has a boiler that is built before 1977 and large enough (Boiler 03)
## All four plants are listed as Phase II plants at 40 CFR 73.10(b)
## We still need, however, to know who emitted at least 500 tons of SO2 and NOx.
## In the 1997 EPA Acid Rain Program compliance guide, EPA lists tpy for SO2 and NOx
## for all four facilities as greater than 500. Therefore, each of these four facilities
## (and all boilers at the facility) are regulated undert his reg.

## we see these Massachusetts plants are the only ones that are not yet assigned exclusively to one regulation.
## except for a problem in North Carolina, post 2009 - this is because there is a utility-level cap on
## emissions. I think we have to drop these observations after we have included the
## plants/boilers not being regulated by the state.
#View(unique_id(geotimecap, plant_code, year, boiler_id))
test <- unique_id(geotimecap, plant_code, boiler_id, year)
nrow(test)
table(test$state)
remove(test)

geotimecap %>% filter(state == "Massachusetts", 
                      other_limits == "emits over 500 tons SO2 and 500 tons NOx in 1997,1998, or 1999, regulated by Acid Rain Program, contains a boiler that is over 100MW and built before 1977") %>% 
  select(plant_code) %>% unique()

geotimecap <- geotimecap %>%
  group_by(plant_code, boiler_id, year) %>%
  mutate(n = n()) %>% ungroup() %>%
  mutate(mass_drop = ifelse(state == "Massachusetts" & n == 2 & is.na(other_limits), 1, 0))

table(geotimecap$mass_drop)

geotimecap <- geotimecap %>%
  filter(mass_drop != 1) %>%
  select(-mass_drop, -n)

test <- unique_id(geotimecap, plant_code, boiler_id, year)
nrow(test)
table(test$state)
remove(test)

geotimecap <- geotimecap %>% 
  mutate(plant_year = paste(plant_code, boiler_id, year),
         linked_state_reg = "YES")

unreg <- plt_data %>% mutate(plant_year = paste(plant_code, boiler_id, year)) %>%
  filter(!plant_year %in% geotimecap$plant_year) %>%
  mutate(linked_state_reg = "NO")

## we bind these two sets together and then filter out certain plants in North Carolina post 2009 
## because the regulation is a utility-wide
## cap, and there is not a way to show that in our dataset
geotimecap <- bind_rows(geotimecap, unreg) %>%
  filter(!(state == "North Carolina" & year >= 2009 & pre_post == "all"))

test <- unique_id(geotimecap, plant_code, boiler_id, year)
nrow(test)
#table(test$state) ## throws an error because there are no values
remove(test)

## 495 unique plants
length(unique(geotimecap$plant_code))
## 1446 unique boilers
length(unique(paste(geotimecap$plant_code, geotimecap$boiler_id)))

geotimecap <- geotimecap %>%
  select(-geo_area, -aqcr, -town, -manual, -min_cap_any_unit,
         -min_cap, -max_cap, -cap_level, -pre_post, -year_cutoff,
         -time_period, -all_ages, -before_cutoff, -plant, -boiler, -boil_max,
         -boil_min, -plant_year, -primary_fuel1, -primary_fuel2, -primary_fuel3, -primary_fuel4, 
         -primary_fuel5, -primary_fuel6, -primary_fuel7, -primary_fuel8) %>%
  rename(reg_id = id)

nrow(unique_id(geotimecap, plant_code, boiler_id, year, reg_id))
nrow(unique_id(geotimecap, plant_code, boiler_id, year))

geotimecap_org <- geotimecap %>% bind_rows(specific)

## if a boiler is linked to a targeted reg and a general reg, we keep the targeted reg
geotimecap_org <- geotimecap_org %>%
  group_by(plant_code, boiler_id, year) %>%
  mutate(n = n()) %>% ungroup() %>%
  mutate(specific_drop = ifelse(n == 2 & (geo_type != "targeted reg" | is.na(geo_type)), 1, 0))

sum(is.na(geotimecap_org$specific_drop))
table(geotimecap_org$specific_drop)
table(geotimecap_org$n)

geotimecap_org <- geotimecap_org %>%
  filter(specific_drop != 1)

test <- unique_id(geotimecap_org, plant_code, boiler_id, year)
nrow(test)
#table(test$state)
remove(test)


# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # -- Regulatory Benefit of Grandfathering ----------
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## we want to keep if:
##      cutoff is "all" OR 
##      pre/post = "pre" and year_cutoff > plt_inservice_y
##      pre/post = "post" and year_cutoff < plt_inservice_y
##   cutoff being non-numeric could make a mess. I make
##   a separate column
## BUT this time, we say that plt_inservice_y = data year
## here, we are asking "what would the regulation be if this plant
## were a NEW plant this year, instead of an existing plant?"

geotime_regs <- geo_regs %>%
  filter(year >= year_std_start, year < year_std_end) %>%
  mutate(inservice_y = year, plt_inservice_y = year) %>%
  mutate(after_start_range = ifelse(inservice_y > as.numeric(pre_post), 1, 0)) %>%
  mutate(before_cutoff = ifelse(inservice_y < year_cutoff, 1, 
                                ifelse((inservice_y == year_cutoff) & (inservice_m <= month_cutoff), 1, 0))) %>%
  mutate(in_range = ifelse(after_start_range == 1 & before_cutoff == 1, 1, 0))

## New Mexico has one set of regulations that considers not only the age of the boiler, but also the age of the rest
## of the plant. (If a boiler is part of a plant containing pieces from between 1976 and 1982 as well as from before 1976,
## it is subject to a 0.55 lbs/mmbtu standard).
nm_sub <- geotime_regs %>%
  filter(state == "New Mexico") %>%
  mutate(between1976_1982 = ifelse(inservice_y <= 1982 & inservice_y >= 1976, 1, 0)) %>%
  group_by(plant_code, year) %>%
  mutate(between1976_1982 = max(between1976_1982)) %>%
  ungroup() %>%
  mutate(newandexistingreg = ifelse(plt_inservice_y <= 1972 & between1976_1982 == 1 & 
                                      other_limits == "if plant includes equipment from before 1972 and between 1976 to 1982", 1, 0))

## there are no plants in our sample to which this New Mexico reg applies
table(nm_sub$newandexistingreg)

## we drop this reg from our dataset
nm_sub <- nm_sub %>%
  filter(other_limits != "if plant includes equipment from before 1972 and between 1976 to 1982") %>%
  select(-newandexistingreg, -between1976_1982)

geotime_regs <- geotime_regs %>%
  filter(state != "New Mexico") %>%
  bind_rows(nm_sub) %>%
  filter((before_cutoff == 1 & pre_post == "pre") |
           (before_cutoff == 0 & pre_post == "post") |
           (all_ages == 1) | (in_range == 1))

geotimecap <- geotime_regs %>%
  mutate(plant = ifelse(cap_level == "Plant", 1, 0),
         boiler = ifelse(cap_level == "Boiler", 1, 0)) %>%
  mutate(boil_max = ifelse(boiler == 1 & min_cap <= min_boi_nameplate & max_cap >= min_boi_nameplate, 1, 0),
         boil_min = ifelse(boiler == 1 & min_cap <= max_boi_nameplate & max_cap >= max_boi_nameplate, 1, 0)) %>%
  mutate(plantcap250 = ifelse(plant_capacity >= 250, 1, 0))

# there are only 92 edge cases out of 5998 obs. Sylwia - do you prefer we use
# the max or min?
sum(geotimecap$boil_max == 1, na.rm = TRUE)
sum(geotimecap$boil_min == 1, na.rm = TRUE)
sum(geotimecap$boil_max == 1 & geotimecap$boil_min == 1, na.rm = TRUE)

geotimecap <- geotimecap %>%
  filter(boil_min == 1 |
           (plant == 1 & plant_capacity >= min_cap & plant_capacity <= max_cap) |
           (min_cap == 0 & max_cap == 9999999))

## Minnesota considers both plant heat and boiler heat, depending, so we separate it out
minnesota <- geotimecap %>%
  filter((other_limits != "total heat input location >250mmBTU" &
            boil_min == 1 & plant_capacity > 250) |
           (other_limits == "total heat input at location <= 250mmBTU" &
              boil_min == 1 &  plant_capacity <= 250))

# michigan (alone) uses steam instead of mmbtu, so we treat it separately
michigan <- geotime_regs %>% 
  filter(min_cap_any_unit == "500k lbs steam" & plant_steam_flow > 500 |
           max_cap_any_unit == "500k lbs steam" & plant_steam_flow <= 500)

## nj has requirements based on fuel:
nj <- geotime_regs %>%
  filter(primary_fuel1 == "ANT" & other_limits == "anthracite coal" |
           primary_fuel1 == "BIT" & other_limits == "bituminous coal" |
           primary_fuel1 != "ANT" & other_limits == "not anthracite")

geotimecap <- geotimecap %>%
  filter(!other_limits %in% c("total heat input location >250mmBTU", 
                              "total heat input at location <= 250mmBTU")) %>%
  bind_rows(minnesota) %>%
  filter(!min_cap_any_unit == "500k lbs steam", !max_cap_any_unit == "500k lbs steam") %>%
  bind_rows(michigan) %>%
  filter(!other_limits %in% c("anthracite coal", "bituminous coal", "not anthracite")) %>%
  bind_rows(nj)

## we sometimes have multiple regs assigned because we have failed to consider
## other features. For example, in Florida, in Hillsborough
## County, we were unable to isolate the year end date for a particular regulation, however, the two plants in our
## dataset in Hillsborough County are regulated by name, so we can filter out that other regulation.
geotimecap <- geotimecap %>% 
  mutate(drop_hillsborough = ifelse(plt_county == "Hillsborough" & state == "Florida" & std_so2_lbs_mmbtu == 1.5, 1, 0)) %>%
  filter(drop_hillsborough != 1) %>%
  select(-drop_hillsborough)

## in New Jersey, boilers over 200mmBTU and plants over 400 mmBTU are subject to a 1.5 lbs/mmBTU standard,
## but there are, of course, boilers over 200mmBTU that serve plants over 400mmBTU. We filter out the
## extra regulation match for boilers over 200mmbtu serving plants over 400mmbtu.
geotimecap <- geotimecap %>%
  mutate(drop_nj = ifelse(state == "New Jersey" & min_cap == 200 & plant_capacity >= 400, 1, 0)) %>%
  filter(drop_nj != 1) %>%
  select(-drop_nj)

## Massachusetts has a separate standard for plants that emitted over emits over 500 tons SO2 in 1997, 1998, or 1999
## built before 1977, over 100MW capacity, and regulated by the Federal Acid Rain Program. This regulation
## no longer poses a problem for us when considering new plants, because the regulation requires the source
## to have been constructed before 1977, which will never be the case for our "dummy" new plants/boilers.

## for plants in New Mexico built between 1976 and 1982, there is a separate standard
## if any of the plant is from before 1972. We had to look at this manually before,
## but for a hypothetical new plant/boiler, in 1982 (for example) this regulation is not
## in place, and in 1983 and later, we would be assuming nothing pre-existed.

## we see there is an issue with the linkage in certain North Carolina plants. This is because North Carolina has
## created a utility-wide cap after 2010. We will drop those observations after we have linked up to the
## unregulated plants, having no good way to identify the actual regulation on a given plant/boiler.
#View(unique_id(geotimecap, plant_code, year, boiler_id))
test <- unique_id(geotimecap, plant_code, boiler_id, year)
nrow(test)
table(test$state)
remove(test)

geotimecap <- geotimecap %>% 
  mutate(plant_year = paste(plant_code, boiler_id, year),
         ifnew_linked_state_reg = "YES")

unreg <- plt_data %>% mutate(plant_year = paste(plant_code, boiler_id, year)) %>%
  filter(!plant_year %in% geotimecap$plant_year) %>%
  mutate(ifnew_linked_state_reg = "NO")

geotimecap <- bind_rows(geotimecap, unreg)

## 495 unique plants
length(unique(geotimecap$plant_code))
## 1446 unique boilers
length(unique(paste(geotimecap$plant_code, geotimecap$boiler_id)))

## we drop certain plants in North Carolina post 2009 because the regulation is a utility-wide
## cap, and there is not a way to show that in our dataset
geotimecap <- geotimecap %>%
  filter(!(state == "North Carolina" & year >= 2009 & pre_post == "all"))

## now we can see that we have no issues
test <- unique_id(geotimecap, plant_code, boiler_id, year)
nrow(test)
#table(test$state) ## throws an error because there are no values in "test"
remove(test)

geotimecap <- geotimecap %>%
  select(-geo_area, -aqcr, -town, -manual, -min_cap_any_unit,
         -min_cap, -max_cap, -cap_level, -pre_post, -year_cutoff,
         -time_period, -all_ages, -before_cutoff, -plant, -boiler, -boil_max,
         -boil_min, -plant_year, -primary_fuel1, -primary_fuel2, -primary_fuel3, -primary_fuel4, 
         -primary_fuel5, -primary_fuel6, -primary_fuel7, -primary_fuel8) %>%
  rename(reg_id = id)

nrow(unique_id(geotimecap, plant_code, boiler_id, year, reg_id))
nrow(unique_id(geotimecap, plant_code, boiler_id, year))

## we add our targeted regulations back in
new_stds <- geotimecap %>% bind_rows(specific)

## if a boiler is linked to a targeted reg and a general reg, we keep the targeted reg
new_stds <- new_stds %>%
  group_by(plant_code, boiler_id, year) %>%
  mutate(n = n()) %>% ungroup() %>%
  mutate(specific_drop = ifelse(n == 2 & (geo_type != "targeted reg" | is.na(geo_type)), 1, 0))

sum(is.na(new_stds$specific_drop))
table(new_stds$specific_drop)
table(new_stds$n)

new_stds <- new_stds %>%
  filter(specific_drop != 1) %>% select(-n, -specific_drop)

test <- unique_id(new_stds, plant_code, boiler_id, year)
nrow(test)
#table(test$state)
remove(test)

## 495 unique plants
length(unique(new_stds$plant_code))
## 1446 unique boilers
length(unique(paste(new_stds$plant_code, new_stds$boiler_id)))

new_stds <- new_stds %>%
  rename(ifnew_calc_std_so2 = calc_std_so2,
         ifnew_calc_plt_lbs_so2_hr = calc_plant_std_lbs_so2_hr,
        ifnew_std_so2_lbs_mmbtu = std_so2_lbs_mmbtu,
         ifnew_std_fuel_content = std_fuel_content,
         ifnew_std_so2_ppm = std_so2_ppm,
         ifnew_std_lbs_so2_hr = std_lbs_so2_hr,
         ifnew_std_other = std_other) %>%
  select(plant_code, boiler_id, year, ifnew_std_so2_lbs_mmbtu, ifnew_calc_std_so2,
         ifnew_std_fuel_content, ifnew_std_so2_ppm, ifnew_std_lbs_so2_hr, ifnew_std_other,  ifnew_calc_plt_lbs_so2_hr,
         ifnew_linked_state_reg) %>%
  unique()

unique_id(new_stds, plant_code, boiler_id, year)
unique_id(geotimecap_org, plant_code, boiler_id, year)

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # -- Merge and save file! ---------------------------
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

geotimecap <- left_join(geotimecap_org, new_stds) %>%
  select(-year_std_end, -year_std_start, -month_cutoff, -indiana_county, -after_start_range,
         -in_range, -id, -n, -co_drop, -specific_drop, -max_cap_any_unit, -scrubber_est)

write.csv(geotimecap, "use_data/merged_regs_and_plants.csv")

# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # -- Modifications ---------------------------------
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# boilers <- read.csv("use_data/boilers_1985-2018.csv", stringsAsFactors = FALSE)
# names(boilers)
# 
# table(boilers$nsr_d)
# 
# # we only have access to the NSR value post 2009
# table(boilers$year, boilers$nsr_d)
# 
# hist(as.numeric(boilers$nsr_permit_y))
# 
# test <- boilers %>% mutate(nsr = ifelse(nsr_d == "Y", 1, 0)) %>%
#   group_by(utility_code, plant_code) %>%
#   mutate(ever_nsr = max(nsr)) %>% ungroup() %>%
#        select(utility_code, plant_code, boiler_id, year, 
#               type_of_boiler, ever_nsr, nsr_d, nsr_permit_y, inservice_y, 
#               retirement_y) %>% 
#        arrange(utility_code, plant_code, boiler_id, year)
# 
# View(test %>% filter(ever_nsr == 1))
# 
# nsr <- boilers %>% select(year, utility_code, plant_code, boiler_id,
#                           type_of_boiler, nsr_d, nsr_permit_y, nsr_permit_num)
# View(nsr)
# 