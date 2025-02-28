## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 08_feature_generation
## Bridget Pals
## 01 February 2019
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This program merges all data, generates additional useful features, eliminates unnecessary
## variables, and conducts additional data cleaning.
  

### START CODE ###


# IMPORT ------------------------------------------------------------------------------------------

## General --------------------------------------------------------------------

plant_regs <- read.csv(here::here("data/merged_regs_and_plants.csv"), stringsAsFactors = FALSE) %>% 
  select(-X)

plants <- read.csv(here::here("data/plants_1985_2018.csv"), stringsAsFactors = FALSE) %>% select(-X)
boilers <- read.csv(here::here("data/boilers_1985_2018.csv"), stringsAsFactors = FALSE) %>% select(-X)
sfs <- read.csv(here::here("data/stackflues_1985_2018.csv"), stringsAsFactors = FALSE) %>% select(-X)
gens <- read.csv(here::here("data/generators_1985_2018.csv"), stringsAsFactors = FALSE) %>% select(-X)
fgds <- read.csv(here::here("data/fgds_1985_2018.csv"), stringsAsFactors = FALSE) %>% 
  select(-X)
fgds$utility_code <- as.numeric(fgds$utility_code)

fuel_comp <- read.csv(here::here("data/boilers_fuel_comp_1985_2018.csv"), stringsAsFactors = FALSE) %>% 
  select(-X)

all_fuel <- read.csv(here::here("data/all_fuel_1970_2018.csv"), stringsAsFactors = FALSE) %>%
  select(-X)

## Check average capacity of utilities
utility_cap <- gens %>% select(utility_code, plant_code, generator_id, nameplate_mmbtu) %>%
  unique() 

nrow(unique_id(utility_cap, plant_code, generator_id))

utility_cap <- gens %>% filter(!is.na(nameplate_mmbtu)) %>%
  group_by(utility_code, plant_code, generator_id) %>%
  mutate(nameplate = mean(nameplate_mmbtu)) %>%
  ungroup() %>% group_by(utility_code) %>%
  summarise(nameplate = sum(nameplate))

hist(utility_cap$nameplate[utility_cap$nameplate < 20000])
hist(utility_cap$nameplate[utility_cap$nameplate < 10000])
hist(utility_cap$nameplate[utility_cap$nameplate < 5000])

utility_cap <- gens %>% select(utility_code, plant_code, generator_id, year, nameplate_mmbtu) %>%
  filter(!is.na(nameplate_mmbtu), year <2000) %>% unique() %>% 
  group_by(utility_code, plant_code, generator_id) %>%
  mutate(nameplate = mean(nameplate_mmbtu)) %>%
  ungroup() %>% group_by(utility_code) %>%
  summarise(nameplate = sum(nameplate))

hist(utility_cap$nameplate[utility_cap$nameplate < 20000])
hist(utility_cap$nameplate[utility_cap$nameplate < 10000])
hist(utility_cap$nameplate[utility_cap$nameplate < 5000])


## Utility/non-utility --------------------------------------------

## Between 1990 and 2000, EIA-860 separately listed "utility" and "non-utility" 
## plants. We read in the utility files to get a full list of all utility codes 
## over that timeframe.  We assume utility codes do not change over time and that
## a code associated with a utility in 1990 will continue to be associated with 
## a utility in later periods.

utils <- data.frame()

for (i in 1990:2000) {
  
  zip_file <- here::here("data/eia/f860", paste0("f860_a", i, ".zip"))
  l.zip <- zip::zip_list(zip_file) %>%
    dplyr::pull(filename) %>%
    as.list()
  zip::unzip(zip_file, exdir=fs::path_dir(zip_file))
  
  if (i %in% c(1990:1991)) {
    util_sub <- read_excel(here::here("data/eia/f860", paste0("UTILY", substr(i,3,4), ".xls")), 
                           sheet=1, col_types="text") %>%
      select(utility_code = 'Utility Code', utility_name = 'Utility Name', state = State)
  } else if (i %in% c(1992:1994)) {
    util_sub <- read_excel(here::here("data/eia/f860", paste0("UTIL", substr(i,3,4), ".xls")), 
                           sheet=1, col_types="text") %>%
      select(utility_code = UTILCODE, utility_name = UTILNAME, state = STATE)
  } else if (i %in% c(1995:1996)) {
    util_sub <- read_excel(here::here("data/eia/f860", paste0("UTILY", substr(i,3,4), ".xls")), 
                           sheet=1, col_types="text") %>%
      select(utility_code = UTILCODE, utility_name = UTILNAME, state = STATE)
  } else if (i == 1997) {
    util_sub <- read_excel(here::here("data/eia/f860/UTILITY.xls"), 
                           sheet=1, col_types="text") %>%
      select(utility_code = UTILCODE, utility_name = UTILNAME, state = STATE)
  } else if (i %in% c(1998:2000)) {
    util_sub <- read_excel(here::here("data/eia/f860", paste0("Utility", i, ".xls")), 
                           sheet=1, col_types="text") %>%
      select(utility_code = EIA_UTILITY_CODE, utility_name = UTILITY_NAME, state = MAIL_STATE)
  }

  l.zip %>%
    purrr::map_chr(\(x) fs::path(fs::path_dir(zip_file), x)) %>%
    fs::file_delete()
  
  util_sub <- mutate(util_sub, year = i)
  utils <- bind_rows(util_sub, utils)
  print(i)
}

## Utility code uniquely identifies
unique_id(utils, year, utility_code)

## We note way more obs in 1990-1996 than in later years - it is possible in those 
## years some non-utility utility codes slipped in. We read in the non-utility
## datasets to check this.
table(utils$year)

nonutil <- data.frame()

for (i in c(1998:2000)) {
  
  zip_file <- here::here("data/eia/f860", paste0("f860_b", i, ".zip"))
  l.zip <- zip::zip_list(zip_file) %>%
    dplyr::pull(filename) %>%
    as.list()
  zip::unzip(zip_file, exdir=fs::path_dir(zip_file))
  
  nonutil_sub <- read_excel(here::here("data/eia/f860/nuppfac.xls"), 
                            sheet=1, col_types="text")
 
  l.zip %>%
    purrr::map_chr(\(x) fs::path(fs::path_dir(zip_file), x)) %>%
    fs::file_delete()
  
  print(nrow(nonutil_sub))
  nonutil_sub <- mutate(nonutil_sub, year = i) %>%
    select(utility_code = EIAUTILCD, year) %>% unique()
  print(nrow(nonutil_sub))
  
  nonutil <- bind_rows(nonutil_sub, nonutil)
  print(i)
}

table(utils$year)
table(nonutil$year)

util_names <- utils %>%
  filter(year %in% c(1996, 1997)) %>%
  group_by(utility_code) %>%
  mutate(both_years = ifelse(n()-1 == 0, "N", "Y")) %>%
  ungroup() %>%
  arrange(utility_code, year)

write.csv(util_names, here::here("data/utilities_in_1996-97.csv"))

rm(util_names)

utils97_00 <- utils %>% 
  filter(year %in% c(1997:2000)) %>%
  select(-utility_name, -year, -state) %>%
  mutate(utility_flag97_00 = 1) %>% unique() %>%
  mutate(utility_code = as.numeric(utility_code))

utils_all <- utils %>% mutate(utility_flag = 1) %>% 
  select(-utility_name, -year, -state) %>%
  unique() %>% mutate(utility_code = as.numeric(utility_code))
  
util_join <- left_join(utils_all, utils97_00)

unique_id(util_join, utility_code)

## We add this flag onto the plant dataset
plants <- left_join(plants, util_join)

rm(util_join)


# CLEAN -------------------------------------------------------------------------------------------

## Investigate utilities ------------------------------------------------------

util_plants <- plants %>%
  filter(utility_flag == 1) %>%
  group_by(utility_code, year) %>%
  summarise(num_plants = n()) %>%
  ungroup()

#View(util_plants)

pdf(here::here("out", date, "hist_plants_by_utility_by_year.pdf"))

for (i in c(1985:2005, 2007:2018)) {
  
  util_sub <- util_plants %>% filter(year == i)
  
  hist(util_sub$num_plants, xlab = "# of Utilities", ylab = "# of Plants",
       main = paste("Number of Plants, by Utility,", i))
}

dev.off()

util_plants <- plants %>%
  filter(utility_flag97_00 == 1) %>%
  group_by(utility_code, year) %>%
  summarise(num_plants = n()) %>%
  ungroup()

pdf(here::here("out", date, "hist_plants_by_utility_by_year_97_00.pdf"))

for (i in c(1985:2005, 2007:2018)) {
  
  util_sub <- util_plants %>% filter(year == i)
  
  hist(util_sub$num_plants, xlab = "# of Utilities", ylab = "# of Plants",
       main = paste("Number of Plants, by Utility,", i))
  
}

dev.off()


## Identify plant vars --------------------------------------------------------
## We know that plant_code, and year uniquely identify the rows, even if there is 
## information that could be missing in some years and present in others.
unique_id(plants, plant_code, year)


## Additional plant-level vars ------------------------------------------------
## For now, we keep all plant vars
names(plants)


## Select boiler vars ---------------------------------------------------------

boilers %>% unique_id(plant_code, boiler_id, year)

## We examine the "hours under load" var and find that it is only present in 
## certain years
sum(boilers$hours_under_load == 0, na.rm = TRUE)/nrow(boilers)
sum(is.na(boilers$hours_under_load))/nrow(boilers)
table(!is.na(boilers$hours_under_load), boilers$year)

## We make an so2_lbs_mmbtu var (the REPORTED binding compliance limit on a boiler 
## as submitted to EIA).  We get this variable into consistent units (some states 
## report pounds of sulfur, others pounds of so2).
boilers <- boilers %>% 
  mutate(reported_so2_lbs_mmbtu = ifelse(so2_unit == "DP", as.numeric(so2_std_rate),
                                ifelse(so2_unit == "SB", 2*as.numeric(so2_std_rate), NA)))

## We make a variable for boiler manufacturer, which is missing in some years for 
## some boilers and present in others, so we are going to fill in whatever we can.
manus <- boilers %>% select(plant_code, boiler_id, boiler_manufacturer) %>%
  filter(!is.na(boiler_manufacturer)) %>%
  group_by(plant_code, boiler_id, boiler_manufacturer) %>%
  summarise(num_obs = n()) %>%
  ungroup()

#View(manus %>% filter(!is.na(boiler_manufacturer)) %>% unique_id(plant_code, boiler_id))

manus <- manus %>%
  mutate(other = ifelse(boiler_manufacturer == "OT", 1, 0)) %>%
  group_by(plant_code, boiler_id) %>%
  mutate(types = n()) %>%
  ungroup() 

table(manus$types)

manus <- manus %>%
  filter(!(types >= 2 & other == 1)) %>%
  group_by(plant_code, boiler_id) %>%
  arrange(desc(num_obs)) %>%
  mutate(most = 1:n()) %>%
  ungroup() 

table(manus$most)

manus <- manus %>%
  filter(most == 1) %>%
  mutate(boiler_manu_clean = boiler_manufacturer) %>%
  select(plant_code, boiler_id, boiler_manu_clean)

## By design, we no longer have any duplicates. Now we must join this back on to the
## boiler dataset.
nrow(unique_id(manus, plant_code, boiler_id))/nrow(manus)

unique_id(boilers, plant_code, boiler_id, year)

boilers <- boilers %>% left_join(manus)

sum(is.na(boilers$boiler_manufacturer))
sum(!is.na(boilers$boiler_manufacturer))

## We now have coverage of 99% of boiler/year pairs.
sum(is.na(boilers$boiler_manu_clean))
sum(!is.na(boilers$boiler_manu_clean))

## We look at and clean a few other variables that might be useful controls.
boilers$efficiency_100_pct_load <- as.numeric(boilers$efficiency_100_pct_load)
sum(!is.na(boilers$efficiency_100_pct_load))/nrow(boilers)
sum(boilers$efficiency_100_pct_load == 0, na.rm = TRUE)

boilers$efficiency_50_pct_load <- as.numeric(boilers$efficiency_50_pct_load)
sum(!is.na(boilers$efficiency_50_pct_load))/nrow(boilers)
sum(boilers$efficiency_50_pct_load == 0, na.rm = TRUE)

## From a quick histogram, we see that some boilers report as a decimal and others
## as a percent. We recode values between 0 and 1 to 100x their value, to be in line
## with the majority approach.
hist(boilers$efficiency_100_pct_load)
hist(boilers$efficiency_50_pct_load)

boilers$efficiency_100_pct_load <- as.numeric(boilers$efficiency_100_pct_load)
boilers$efficiency_50_pct_load <- as.numeric(boilers$efficiency_50_pct_load)

boilers <- boilers %>%
  mutate(efficiency_100_pct_load = ifelse(efficiency_100_pct_load <= 1, efficiency_100_pct_load*100, efficiency_100_pct_load)) %>%
  mutate(efficiency_50_pct_load = ifelse(efficiency_50_pct_load <= 1, efficiency_50_pct_load*100, efficiency_50_pct_load))

table(boilers$wet_dry_bottom)
sum(!is.na(boilers$wet_dry_bottom))/nrow(boilers)
boilers$wet_dry_bottom <- toupper(boilers$wet_dry_bottom)
table(boilers$wet_dry_bottom)

table(boilers$fly_ash_reinjection)
sum(!is.na(boilers$fly_ash_reinjection))/nrow(boilers)
boilers$fly_ash_reinjection <- toupper(boilers$fly_ash_reinjection)
table(boilers$fly_ash_reinjection)

unique_id(boilers, plant_code, boiler_id, year)


## Select FGD vars ------------------------------------------------------------

fgds %>% unique_id(plant_code, fgd_id, boiler_id, year)

## The duplicates on particular boilers are essentially identical except for entry 
## errors (e.g. listing a cost as 0 instead of NA). As such, we keep only one 
## observation.

#View(fgds %>% unique_id(utility_code, plant_code, boiler_id, year))

fgds$cost_structure <- as.numeric(fgds$cost_structure)
fgds$cost_disposal <- as.numeric(fgds$cost_disposal)
fgds$cost_other <- as.numeric(fgds$cost_other)
fgds$cost_total <- as.numeric(fgds$cost_total)

fgds$expend_feed <- as.numeric(fgds$expend_feed)
fgds$expend_labor <- as.numeric(fgds$expend_labor)
fgds$expend_disposal <- as.numeric(fgds$expend_disposal)
fgds$expend_other <- as.numeric(fgds$expend_other)
fgds$expend_total <- as.numeric(fgds$expend_total)

fgds$energy_consumption <- as.numeric(fgds$energy_consumption)
fgds$sorbent_quantity <- as.numeric(fgds$sorbent_quantity)

fgds$inservice_hours <- as.numeric(fgds$inservice_hours)
fgds$removal_eff_so2 <- as.numeric(fgds$removal_eff_so2)
fgds$eff_sulfur_at_100per <- as.numeric(fgds$eff_sulfur_at_100per)
fgds$eff_sulfur_factor <- as.numeric(fgds$eff_sulfur_factor)

fgds$so2_emission_rate <- as.numeric(fgds$so2_emission_rate)
fgds$spec_coal_ash <- as.numeric(fgds$spec_coal_ash)
fgds$spec_coal_so2 <- as.numeric(fgds$spec_coal_so2)
fgds$fgd_trains_total <- as.numeric(fgds$fgd_trains_total)
fgds$fgd_trains_100per <- as.numeric(fgds$fgd_trains_100per)

plant_fgd <- fgds %>%
  select(plant_code, fgd_id, year, cost_structure, cost_disposal, cost_other, 
         cost_total, expend_feed, expend_labor, expend_other, expend_total) %>%
  unique()

unique_id(plant_fgd, plant_code, fgd_id, year)

plant_fgd <- plant_fgd %>% group_by(plant_code, year) %>%
  summarise(fgd_cost_structure_by_plt_min = sum(cost_structure, na.rm = TRUE),
         fgd_cost_structure_by_plt_tot = sum(cost_structure),
         fgd_cost_disposal_by_plt_min = sum(cost_disposal, na.rm = TRUE),
         fgd_cost_disposal_by_plt_tot = sum(cost_disposal),
         fgd_cost_other_by_plt_min = sum(cost_other, na.rm = TRUE),
         fgd_cost_other_by_plt_tot = sum(cost_other),
         fgd_cost_total_by_plt_min = sum(cost_total, na.rm = TRUE),
         fgd_cost_total_by_plt_tot = sum(cost_total)) %>% ungroup()

test <- unique_id(fgds, plant_code, boiler_id, year)
nrow(test)
#length(unique(paste(test$plant_code, test$boiler_id)))
#View(test)

unique_id(fgds, year, plant_code, boiler_id)

fgds <- fgds %>%
  mutate(fgd_inservice_y = ifelse(is.na(fgd_inservice_date), fgd_inservice_y, substr(fgd_inservice_date, 1, 4)),
         fgd_inservice_m = ifelse(is.na(fgd_inservice_date), fgd_inservice_m, substr(fgd_inservice_date, 6, 7))) %>%
  mutate(fgd_inservice_y = as.numeric(fgd_inservice_y),
         fgd_inservice_m = as.numeric(fgd_inservice_m)) %>%
  mutate(fgd_inservice_y = ifelse(fgd_inservice_y > 2040, fgd_inservice_y - 100, fgd_inservice_y))

fgds <- fgds %>%
  filter(fgd_status == "OP")

fgds <- fgds %>% select(-fgd_id) %>%
  mutate(energy_consumption_kwh = ifelse(energy_units == "KWH", energy_consumption, 1000*energy_consumption)) %>% 
  group_by(year, plant_code, boiler_id) %>%
  mutate(fgd_tot_cost_structure_by_boiler = sum(cost_structure, na.rm = TRUE),
         fgd_tot_cost_disposal_by_boiler = sum(cost_disposal, na.rm = TRUE),
         fgd_tot_cost_other_by_boiler = sum(cost_other, na.rm = TRUE),
         fgd_tot_cost_total_by_boiler = sum(cost_total, na.rm = TRUE),
         fgd_tot_expend_feed_by_boiler = sum(expend_feed, na.rm =TRUE),
         fgd_tot_expend_labor_by_boiler = sum(expend_labor, na.rm =TRUE),
         fgd_tot_expend_disposal_by_boiler = sum(expend_disposal, na.rm =TRUE),
         fgd_tot_expend_other_by_boiler = sum(expend_other, na.rm =TRUE),
         fgd_tot_expend_total_by_boiler = sum(expend_total, na.rm =TRUE),         
         num_fgds_to_boiler = n(),
         fgd_inservice_hrs_avg_by_boiler = mean(inservice_hours),
         fgd_inservice_hrs_tot_by_boiler = sum(inservice_hours, na.rm = TRUE),
         fgd_avg_removal_eff_by_boiler = mean(removal_eff_so2),
         fgd_avg_energy_consump_kwh_by_boiler = mean(energy_consumption_kwh),
         fgd_tot_energy_consump_kwh_by_boiler = sum(energy_consumption_kwh, na.rm = TRUE),
         fgd_avg_sorbent_quantity_by_boiler = mean(sorbent_quantity),
         fgd_tot_sorbent_quantity_by_boiler = sum(sorbent_quantity, na.rm = TRUE),
         fgd_avg_eff_sulfur_factor_by_boiler = mean(eff_sulfur_factor),
         fgd_avg_eff_sulfur_at_100_by_boiler = mean(eff_sulfur_at_100per),
         fgd_tot_so2_emission_rate_by_boiler = sum(so2_emission_rate, na.rm = TRUE),
         fgd_trains_tot_by_boiler = sum(fgd_trains_total, na.rm=TRUE),
         fgd_trains_100per_tot_by_boiler = sum(fgd_trains_100per),
         fgd_earliest_inservice_by_boiler = min(fgd_inservice_y)) %>% ungroup() %>%
  select(-cost_structure, -cost_disposal, -cost_other, -cost_total, -inservice_hours, -removal_eff_so2,
         -expend_feed, -expend_labor, -expend_disposal, -expend_other, -expend_total, -energy_consumption,
         -energy_units, -energy_consumption_kwh, -sorbent_quantity, -state, -county, -utility_name, -plant_name, -fgd_inservice_m,
         -eff_sulfur_factor, -eff_sulfur_at_100per, -fgd_inservice_date, -so2_emission_rate, -eff_test_date,
         -eff_test_date_code, -fgd_retirement_m, -fgd_retirement_y, -sludge_pond, -steam_plt_type,
         -plant_sum_cap, -name_of_water_source, -sludge_pond_lined, -flue_gas_exit_rate, -flue_gas_exit_temp, -fgd_inservice_date_code,
         -flue_gas_enter_fgd, -fgd_trains_total, -fgd_trains_100per, -fgd_manuf, -fgd_type1,
         -fgd_type2, -fgd_type3, -fgd_type4, -sorbent_type1, -sorbent_type2, -sorbent_type3, -sorbent_type4,
         -fgd_inservice_y, -pond_landfill_Req, -byproduct_recovery, -flue_gas_bypass_fgd,
         -spec_coal_ash, -spec_coal_so2) %>%
  unique()

## NB: "expend total" is the operating costs. It is largely unavailable after 2006.

## We take a closer look at plant-level FGD vars.
table(plant_fgd$year)

# View(plant_fgd %>% 
#   group_by(year) %>%
#   summarise_each(funs(sum(!is.na(.)))))


## Select generation vars -----------------------------------------------------

#gens %>% unique_id(plant_code, boiler_id, year)
gens %>% unique_id(plant_code, boiler_id, year, generator_id)

gens <- gens %>% filter(!is.na(year))

names(gens)

subgens <- gens %>%
  select(plant_code, year, boiler_id, 
         deliver_power_transgrid,
         nameplate_mmbtu, summer_cap_mmbtu, winter_cap_mmbtu, 
         status, gen_retirement_y, topping_bottoming,
         gen_cogen, planned_mod, 
         planned_mod, planned_uprates_net_summer_cap,  
         planned_uprates_net_winter_cap,              
         planned_uprates_y, planned_derates_net_summer_cap,
         planned_derates_net_winter_cap,              
         planned_derates_y, planned_repower_y,
         uprate_derate_completed_thisyear, uprate_derate_completed_y,
         total_gen) %>%
  filter(status != "RE") %>% select(-status)

subgens[subgens == "Y"] <- 1
subgens[subgens == "N"] <- 0

maxvars <- c("deliver_power_transgrid", "topping_bottoming",
             "gen_cogen", "planned_mod", "planned_uprates_y",
             "planned_derates_y", "planned_repower_y",
             "uprate_derate_completed_thisyear",
             "uprate_derate_completed_y", "gen_retirement_y")

sumvars <- c("nameplate_mmbtu", "summer_cap_mmbtu",
             "winter_cap_mmbtu", "total_gen",
             "planned_uprates_net_summer_cap",  
             "planned_uprates_net_winter_cap",              
             "planned_derates_net_summer_cap",
             "planned_derates_net_winter_cap")

subgens$deliver_power_transgrid <- as.numeric(subgens$deliver_power_transgrid)
subgens$topping_bottoming <- as.numeric(subgens$topping_bottoming)
subgens$gen_cogen <- as.numeric(subgens$gen_cogen)
subgens$planned_mod <- as.numeric(subgens$planned_mod)
subgens$planned_uprates_y <- as.numeric(subgens$planned_uprates_y)
subgens$planned_derates_y <- as.numeric(subgens$planned_derates_y)
subgens$planned_repower_y <- as.numeric(subgens$planned_repower_y)
subgens$uprate_derate_completed_thisyear <- as.numeric(subgens$uprate_derate_completed_thisyear)
subgens$uprate_derate_completed_y <- as.numeric(subgens$uprate_derate_completed_y)
subgens$gen_retirement_y <- as.numeric(subgens$gen_retirement_y)
subgens$nameplate_mmbtu <- as.numeric(subgens$nameplate_mmbtu)
subgens$summer_cap_mmbtu <- as.numeric(subgens$summer_cap_mmbtu)
subgens$winter_cap_mmbtu <- as.numeric(subgens$winter_cap_mmbtu)
subgens$total_gen <- as.numeric(subgens$total_gen)
subgens$planned_uprates_net_summer_cap <- as.numeric(subgens$planned_uprates_net_summer_cap)
subgens$planned_uprates_net_winter_cap <- as.numeric(subgens$planned_uprates_net_winter_cap)
subgens$planned_derates_net_summer_cap <- as.numeric(subgens$planned_derates_net_summer_cap)
subgens$planned_derates_net_winter_cap <- as.numeric(subgens$planned_derates_net_winter_cap)

subgens <- subgens %>%
  group_by(plant_code, boiler_id, year) %>%
  mutate_at(maxvars, max) %>% 
  mutate_at(sumvars, sum) %>% 
  ungroup() %>% unique()

unique_id(subgens, plant_code, year, boiler_id)

names(subgens)[4:length(names(subgens))] <- paste0("gen_", names(subgens)[4:length(names(subgens))])
names(subgens) <- gsub("gen_gen_", "gen_", names(subgens))


## Merge regulations ----------------------------------------------------------

unique_id(plant_regs, plant_code, boiler_id, year)

plant_regs <- plant_regs %>% 
  left_join(fgds)

plant_regs <- plant_regs %>%
  mutate(fgd = ifelse(!is.na(num_fgds_to_boiler), 1, 0),
         pre_1980 = ifelse(fgd_earliest_inservice_by_boiler <= 1980, 1, 0))

#View(unique_id(plant_regs, plant_code, boiler_id, year))

plant_regs <- plant_regs %>% select(plant_code, boiler_id, year, state, geo_type,
                                    linked_state_reg,
       plt_county, plant_capacity, min_plant_capacity, max_boi_nameplate,
       min_boi_nameplate, 
       reg_calc_so2_lbs_mmbtu = calc_std_so2,
       reg_so2_lbs_mmbtu = std_so2_lbs_mmbtu,
       reg_fuel_content = std_fuel_content,
       reg_so2_ppm = std_so2_ppm, reg_lbs_so2_hr = std_lbs_so2_hr,
       reg_calc_plt_lbs_so2_hr = calc_plant_std_lbs_so2_hr,
       reg_other = std_other,
       ifnew_calc_so2_lbs_mmbtu = ifnew_calc_std_so2,
       ifnew_so2_lbs_mmbtu = ifnew_std_so2_lbs_mmbtu,
       ifnew_fuel_content = ifnew_std_fuel_content,
       ifnew_so2_ppm = ifnew_std_so2_ppm,
       ifnew_calc_plt_lbs_so2_hr,
       ifnew_lbs_so2_hr = ifnew_std_lbs_so2_hr,
       ifnew_linked_state_reg) %>% unique()


## Create full sample ---------------------------------------------------------

## First, we synthesize the plant data together, but we start with the inservice 
## dataset, since if we don't have an inservice year it is useless for our purposes.

## NB: utility_code sometimes changes over time but plant_code is a unique identifier, 
## so we are going to make our full universe based ONLY on plant_code and then append
## utility code ## after.
boilers %>% unique_id(plant_code, boiler_id, year)

universe <- boilers %>% 
  select(plant_code, boiler_id, year, plt_state, inservice_y, plt_inservice_y, plt_inservice_m) %>%
  filter(!is.na(inservice_y) | !is.na(plt_inservice_y)) %>% unique() %>%
  arrange(year) %>%
  group_by(plant_code, boiler_id) %>%
  mutate(n = 1:n(), num = n()) %>%
  ungroup() %>% filter(n == num) %>% select(-year, -n, -num) %>%
  mutate(join = 1)

retirement <- boilers %>% 
  select(plant_code, boiler_id, calc_retirement_y) %>%
  unique()

unique_id(retirement, plant_code, boiler_id)

## If retirement 2018, we assume persists forward.
retirement$calc_retirement_y[retirement$calc_retirement_y == 2018] <- 9999

## Plant retirement yr
retire_plt <- boilers %>%
  filter(boiler_status != "RE") %>%
  group_by(plant_code) %>%
  summarise(plt_calc_retirement_y = max(year)) %>% ungroup()

retire_plt$plt_calc_retirement_y[retire_plt$plt_calc_retirement_y == 2018] <- 9999

retirement <- left_join(retirement, retire_plt)
  
universe <- left_join(universe, retirement)

unique_id(universe, plant_code, boiler_id)

## We want to create a universe across all years, so we do that here.
years <- data.frame(year = c(1985:2018), join = 1)

universe <- full_join(universe, years) %>% select(-join) %>%
  filter(inservice_y <= year, calc_retirement_y >= year)

unique_id(universe, plant_code, boiler_id, year)

## We want to merge on plt_county at this stage.
counties <- plants %>% select(plant_code, plt_county) %>% unique()
unique_id(counties, plant_code)

universe <- universe %>% left_join(counties)
sum(is.na(universe$plt_county))


## County information ---------------------------------------------------------

head(universe$plt_county)

counties <- universe %>% select(plt_county, plt_state) %>% unique() %>%
  mutate(merge = 1)

## We need to make sure the county names are clean and merged correctly.

ctrpts <- read.csv(here::here("data/ctrpoints_counties.csv"), stringsAsFactors = FALSE) %>%
  select(-X)
attainment <- read.csv(here::here("data/nonattainment_by_cty_year.csv"), stringsAsFactors = FALSE) %>%
  select(-X)

ctrpts <- rename(ctrpts, plt_county = county, plt_state = st_abbr) %>%
  mutate(merge1 = 1)
ctrpts$plt_county <- toupper(ctrpts$plt_county)

unique_id(ctrpts, plt_county, plt_state)

attainment <- rename(attainment, plt_state = st_abbr, plt_county = county) %>%
  mutate(merge1 = 1)

test <- left_join(counties, ctrpts)
sum(is.na(test$merge1))

ctrpts$plt_county[ctrpts$plt_county == "DE SOTO"] <- "DESOTO"
ctrpts$plt_county[ctrpts$plt_county == "PORTSMOUTH"] <- "PORTSMOUTH CITY"
ctrpts$plt_county[ctrpts$plt_county == "CHESAPEAKE"] <- "CHESAPEAKE CITY"
ctrpts$plt_county[ctrpts$plt_county == "HOPEWELL"] <- "HOPEWELL CITY"
ctrpts$plt_county[ctrpts$plt_county == "COVINGTON" & ctrpts$plt_state == "VA"] <- "COVINGTON CITY"

test <- left_join(counties, ctrpts)
sum(is.na(test$merge1))

## We see if every attainment county maps onto ctrpts, so we can be sure they will 
## map on properly to the plant data (since a non-match, in this case, indicates
## attainment, we require this backcheck).

## There are five, initially, that do not merge.
test3 <- left_join(attainment %>% select(-merge1), ctrpts %>% mutate(m = 1)) %>% 
  select(state_name, plt_state, plt_county, m) %>% unique()
sum(is.na(test3$m))

attainment$plt_county[attainment$plt_county == "DE SOTO"] <- "DESOTO"
attainment$plt_county[attainment$plt_county == "PORTSMOUTH"] <- "PORTSMOUTH CITY"
attainment$plt_county[attainment$plt_county == "CHESAPEAKE"] <- "CHESAPEAKE CITY"
attainment$plt_county[attainment$plt_county == "HOPEWELL"] <- "HOPEWELL CITY"
attainment$plt_county[attainment$plt_county == "COVINGTON" & attainment$plt_state == "VA"] <- "COVINGTON CITY"
#attainment$plt_county[attainment$plt_county == "RICHMOND" & attainment$plt_state == "VA"] <- "RICHMOND CITY"

test3 <- left_join(attainment %>% select(-merge1), ctrpts %>% mutate(m = 1)) %>% 
  select(state_name, plt_state, plt_county, m) %>% unique()
sum(is.na(test3$m))

## The last one is in Guam.
test3 %>% filter(is.na(m))

county_data <- left_join(counties, ctrpts) %>% 
  select(-merge, -merge1) %>% mutate(join = 1)

county_data <- full_join(county_data, years)

county_data <- county_data %>%
  left_join(attainment)

## Now, we need to merge county_data back onto the plant data.
universe <- universe %>% left_join(county_data)
sum(is.na(universe$cty_lat))

## Now, we clean up the attainment data. If the year is 1977 or earlier, the 
## attainment vars should be NA.  If 1978 or later, if the var is NA, it means 
## the county was in attainment (so the var should be 0).
table(universe$so2_nonattain)
universe$so2_nonattain[is.na(universe$so2_nonattain)] <- 0
universe$so2_nonattain[universe$year <= 1977] <- NA

table(universe$so2_nonattain)
sum(is.na(universe$so2_nonattain))

table(universe$other_nonattain)
universe$other_nonattain[is.na(universe$other_nonattain)] <- 0
universe$other_nonattain[universe$year <= 1977 | universe$year == 1991] <- NA

table(universe$other_nonattain)
sum(is.na(universe$other_nonattain))


## Get fuel dataset utility ids -----------------------------------------------

names(fuel_comp)
names(all_fuel)

## We confirm proper identification
fuel_comp %>% unique_id(plant_code, boiler_id, year)

all_fuel %>% unique_id(plant_code, year)

## Plant codes are unique, so we can rely on that and the year to impute the 
## correct utility_code.
universe %>% unique_id(plant_code, boiler_id, year)


# MERGE -------------------------------------------------------------------------------------------

full <- universe %>% 
  select(plant_code, boiler_id, calc_inservice_y = inservice_y, plt_calc_inservice_y = plt_inservice_y, calc_retirement_y, plt_calc_retirement_y, 
         year, plt_state, plt_county, cty_lat, cty_lon, fips_st, fips_cnty,
         so2_nonattain, other_nonattain) %>%
  left_join(plant_regs %>% rename(reg_masked_plt_county = plt_county) %>% select(-state)) %>%
  left_join(plants %>% select(-plt_state)) %>%
  left_join(all_fuel) %>%
  left_join(fuel_comp) %>%
  left_join(boilers %>% select(-plt_state, -utility_name, -utility_code, -full_id, -calc_retirement_y) %>% mutate(flag_data_in_year = 1)) %>%
  left_join(plant_fgd) %>%
  left_join(fgds) %>%
  left_join(subgens)

full <- full %>%
  filter(!plt_state %in% c("CA", "OH"))

## We drop North Carolina post 2010 because the regulation is at the utility level.
full <- full %>%
  filter(!(plt_state == "NC" & year > 2009))

states <- read.csv(here::here("data/xwalk/state_code_xwalk.csv"), stringsAsFactors = FALSE)
full <- left_join(full, states, by = "plt_state")

# No observations matched to multiple regs, which is as it should be.
unique_id(full, utility_code, plant_code, boiler_id, year)

## We fill in values that should remain over time, but we added interim years. 
## We see we have tidied everything up well.
sum(is.na(full$calc_retirement_y))
sum(is.na(full$plt_calc_inservice_y))
sum(is.na(full$calc_inservice_y))

## Now, we move to NSR permits.
table(full$nsr_permit_y)

table(!is.na(full$nsr_permit_y), full$year)

## NSR Permit Y is only available in recent years. What we do, is we take the
## EARLIEST permit year given and make a dummy equal to 1 for all years greater 
## than or equal to the year given.
full <- full %>%
  mutate(nsr_permit_y = as.numeric(nsr_permit_y)) %>%
  group_by(utility_code, plant_code, boiler_id) %>%
  mutate(nsr_first_permit_y = min(nsr_permit_y, na.rm = TRUE)) %>%
  ungroup()

full$nsr_first_permit_y[is.na(full$nsr_first_permit_y) | full$nsr_first_permit_y== Inf] <- 9999

## However, if a boiler wasn't in the dataset in these later years, we can't know 
## if there is no permit because it's not in the dataset or because it didn't have 
## one, so we have to recode those as NA.
full <- full %>% mutate(full_id = paste(plant_code, boiler_id))
post2009 <- full %>% filter(year >= 2009) %>% select(plant_code, boiler_id, full_id) %>% unique()

table(full$nsr_first_permit_y)
sum(is.na(full$nsr_first_permit_y))

full$nsr_first_permit_y[!full$full_id %in% post2009$full_id] <- NA

table(full$nsr_first_permit_y)
sum(is.na(full$nsr_first_permit_y))

full <- full %>%
  mutate(nsr_d_calc = ifelse(nsr_first_permit_y <= year, 1, 0)) %>%
  mutate(nsr_in_y = ifelse(nsr_first_permit_y == year, 1, 0))

## Now, we need to do the same thing for scrubbers. We are going to look at the 
## earliest year that a boiler had a scrubber.
full <- full %>%
  group_by(plant_code, boiler_id) %>%
  mutate(fgd_earliest_inservice_by_boiler = min(fgd_earliest_inservice_by_boiler, na.rm = TRUE)) %>%
  ungroup()

## As before, if no FGD, we want inservice year = 9999.
full$fgd_earliest_inservice_by_boiler[is.na(full$fgd_earliest_inservice_by_boiler) 
                                      | full$fgd_earliest_inservice_by_boiler == Inf] <- 9999

## Again, as before, if the boiler isn't present in later years, we don't know if 
## it might have had a boiler earlier, so after we generate the dummy we'll replace 
## with NA.
table(full$fgd_earliest_inservice_by_boiler)
sum(is.na(full$fgd_earliest_inservice_by_boiler))

## fgd_d_calc is a dummy for whether or not there is an fgd in a given year.
full <- full %>%
  mutate(fgd_d_calc = ifelse(year >= fgd_earliest_inservice_by_boiler, 1, 0))

full$fgd_earliest_inservice_by_boiler[!full$full_id %in% post2009$full_id] <- NA
full$fgd_d_calc[!full$full_id %in% post2009$full_id] <- NA

table(full$fgd_earliest_inservice_by_boiler)
sum(is.na(full$fgd_earliest_inservice_by_boiler))
table(full$fgd_d_calc)
sum(is.na(full$fgd_d_calc))

# nrow(unique_id(plant_regs, plant_code, utility_code, boiler_id, year))
# unique_id(plants, plant_code, utility_code, year)
# unique_id(fuel, plant_code, utility_code, year)
# unique_id(boilers, plant_code, utility_code, year, boiler_id)
# unique_id(fgds, plant_code, utility_code, year, boiler_id)

## Some of our regs are duplicates because we need to determine whether the plant 
## has a scrubber to determine which reg applies.

## Identified at boiler level
# nrow(unique_id(dataset, plant_code, utility_code, year, boiler_id, fgd_id))

## We verify inservice year is ALWAYS present and retirement year is always present.
sum(is.na(full$calc_inservice_y))
sum(is.na(full$calc_retirement_y))

write.csv(full, here::here("data/all_years_all_plants_and_features.csv"))


### END CODE ###

