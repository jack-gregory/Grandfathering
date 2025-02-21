## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 07_generate_regression_vars
## Bridget Pals
## 08 February 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## Final data cleaning to generate features for the model.


### START CODE ###


# PREAMBLE ----------------------------------------------------------------------------------------

## Initiate
## ... Packages
pkgs <- c(
  "fs","here",                      # File system
  "dplyr","Hmisc","zoo"             # Data wrangling
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

## ... Functions
source(here::here("src/boilers.R"))

## ... Definitions
date <- format(Sys.Date(), "%Y%m%d")
fs::dir_create(here::here("out", date))


# IMPORT ------------------------------------------------------------------------------------------

boilers <- read.csv(here::here("data/all_years_all_plants_and_features.csv"), 
                    stringsAsFactors = FALSE) %>%
  select(-X)

boilers <- boilers %>%
  mutate(age_plant = year - plt_calc_inservice_y,
         age_boiler = year - calc_inservice_y)

sum(is.na(boilers$age_plant))
sum(is.na(boilers$age_boiler))

unique_id(boilers, plant_code, boiler_id, year)


# CLEAN -------------------------------------------------------------------------------------------

## Drop unnecessary vars ------------------------------------------------------

## We drop some unnecessary vars.
names(boilers)

drop1 <- grep("util_", names(boilers))

boilers <- boilers[-drop1]

names(boilers)

boilers <- boilers %>%
  select(-datum, -pipeline_notes)

names(boilers)


## Create modification indicators ---------------------------------------------

boilers <- boilers %>%
  mutate(mod_D = ifelse(type_of_boiler == "D" & calc_inservice_y < 1972, 1 ,0),
         year_D = ifelse(type_of_boiler == "D" & calc_inservice_y < 1972, year, 9999),
         mod_Da = ifelse(type_of_boiler == "DA" & calc_inservice_y < 1978, 1, 0),
         year_Da = ifelse(type_of_boiler == "DA" & calc_inservice_y < 1978, year, 9999),
         mod_Db = ifelse(type_of_boiler == "DB" & calc_inservice_y < 1985, 1, 0),
         year_Db = ifelse(type_of_boiler == "DB" & calc_inservice_y < 1978, year, 9999)) %>%
  group_by(plant_code, boiler_id) %>%
  mutate(year_D = min(year_D, na.rm = TRUE),
         year_Da = min(year_Da, na.rm = TRUE),
         year_Db = min(year_Db, na.rm = TRUE)) %>%
  ungroup()

table(boilers$mod_D)

boilers$mod_D[is.na(boilers$mod_D)] <- 0

## Few potential mods in Da or Db.
table(boilers$mod_Da)
table(boilers$mod_Db)

table(boilers$year_D)
table(boilers$year_Da)
table(boilers$year_Db)

boilers$year_D[boilers$year_D == 9999] <- NA
boilers$year_D[boilers$year_D == Inf] <- NA

boilers$year_Da[boilers$year_Da == 9999] <- NA
boilers$year_Da[boilers$year_Da == Inf] <- NA

boilers$year_Db[boilers$year_Db == 9999] <- NA
boilers$year_Db[boilers$year_Db == Inf] <- NA

boilers <- boilers %>%
  mutate(mod_D_in_y = ifelse(year_D == year, 1, 0),
         mod_Da_in_y = ifelse(year_Da == year, 1, 0),
         mod_Db_in_y = ifelse(year_Db == year, 1, 0))

boilers$mod_D_in_y[is.na(boilers$mod_D_in_y)] <- 0
boilers$mod_Da_in_y[is.na(boilers$mod_Da_in_y)] <- 0
boilers$mod_Db_in_y[is.na(boilers$mod_Db_in_y)] <- 0

## Add flag if change regulation from state to fed.

## NB: This data begins in 1985 (and has reasonably good coverage after that year
## (approximately 89% non-missing).
table(boilers$so2_regulation)
sum(is.na(boilers$so2_regulation))/nrow(boilers)

table(boilers$year, boilers$so2_regulation)
sum(is.na(boilers$so2_regulation) & boilers$year >= 1985)
sum(!is.na(boilers$so2_regulation) & boilers$year >= 1985)

boilers <- boilers %>%
  mutate(federal_so2_std = ifelse(so2_regulation == "FD", 1, 0),
         mod_federal = ifelse(so2_regulation =="FD" & calc_inservice_y < 1972 & 
                                ((so2_std_rate == 1.2 & so2_unit == "DP") |
                                   (so2_std_rate == 0.6 & so2_unit == "SB")) , 1, 0),
         year_mod_fed = ifelse(mod_federal == 1, year, 9999)) %>%
  group_by(plant_code, boiler_id) %>%
  mutate(year_mod_fed = min(year_mod_fed, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mod_fed_in_y = ifelse(year_mod_fed == year, 1, 0))

boilers$year_mod_fed[boilers$year_mod_fed == 9999] <- NA
boilers$mod_federal[is.na(boilers$mod_federal)] <- 0


## Convert percent sulfur standards -------------------------------------------

## For now, of the top three fuels in primary_fuel1, bituminous is a primary fuel
## for 62% of obs, subbituminous for 35% of obs, and lignite for 3% of obs.
## Using the conversion table in the CFR, we calculate average conversion factors of: 
##    (1.66*0.62 + 2.22*0.35+2.86*0.03) = 1.89 for % Sulfur to lbs/mmBTU
##    (0.00287*0.62 + 0.00384*0.35 + 0*0.03) = 0.00312 for ppm to lbs/mmBTU
## See https://www.law.cornell.edu/cfr/text/40/appendix-B_to_part_72.
table(boilers$primary_fuel1)

boilers$reg_calc_so2_lbs_mmbtu <- as.numeric(boilers$reg_calc_so2_lbs_mmbtu)
boilers$reg_fuel_content <- as.numeric(boilers$reg_fuel_content)

boilers$ifnew_calc_so2_lbs_mmbtu <- as.numeric(boilers$ifnew_calc_so2_lbs_mmbtu)
boilers$ifnew_fuel_content <- as.numeric(boilers$ifnew_fuel_content)

boilers <- boilers %>%
  mutate(reg_so2_lbs_mmbtu_calc = ifelse(!is.na(reg_calc_so2_lbs_mmbtu), reg_calc_so2_lbs_mmbtu,
                                         (reg_fuel_content*100*1.89)),
         ifnew_so2_lbs_mmbtu_calc = ifelse(!is.na(ifnew_calc_so2_lbs_mmbtu), ifnew_calc_so2_lbs_mmbtu,
                                           (ifnew_fuel_content*100*1.89))) %>%
  mutate(reg_so2_lbs_mmbtu_calc = ifelse(!is.na(reg_so2_lbs_mmbtu_calc), reg_so2_lbs_mmbtu_calc,
                                         (reg_so2_ppm*0.00312)),
         ifnew_so2_lbs_mmbtu_calc = ifelse(!is.na(ifnew_so2_lbs_mmbtu_calc), ifnew_so2_lbs_mmbtu_calc,
                                           (ifnew_so2_ppm*0.00312))) %>%
  rename(reg_so2_lbs_mmbtu_weq = reg_calc_so2_lbs_mmbtu)

## We choose 9.09 as our stand-in value because coal rarely contains 10% sulfur, 
## but a 10% sulfur content would yield lbs/mmbtu of 9.09lbs/mmbtu.  We only 
## replace if the boiler wasn't joined to a reg (after all, some boilers) are 
## still regulated just under other paradigms, i.e., ppm.
boilers$reg_so2_lbs_mmbtu_calc[is.na(boilers$reg_so2_lbs_mmbtu_calc)] <- 9.0909
boilers$ifnew_so2_lbs_mmbtu_calc[is.na(boilers$ifnew_so2_lbs_mmbtu_calc)] <- 9.0909

boilers$reg_so2_lbs_mmbtu_calc[boilers$reg_so2_lbs_mmbtu_calc == 9.0909 & boilers$linked_state_reg == "YES"] <- NA
boilers$reg_so2_lbs_mmbtu_calc[boilers$reg_so2_lbs_mmbtu_calc == 9.0909 & boilers$linked_state_reg == "YES"] <- NA
boilers$reg_so2_lbs_mmbtu_calc[boilers$reg_so2_lbs_mmbtu_calc == 9.0909 & boilers$linked_state_reg == "YES"] <- NA
hist(boilers$reg_so2_lbs_mmbtu_calc)

boilers$ifnew_so2_lbs_mmbtu_calc[boilers$ifnew_so2_lbs_mmbtu_calc == 9.0909 & boilers$ifnew_linked_state_reg == "YES"] <- NA
boilers$ifnew_so2_lbs_mmbtu_calc[boilers$ifnew_so2_lbs_mmbtu_calc == 9.0909 & boilers$ifnew_linked_state_reg == "YES"] <- NA
boilers$ifnew_so2_lbs_mmbtu_calc[boilers$ifnew_so2_lbs_mmbtu_calc == 9.0909 & boilers$ifnew_linked_state_reg == "YES"] <- NA
hist(boilers$ifnew_so2_lbs_mmbtu_calc)

# boilers$ifnew_so2_lbs_mmbtu_calc[!is.na(boilers$ifnew_lbs_so2_hr) & is.na(boilers$ifnew_so2_lbs_mmbtu)] <- NA
# boilers$ifnew_so2_lbs_mmbtu_calc[!is.na(boilers$ifnew_so2_ppm) & is.na(boilers$ifnew_so2_lbs_mmbtu)] <- NA
# hist(boilers$ifnew_so2_lbs_mmbtu_calc)

## We also need to apply federal standards to this.  First, we pretend a new plant 
## is being built.  We use plant b/c if you were building a bunch of boilers 
## altogether, it would count as a new source (same as a single big boiler).
boilers <- boilers %>%
  mutate(fed = ifelse(plant_capacity > 250, 1.2, 100),
         ifnew_fed_state_min_std = pmin(fed, ifnew_so2_lbs_mmbtu_calc, na.rm = TRUE)) %>%
  mutate(ifnew_req_scrubber = ifelse(fed == 1, 1, 0))

boilers <- boilers %>%
  mutate(fed = ifelse(plant_capacity > 250 & (calc_inservice_y >= 1972 | nsr_d_calc == 1), 1.2, 100),
         fed_state_min_std = pmin(fed, reg_so2_lbs_mmbtu_calc, na.rm = TRUE),
         fed_std_req_scrubber = ifelse(type_of_boiler == "DA", 1, 0)) %>%
  select(-fed)

## Quick test: what if we make a histogram by boiler?
test <- boilers %>% select(plant_code, boiler_id, reg_so2_lbs_mmbtu_calc) %>%
  group_by(plant_code, boiler_id) %>%
  summarise(reg_so2_lbs_mmbtu_calc = min(reg_so2_lbs_mmbtu_calc, na.rm = TRUE))

hist(test$reg_so2_lbs_mmbtu_calc)


## Additional vars of interest ------------------------------------------------

boilers$plt_anngen <- as.numeric(boilers$plt_anngen)
boilers$total_quantity <- as.numeric(boilers$total_quantity)
boilers$gen_total_gen <- as.numeric(boilers$gen_total_gen)
boilers$fgd_tot_cost_total_by_boiler <- as.numeric(boilers$fgd_tot_cost_total_by_boiler)
boilers$new_exp_air_abate <- as.numeric(boilers$new_exp_air_abate)
boilers$ann_sulfur_content <- as.numeric(boilers$ann_sulfur_content)

boilers <- boilers %>% 
  arrange(plant_code, boiler_id, year) %>%
  group_by(plant_code, boiler_id) %>%
  mutate(pltanngen3yr = rollmean(plt_anngen, k = 3, fill = NA),
         pltanngen10yr = rollmean(plt_anngen, k = 10, fill = NA),
         pltanngeninc = plt_anngen/lag(plt_anngen),
         boiler_quant3yr = rollmean(total_quantity, k = 3, fill = NA),
         boiler_quant5yr = rollmean(total_quantity, k = 5, fill = NA),
         boiler_quant10yr = rollmean(total_quantity, k = 10, fill = NA),
         boiler_quantinc1y = total_quantity/lag(total_quantity),
         boiler_quantinc3yr = total_quantity/lag(boiler_quant3yr),
         boiler_quantinc5yr = total_quantity/lag(boiler_quant5yr),
         boiler_quantinc10yr = total_quantity/lag(boiler_quant10yr),
         boiler_geninc = gen_total_gen/lag(gen_total_gen),
         fgd_costinc1yr = fgd_tot_cost_total_by_boiler/lag(fgd_tot_cost_total_by_boiler),
         fgd_cost3yr = rollmean(fgd_tot_cost_total_by_boiler, k = 3, fill = NA),
         fgd_cost5yr = rollmean(fgd_tot_cost_total_by_boiler, k = 5, fill = NA),
         fgd_cost10yr = rollmean(fgd_tot_cost_total_by_boiler, k = 10, fill = NA),
         fgd_costinc3yr = fgd_tot_cost_total_by_boiler/lag(fgd_cost3yr),
         fgd_costinc5yr = fgd_tot_cost_total_by_boiler/lag(fgd_cost5yr),
         fgd_costinc10yr = fgd_tot_cost_total_by_boiler/lag(fgd_cost10yr),
         air_abate3yr = rollmean(new_exp_air_abate, k = 3, fill = NA),
         ann_sulfur_content3yr = rollmean(ann_sulfur_content, k =3, fill = NA),
         ann_sulfur_content5yr = rollmean(ann_sulfur_content, k = 5, fill = NA),
         ann_sulfur_content10yr = rollmean(ann_sulfur_content, k = 10, fill = NA),
         ann_sulfur_inc3yr = ann_sulfur_content/lag(ann_sulfur_content3yr),
         ann_sulfur_inc5yr = ann_sulfur_content/lag(ann_sulfur_content5yr),
         ann_sulfur_inc10yr = ann_sulfur_content/lag(ann_sulfur_content10yr),
         hasfgd = ifelse(!is.na(num_fgds_to_boiler) & fgd_status == "OP", 1, 0)) %>%
  ungroup() %>%
  select(-plt_state) %>% rename(plt_state = state) %>%
  mutate(retired_boiler = ifelse(boiler_status == "RE", 1, 0),
         retire_this_year = ifelse(calc_retirement_y == year, 1, 0))

hist(boilers$boiler_quantinc3yr)

str(boilers)
str(boilers[100:200])

write.csv(boilers, here::here("data/regression_vars.csv"))

cor(data.frame(boilers$mod_fed_in_y, boilers$nsr_in_y, boilers$mod_D_in_y))
cor(data.frame(boilers$mod_federal, boilers$mod_D, boilers$nsr_d_calc))

sum(boilers$mod_fed_in_y)
sum(boilers$nsr_in_y)
sum(boilers$mod_D_in_y)

test <- boilers %>% 
  select(plant_code, boiler_id, nsr_d_calc, mod_D,  mod_Da, mod_Db, mod_federal) %>%
  group_by(plant_code, boiler_id) %>%
  summarise(nsr_d_calc = max(nsr_d_calc),
            mod_D = max(mod_D, na.rm = TRUE),
            mod_Da = max(mod_Da, na.rm = TRUE),
            mod_Db = max(mod_Db, na.rm = TRUE),
            mod_federal = max(mod_federal)) %>% ungroup() %>%
  mutate(mod_any = ifelse(mod_D + mod_Da + mod_Db >= 1, 1, 0))

cor(data.frame(test$nsr_d_calc, test$mod_D, test$mod_federal), use = "complete.obs")

sum(test$nsr_d_calc == 1 & test$mod_any == 1, na.rm = TRUE)/sum(test$mod_any == 1, na.rm = TRUE)

sum(test$nsr_d_calc == 1 & test$mod_federal == 1, na.rm = TRUE)

sum(test$mod_D == 1 & test$mod_federal == 1)


## Summary of vars ------------------------------------------------------------

## Create some summary statistics
sink(here::here("out", date, "summary_statistics_reg_vars.txt"))

describe(boilers)

sink()

names(boilers)

summary <- boilers %>%
  group_by(year) %>%
  summarise_each(funs(sum(!is.na(.))))

write.csv(summary, here::here("out", date, "non_missing_values_by_year_reg_var.csv"))


### END CODE ###

