** %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
** Grandfathering
** regression_data
** Sylwia Bialek
** 08 December 2024
** %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


** INTRODUCTION -----------------------------------------------------------------------------------
** This program preprocesses the data in advance of the analysis for the Grandfathering project.


*** START CODE ***


** SETUP ------------------------------------------------------------------------------------------

** Set definitions
set matsize  900, permanently
global path_data "your_path_here\data"

** Define program
cap program drop states_def
program define states_def, rclass
  replace state="Alaska" if state=="AK"
  replace state="Alabama" if state=="AL"
  replace state="Arkansas" if state=="AR" 	
  replace state="Arizona" if state=="AZ" 	
  replace state="California" if state=="CA"
  replace state="Colorado" if state=="CO"
  replace state="Connecticut" if state=="CT"
  replace state="Delaware" if state=="DE"
  replace state="Florida" if state=="FL"
  replace state="Georgia" if state=="GA"
  replace state="Hawaii" if state=="HI"
  replace state="Idaho" if state=="ID"
  replace state="Illinois" if state=="IL"
  replace state="Indiana" if state=="IN"
  replace state="Iowa" if state=="IA"
  replace state="Kansas" if state=="KS"
  replace state="Kentucky" if state=="KY"
  replace state="Louisiana" if state=="LA"
  replace state="Maine" if state=="ME"
  replace state="Maryland" if state=="MD"
  replace state="Massachusetts" if state=="MA"
  replace state="Michigan" if state=="MI"
  replace state="Minnesota" if state=="MN"
  replace state="Mississippi" if state=="MS"
  replace state="Missouri" if state=="MO"
  replace state="Montana" if state=="MT"
  replace state="Nebraska" if state=="NE"
  replace state="Nevada" if state=="NV"
  replace state="New Hampshire" if state=="NH"
  replace state="New Jersey" if state=="NJ"
  replace state="New Mexico" if state=="NM"
  replace state="New York" if state=="NY"
  replace state="North Carolina" if state=="NC"
  replace state="North Dakota" if state=="ND"
  replace state="Ohio" if state=="OH"
  replace state="Oklahoma" if state=="OK"
  replace state="Oregon" if state=="OR"
  replace state="Pennsylvania" if state=="PA"
  replace state="Rhode Island" if state=="RI"
  replace state="South Carolina" if state=="SC"
  replace state="South Dakota" if state=="SD"
  replace state="Tennessee" if state=="TN"
  replace state="Texas" if state=="TX"
  replace state="Utah" if state=="UT"
  replace state="Vermont" if state=="VT"
  replace state="Virginia" if state=="VA"
  replace state="Washington" if state=="WA"
  replace state="West Virginia" if state=="WV"
  replace state="Wisconsin" if state=="WI"
  replace state="Wyoming" if state=="WY"
end
 

** Prepare data to be merged ----------------------------------------------------------------------

** Get demand data by state
clear
import delimited "$path_data\eia\EIA electricity demand data.csv", varnames(1) 
keep if msn=="ESTCB"
reshape long y, i(state) j(year)
drop data_status msn
states_def
rename y el_demand
sort state year
gen d_growth=(el_demand[_n+1]-el_demand)/el_demand if state==state[_n+1]
rename state plt_state
save "$path_data\electricity_demand_by_state.dta", replace

** Get improved ownership assignment
import delimited "$path_data\utils_2007_2018.csv", varnames(1)  clear
keep if util_entity_type!="NA"
drop util_attn_line	v1	util_pobox util_street_address	util_city	util_state	///
  util_zip util_zip4	util_contact_firstname	util_contact_lastname	util_contact_title	///
  util_contact2_firstname	util_contact2_lastname	util_contact2_tile util_street_address2	///
  util_owner	util_operator	util_asset_manager	util_other_relationships_plants
drop if utility_code==.
drop if year==.
* C - co-op, Q - Independent Power Producer, I- Investor owned utility, P- political, 
* F - fed, S- state. Ind-industrial, Com - commercial, M - municipial
save "$path_data\utility_data.dta", replace

** Get info about grandfathering status
clear
import excel "$path_data\regs\gf_status_born_around_1978",  firstrow
keep plant_code boiler_id gf
rename gf pre_1978
drop if plant_code==.
save "$path_data\gf_status_born_around_1978.dta", replace

** Get additional info on independent power producers
** NB: data is from EIA 867 available for years 1989 - 1998
clear
import excel "$path_data\eia\1989_1998_Nonutility_Power_Producer.xls", sheet("Data") firstrow
gen sect=8 if (Sector==2| Sector==3)
replace sect=2 if (Sector==4| Sector==5)
replace sect=5 if (Sector==6| Sector==7)
drop if plant_code==.
*drop State_IPP
save "$path_data\nonutilityData.dta", replace

** Get information on low sulfur coal availability
** NB: data is from EIA 923 and 423
clear
import delimited "$path_data\sulfur_iv.csv"
rename orispl plant_code
save "$path_data\sulfur_iv.dta", replace

** Import data on which power plants were in the first phase of ACid Rain Program
clear
import delimited "$path_data\regs\ARP_power_plants.csv", clear 
gen ARP_subject=1
save "$path_data\ARP_PhaseI.dta", replace

** Import data on ACid Rain Program permit prices
clear
import excel "$path_data\regs\ARP_prices.xlsx", sheet("Data") firstrow
save "$path_data\ARP_prices.dta", replace

** Import data on coal trades (including sulfur intesity)
clear
import delimited "$path_data\coal_char_data_by_plant_year.csv", clear 
drop v1 mine_cou*
save "$path_data\coal_trades_data.dta", replace

** Prepare annual CEMS data on emissions
use "$path_data\cems_yr.dta", clear 
rename ORISPL plant_code
rename GF_BOILER boiler_id
rename YEAR year
save "$path_data\cems_yearly.dta", replace
 
** Prepare data for input prices (coal and gas)
import excel "$path_data\eia_fuel.xlsx", sheet("Fuel by State") firstrow clear
sort STATE YR FUEL
gen coal2gas_price=COST_ENRG/COST_ENRG[_n+1] if (FUEL=="Coal" & COST_ENRG!=0) 
drop if FUEL!="Coal"
** For some states the price data is missing. Use the country-wide average
bys YR: egen yearly_aver=mean(coal2gas_price)
replace coal2gas_price=yearly_aver if coal2gas_price==.
rename YR year
rename STATE state
states_def
rename state plt_state
keep year plt_state coal2gas_price
save "$path_data\coal2gasPrice.dta", replace

** Import data on state-level capacities of existing power plants
clear
import delimited "$path_data\eia_capacity.csv", clear 
states_def
bys state year: egen cap_state_tot=sum(nameplate_cap)
drop generator facility summer_cap
rename nameplate_cap cap_state
replace fuel="NatGas" if fuel=="Natural Gas"
rename state plt_state
reshape wide cap_state, i(plt_state year) j(fuel) string
sort plt_state year
gen state_cap_growth=(cap_state_tot-cap_state_tot[_n-1])/cap_state_tot[_n-1] if plt_state==plt_state[_n-1]
save "$path_data\State_capacity.dta", replace


** Merge, clean data, prepare new variables -------------------------------------------------------

clear
import delimited "$path_data\regression_vars.csv", clear

** Basic cleaning
drop v1 transmission_distrib_owner transmission_distrib_ownerid transmission_distrib_owner_state ///
  regulatory_status ferc_cogen_status ferc_cogen_docketnum ferc_small_power_producer_status ///
  ferc_small_power_producer_docket ferc_exempt_wholesale_gen_status ferc_exempt_wholesale_gen_docket ///
  sector_name sector iso_rto iso_rto_code grid_voltage plt_latitude plt_longitude utility_name ///
  balancing_auth_code balancing_auth_name net_metering ash_impoundment ash_impoundment_lined ///
  ash_impoundment_status grid_voltage2 grid_voltage3 nat_gas_pipeline_name1 energy_storage ///
  nat_gas_ldc_name nat_gas_pipeline_name2 nat_gas_pipeline_name3 nat_gas_storage liquefied_nat_gas_storage ///
  fly_ash_landfill fly_ash_ponds fly_ash_onsite fly_ash_sold fly_ash_offsite bott_ash_total ///
  bott_ash_landfill bott_ash_ponds bott_ash_onsite bott_ash_sold bott_ash_offsite sludge_total ///
  sludge_landfill sludge_ponds sludge_onsite sludge_sold sludge_offsite gypsum_total ///
  coal_year_plus_5 coal_year_plus_10 petro_year_plus_5 petro_year_plus_10 gas_year_plus_5 ///
  gas_year_plus_10 gas_yr_plus_5_contract gas_yr_plus_10_contract gas_yr_plus_5_total gas_yr_plus_10_total ///
  gas_heat_year_plus_5 gas_heat_year_plus_10 pet_fire_steam_flow gas_fire_steam_flow max_steam_flow ///
  coal_fire_steam_flow primary_fuel7 primary_fuel7 fire_primary_fuel1 fire_primary_fuel2 fly_ash_reinjection ///
  wet_dry_bottom hg_emission_ctrl hg_emission_acj hg_emission_bh hg_emission_ds hg_emission_ep ///
  hg_emission_fgd hg_emission_li hg_emission_ws name_of_water_source hg_emission_bs hg_emission_bp ///
  hg_emission_br hg_emission_ec hg_emission_eh hg_emission_ek hg_emission_ew nox_proposed_ctrl3 ///
  nox_proposed_ctrl2 hg_regulation hg_comp_y hg_existing_ctrl1 hg_existing_ctrl2 hg_existing_ctrl3 ///
  hg_proposed_ctrl1 hg_proposed_ctrl2 hg_proposed_ctrl3 boiler_quantinc10yr boiler_quantinc5yr ///
  gen_planned_repower_y gen_topping_bottoming gen_winter_cap_mmbtu water_source fgd_trains_100per ///
  gypsum_ponds gypsum_landfill gypsum_onsite gypsum_sold gypsum_offsite other_byproduct_total ///
  other_byprod_landfill other_byprod_ponds other_byprod_onsite other_byprod_sold other_byprod_offsite ///
  exp_fly_ash_collection exp_fly_ash_disposal exp_bott_ash_collection exp_bott_ash_disposal ///
  exp_fgd_collection exp_water_abate_coll exp_water_abate_disp exp_other_collection exp_other_disposal ///
  exp_other_other useful_therm_directheat useful_therm_spaceheat useful_therm_processheat useful_therm_enduser ///
  useful_therm_other useful_therm_other_desc other_byprod_total new_exp_water_abate new_exp_waste_abate ///
  batch_no steam_sold waste_heat_input low_nox_process3 low_nox_process2 low_nox_manufacturer ///
  turndown_ratio total_nox_hours nox_removal_op_factor nox_removal_may_sept hg_emission_type fuel8_is_coal ///
  fuel7_is_coal waste_produced gen_planned_uprates_net_winter_c gen_planned_derates_net_winter_c ///
  gen_uprate_derate_completed_this pm_ctrl_noncomp1 pm_ctrl_noncomp2 pm_ctrl_noncomp3 nox_period pm_comp_y ///
  nox_comp_y nox_std_rate nox_unit pm_unit pm_std_rate pm_period nox_ctrl_noncomp1 nox_ctrl_noncomp2 ///
  nox_ctrl_noncomp3 byprod_rev_fly_ash byprod_rev_bott_ash byprod_rev_fly_bott_ash  byprod_rev_other ///
  byprod_rev_total new_exp_abate_water new_exp_abate_waste new_exp_other_abate new_exp_air_abate fly_ash_total ///
  nerc naics petro_heat_yr_plus_5 petro_heat_yr_plus_10 petro_sulfur_yr_plus_5 petro_sulfur_yr_plus_10 ///
  hg_emission_ot air_abate3yr boiler_quant10yr sludge_pond alt_fuel_usage_30_days alt_fuel_days_to_switch ///
  year_alt_fuel_last_used primary_fuel8 primary_fuel6 ann_ash_content plt_fuel_sc plt_fuel_sub plt_fuel_wc ///
  plt_fuel_lig plt_st_address exp_total_collection exp_total_disposal exp_total_other fire_primary_fuel3 ///
  air_flow_100_pct_load alt_fuel_code3 alt_fuel_code2 fuel6_is_coal fuel5_is_coal fuel4_is_coal fuel3_is_coal ///
  fuel2_is_coal fuel1_is_coal fgd_cost_total_by_plt_tot fgd_tot_cost_structure_by_boiler ///
  fgd_tot_cost_disposal_by_boiler fgd_tot_cost_other_by_boiler fgd_tot_expend_feed_by_boiler ///
  fgd_tot_cost_total_by_boiler fgd_tot_expend_labor_by_boiler fgd_tot_expend_disposal_by_boile ///
  fgd_tot_expend_other_by_boiler fgd_tot_expend_total_by_boiler reg_so2_ppm fgd_costinc5yr fgd_costinc10yr ///
  fgd_costinc3yr fgd_cost10yr fgd_cost5yr fgd_cost3yr fgd_costinc1yr ann_heat_content ///
  so2_std_percent_scrubb efficiency_50_pct_load

destring inservice_y, replace force
destring reg_so2_lbs_mmbtu, gen(leg_lbs_mmbtu) force
destring reg_so2_lbs_mmbtu_calc, gen(leg_lbs_mmbtu_calc) force
destring reg_fuel_content, gen(leg_fuel_content) force
destring reg_lbs_so2_hr, gen(leg_lbs_so2_hr) force
destring hours_under_load, gen(hr_load) force
destring ann_sulfur_content, replace force 
destring retirement_y, replace force
destring efficiency_100_pct_load, replace force
destring utility_flag, replace force
destring ifnew_so2_lbs_mmbtu_calc, replace force
replace ifnew_so2_lbs_mmbtu_calc=1.2 if ifnew_so2_lbs_mmbtu_calc>1.2
destring plant_capacity, replace force
destring so2_nonattain, replace force
destring nsr_first_permit_y, replace force
*destring(pre_1978), gen(pre_NSR) force
destring max_boi_nameplate, replace force
rename max_boi_nameplate capacity
encode plt_state, gen(states)

replace  utility_flag=0 if utility_flag==.
gen cny=substr(plt_county , 1, 1)+ lower(substr(plt_county , 2, .))
drop plt_county
rename cny plt_county

** Fix missing age where possible
bys plant_code boiler_id: egen start_y=min(inservice_y)
rename age_boiler age
replace age=year-start_y if age==.
replace inservice_y=start_y if inservice_y==.

** For some reason, the capacity we have from EIA seems to be inflated, with 
** individual boilers being over 3GW when comparing with capacity data from the 
** EIA-EPA crosswalk, it seems like the inflation factor is 3.34
replace capacity=capacity/3.34

** Efficiency measure is rescaled from 2012
replace efficiency_100_pct_load=efficiency_100_pct_load*100  if year>2011
replace efficiency_100_pct_load=. if efficiency_100_pct_load==0 
*correlation between the two measures around 77% - ignore the 50%

** Some nonsensical values assigned to retirement_y variable
replace retirement_y=2016 if retirement_y<1916
replace retirement_y=2030 if retirement_y==.

** Some problems with the 2006 regulation (wrong matching? too much 9.09)
sort plant_code boiler_id year

replace leg_lbs_mmbtu_calc=leg_lbs_mmbtu_calc[_n-1] ///
  if leg_lbs_mmbtu_calc>9.09 & year==2006 ///
  & leg_lbs_mmbtu_calc[_n-1]==leg_lbs_mmbtu_calc[_n+1] ///
  & year[_n+1]==2007 & year[_n-1]==2005
replace leg_lbs_mmbtu_calc=leg_lbs_mmbtu if leg_lbs_mmbtu_calc==. & leg_lbs_mmbtu!=.
replace leg_lbs_mmbtu_calc=leg_lbs_mmbtu_calc[_n-1] ///
  if year==2006 & leg_lbs_mmbtu_calc==. ///
  & leg_lbs_mmbtu_calc[_n-1]!=leg_lbs_mmbtu_calc[_n+1] ///
  & year[_n+1]==2007 & year[_n-1]==2005

** Generate survivial/new indicator
sort plant_code boiler_id year
gen survive=1 if (plant_code[_n+1]==plant_code[_n]&boiler_id[_n+1]==boiler_id[_n])
replace survive=0 if (plant_code[_n+1]!=plant_code[_n]| boiler_id[_n+1]!=boiler_id[_n])
gen closed=1 if  retirement_y<=year & plt_anncon=="NA"
replace survive=0 if (closed[_n+1]==1 & plant_code[_n+1]==plant_code[_n]& boiler_id[_n+1]==boiler_id[_n])
** Some boilers are in the data even though they seem to be decomissioned based on the retirement_y
drop if closed==1
gen new=((plant_code[_n]!=plant_code[_n-1]| boiler_id[_n]!=boiler_id[_n-1]) & year !=1985)

** Add electricity demand by state and utility data
joinby year plt_state using "$path_data\electricity_demand_by_state.dta", unmatched(master)
drop _merge

destring utility_code, replace force
joinby utility_code year using "$path_data\utility_data.dta", unmatched(both)
** Some 3.5 ths obs do not have utility code
** NB:  36 ths obs not matched to the code but the file only for years above 2013. 
**      For those years - only 86 unmatched the EIA 860 data has, on the other hand, 
**      21 ths obs that are not matched to our data - could be owners of gas, solar, etc.
drop if _merge==2
drop _merge 
sort utility_code year util_entity_type 
replace util_entity_type=util_entity_type[_n+1] ///
  if util_entity_type=="" ///
  & util_entity_type[_n+1]!="" ///
  & utility_code==utility_code[_n+1] ///
  & (year==year[_n+1]-1|year==year[_n+1])

** See whether we have utilities that change type:
bys utility_code util_entity_type: gen nvals = _n == 1
bys utility_code: egen no_types=total(nvals)
tab no_types
** We do have some utilities changing type once.
** Fill out the utilities with constant type
sort utility_code year
bys utility_code: gen type=util_entity_type[_N] 
replace type=util_entity_type if (type!=util_entity_type & no_types!=2)
** Fill out the ones that change
gsort utility_code -year
replace type=type[_n-1] if (type=="" & no_types==3& utility_code==utility_code[_n-1])
encode type, gen(ut_type)

** For boilers that do not switch type we fill out ut_type 
tabulate ut_type, generate(ut_type_aux)
gen commerc_ind_aux=(ut_type_aux5==1 |ut_type_aux2==1)
bys plant_code boiler_id: egen commerc_ind=max(commerc_ind_aux)
replace ut_type_aux4=0 if ut_type_aux4==.
bys  plant_code boiler_id: egen IOU=max(ut_type_aux4) 
egen other_aux=rowtotal(ut_type_aux9 ut_type_aux8 ut_type_aux7 ut_type_aux6 ut_type_aux3 ut_type_aux1 ) 
bys plant_code boiler_id: egen other=max(other_aux)
gen type_check=commerc_ind+IOU+other
sort plant_code boiler_id year
replace ut_type=ut_type[_n-1] if ut_type==. ///
  & ut_type[_n-1]!=. & plant_code==plant_code[_n-1] ///
  & boiler_id==boiler_id[_n-1]  & type_check==1
 
** Manually clean some boilers 
**  - Sandow Power Plant - 52071 used for aluminium smelting facility
**  - Allete Inc - utility_code 281 is a utility
**  - Warrick plant - 6705 - supplies energy for aluminium smelter
replace ut_type=5 if plant_code==6639 & ut_type==.
replace ut_type=5 if plant_code==6823 & ut_type==. 
replace ut_type=5 if plant_code== 50397 & ut_type==.
replace ut_type=5 if plant_code== 50407 & ut_type==.
replace ut_type=5 if plant_code== 50481 & ut_type==.
replace ut_type=5 if plant_code== 57919 & ut_type==.
replace ut_type=5 if plant_code==52071 &  year!=2010
replace ut_type=4 if utility_code==281 
replace ut_type=5 if plant_code==6705 
 
** Unclear gf status
drop if  plant_code==10495
drop ut_type_aux* commerc_ind_aux commerc_ind IOU type_check other_aux other ///
  util_entity_type no_types nvals type

joinby year plant_code using "$path_data\nonutilityData.dta", unmatched(master)
** All merged observations have no utility type
replace ut_type=Sector if _merge==3 & ut_type==.
drop _merge Sector

** Generate NSR & NSPS grandfathering indicators
** NB:  NSR is for boilers for which construction began after September 18, 1978 
**      and above 73MW. Pre_NSR gathers hand-picked data on the beginning of 
**      construction for boilers where the inservice date is relatively shortly 
**      after 1978.
order plant_code boiler_id year inservice_y type_of_boiler nsr_d so2_unit so2_std_rate ///
  nsr_permit_y nsr_first_permit_y nsr_permit_num reg_so2_lbs_mmbtu leg_lbs_mmbtu ///
  so2_regulation so2_std_rate inservice_m inservice_y reported_so2_lbs_mmbtu ///
  leg_lbs_mmbtu_calc leg_lbs_mmbtu_calc

** NSR is for bigger boilers but some boilers don't have capacity info in some 
** years. Below we are dealing with most cases where that info is missing 
bys plant_code boiler_id: egen year_earliest_in_sample=min(year)
bys plant_code boiler_id: egen min_capacity=min(capacity)
gen capacity_at_begin= capacity if year==year_earliest_in_sample
replace capacity_at_begin=min_capacity if capacity_at_begin==.
replace capacity_at_begin=capacity_at_begin[_n-1] ///
  if (capacity_at_begin==. & plant_code==plant_code[_n-1] & boiler_id==boiler_id[_n-1])

** Generate the time-invariant grandfathering status
** Import data from Aaron with boiler classification around 1978
joinby plant_code boiler_id using "$path_data\gf_status_born_around_1978.dta", unmatched(master)
** The rules for NSR were initially introduced only for boilers operated "for the 
** purpose of supplying more than one-third of its potential electric output capacity 
** and more than 25 MW net-electrical output to any utility power distribution 
** system for sale" (40 CFR 60.41Da).
** Update for separate rules for commercial and industrial boilers (subpart Db) - 
** boilers greater than 100MMBtu that were also built after 1984 (e.g. 
** grandfathering applies). (40 CFR 60.40Db et seq.)
** That industrial-commercial-institutional boilers built before 1984 but after 
** 1971 that are over 250MMBtu are still subject to the NSPS standard of 
** 1.2 lbs/MMBtu (Subpart D). 
** A separate set of rules for small industrial-commercial-institutional boilers 
** (greater than 10MMBtu but less than 100MMBtu) where construction commenced after 
** June 9, 1989 (again, grandfathering applies).
** These regulations include a 90% reduction standard and a performance standard 
** of 0.20 lb/MMBtu.
** Using typical boiler and generating efficiencies, 100 MMBtu/hr heat input is 
** equal to about 10 MW electric output.
**  https://www.energy.gov/sites/prod/files/2013/11/f4/characterization_industrial_commerical_boiler_population.pdf
** Types of boilers in our data:
** C - co-op (1), Q - Independent Power Producer (8), I- Investor owned utility 
** (4), P- political (7), F - fed (3), S- state (9). Ind-industrial 
** (5), Com - commercial (2), M - municipial (6)
gen Gf=(start_y<1981 | capacity_at_begin<=73)
replace Gf=1 if (pre_1978==1)
replace Gf=1 if ((start_y<1987 | capacity_at_begin<=29) & (ut_type==5|ut_type==2))
replace Gf=0 if ((start_y>1987 & capacity_at_begin>29) & (ut_type==5|ut_type==2))
replace Gf=0 if ((start_y>1991 & capacity_at_begin>3) & (ut_type==5|ut_type==2))

** Generate alternative grandfathering status for the "conservative" robustness check
gen GfA=(start_y<=1978 | capacity_at_begin<=73)
replace GfA=1 if ((start_y<=1986 | capacity_at_begin<=29) & (ut_type==5|ut_type==2))
replace GfA=0 if ((start_y>1986 & capacity_at_begin>29) & (ut_type==5|ut_type==2))
replace GfA=0 if ((start_y>1989 & capacity_at_begin>3) & (ut_type==5|ut_type==2))

** Generate applicable local & NSPS regulations (we allow NSPS status to change over time)
gen grand_NSPS=(((type_of_boiler=="N"|type_of_boiler=="NA") & start_y<1973) | capacity_at_begin<75)
replace grand_NSPS=1 if (year>1984 & start_y>1985 & capacity_at_begin>30 & (ut_type==5|ut_type==2))
gen applic_reg=leg_lbs_mmbtu_calc
replace applic_reg=1.2 if grand_NSPS==0 & leg_lbs_mmbtu_calc>1.2
replace applic_reg=1/applic_reg
drop if applic_reg>15

** Generate cohorts for anticipation effects 
gen pre_1963=(start_y<1963)
gen post_1963_Gf=(pre_1963==0 & Gf==1)
drop capacity_at_begin year_earliest_in_sample _merge

** Merge coal data and ARP prices
joinby year plant_code using "$path_data\coal_trades_data.dta", unmatched(master)
drop _merge fuel_cost_* heat_content_* quantity_received_*
destring sulfur_content_tot, replace force

joinby year using "$path_data\ARP_prices.dta", unmatched(master)

** CAIR required that 2 permits required for one unit of emissions. Thus, double 
** prices *in SO2-CAIR subject states:
** In Alabama, District of Columbia, Florida, Georgia, Illinois, Indiana, Iowa, 
** Kentucky, Louisiana, Maryland, Michigan, Minnesota, Mississippi, Missouri, 
** New York, North Carolina, Ohio, Pennsylvania, South Carolina, Tennessee, Texas, 
** Virginia, West Virginia, and Wisconsin
rename ARPprice_sp ARPprice
replace ARPprice=2*ARPprice if (year>2009 & (states==1 | states==7 ///
  |states==8 | states==10 | states==11|states==12|states==14|states==15|states==16 ///
  |states==18|states==19|states==20|states==21|states==28|states==29 ///
  |states==34|states==36|states==37|states==39|states==41|states==42))
drop _merge 

joinby plant_code year  using "$path_data\ARP_PhaseI.dta", unmatched(master)
replace ARP_subject=1 if (year>1999 & (capacity>25 | new==1))
replace ARP_subject=0 if ARP_subject==.
replace ARPprice=0 if ARP_subject==0
drop _merge

sort plant_code boiler_id year
drop if plant_code==plant_code[_n+1] & boiler_id==boiler_id[_n+1] & year==year[_n+1]

** Merge with state-level capacity data
joinby year plt_state using "$path_data\State_capacity.dta" , unmatched(master)
drop _merge

** Merge with CEMS yearly data 
joinby year plant_code boiler_id using "$path_data\cems_yearly.dta", unmatched(master)
drop _merge
gen SO2=SO2_MASS/(DURATION*capacity)

** Merge with coal to gas price data
joinby year plt_state using "$path_data\coal2gasPrice.dta", unmatched(master)
drop _merge

** Merge with data on low sulfur coal availability
** NB: sulfur_net - sulfur content of coal weighted by distance over the network 
joinby plant_code using "$path_data\sulfur_iv.dta", unmatched(master)
drop _merge
destring sulfur_net, generate(sulfur_net_iv) force
drop sulfur_netlow sulfur_net

** Generate additional variables for regressions anticipation, boiler manuf, etc.
gen applic_reg_Gf=applic_reg*Gf
gen so2_nonat_Gf = Gf*so2_nonattain
gen capacity_pre_1963=capacity*pre_1963 
gen capacity_post_1963_GF=capacity*post_1963_Gf
gen applic_reg_pre1963=applic_reg*pre_1963
gen applic_reg_post1963GF=applic_reg*post_1963_Gf
gen pre1963_in_nonnat_GF = pre_1963*so2_nonattain
gen post1963_GF_in_nonnat_const = so2_nonattain*post_1963_Gf
gen so2_nonat_GfA = GfA*so2_nonattain
gen applic_reg_GfA=applic_reg*GfA
gen capacity_gfA=GfA*capacity
gen capacity_gf=capacity*Gf
gen age_sq=age^2
gen capacity_sq=capacity^2

*egen plant_boiler = concat(plant_code boiler_id)
egen county_code = concat(fips_st  fips_cnty), punct(_)
egen ID=concat(plant_code boiler_id), punct(-)
encode boiler_manu_clean, generate(manufact)

drop if (year<1985| year==2006)
drop cap_stateGeothermal cap_stateHydro cap_stateOther SLOAD Summer_Cap Cogenerator ///
  sulfur_content_10 sulfur_content_9 sulfur_content_8 sulfur_content_7 ///
  so2_proposed_ctrl3  so2_proposed_ctrl2 nox_existing_ctrl1 nox_existing_ctrl2 ///
  nox_existing_ctrl3 nox_proposed_ctrl1 alt_fuel_code1 plant_steam_flow ///
  utility_steam_flow fgd_cost_other_by_plt_min fgd_cost_other_by_plt_tot ///
  fgd_cost_total_by_plt_min fgd_cost_disposal_by_plt_tot fgd_cost_disposal_by_plt_min ///
  fgd_tot_sorbent_quantity_by_boil fgd_avg_sorbent_quantity_by_boil ///
  fgd_avg_eff_sulfur_factor_by_boi fgd_trains_tot_by_boiler gen_deliver_power_transgrid ///
  gen_cogen gen_planned_mod gen_planned_uprates_net_summer_c gen_planned_uprates_y ///
  gen_planned_derates_net_summer_c gen_planned_derates_y gen_uprate_derate_completed_y ///
  mod_db_in_y mod_da_in_y mod_d_in_y mod_db year_db year_da mod_da year_mod_fed ///
  mod_federal mod_fed_in_y pltanngen10yr fgd_cost_structure_by_plt_min ///
  fgd_cost_structure_by_plt_tot num_fgds_to_boiler low_nox_process1 nox_ctrl_status ///
  reg_masked_plt_county min_boi_nameplate plt_city exp_fgd_disposal byprod_rev_fgd ///
  cogen_type cogen_useful_thermal coal_heat_yr_plus_10 pm_regulation nox_regulation ///
  NOX_MASS coal_heat_yr_plus_5 coal_sulfur_yr_plus_5 coal_sulfur_yr_plus_10 ///
  utility_flag97_00 plt_anngen ifnew_lbs_so2_hr ifnew_calc_plt_lbs_so2_hr ///
  ifnew_linked_state_reg plt_zip plt_fuel_bit so2_existing_ctrl3 ///
  so2_existing_ctrl2 so2_ctrl_noncomp3 so2_ctrl_noncomp2 primary_fuel5 ///
  primary_fuel4 primary_fuel3 primary_fuel2 so2_proposed_ctrl1 nsr_permit_m ///
  steam_plt_type plt_inservice_m fgd_avg_energy_consump_kwh_by_bo ///
  fgd_tot_energy_consump_kwh_by_bo fgd_avg_eff_sulfur_at_100_by_boi /// 
  fgd_tot_so2_emission_rate_by_boi fgd_earliest_inservice_by_boiler ///
  gen_summer_cap_mmbtu other_fire_steam_flow retirement_m plt_anncon plt_annstk ///
  plt_fuel_col boiler_geninc boiler_quantinc3yr boiler_quantinc1y ann_sulfur_content10yr ///
  ann_sulfur_inc10yr ann_sulfur_inc5yr ann_sulfur_content5yr sulfur_content_6 ///
  sulfur_content_5 so2_unit reported_so2_lbs_mmbtu leg_lbs_mmbtu_calc
  
*drop Hawaii
drop if plant_code==10673
  
order year plt_state plant_code boiler_id new age retirement_y plant_status_code d_growth Gf 

** As we have only very few boilers in Oregon, South Dakota, Rhode Island,
** Washington, Maine, Louisiana, North Dakota, Hawaii, Arizona, Arkansas
** and we use state FE and boiler FE, we group these states into one category
** used "tab manuf* year if e(sample)" to check that
replace states=99 if states==33|states==35| states==42 | states==16 | states==15 ///
  | states==31 | states==9 | states==2| states==3 | states==37

** As we have limited commercial boilers in our sample and we use owner type FE 
** and boiler FE, we merge commercial boilers into industrial
** C = Cooperative, I = Investor-Owned Utility, Q = Independent Power Producer, 
** M = Municipally-Owned Utility, P = Political Subdivision, F = Federally-Owned Utility, 
** S = State-Owned Utility, IND = Industrial, COM = Commercial
replace ut_type=5 if ut_type==2

** As we have only very few boilers of certain manufacturers and we use 
** manufacturer FE and boiler FE in some regressions, we merge these seldom 
** manufacturers into one group
replace manufact=99 if manufact==1|manufact==2|manufact==5|manufact==6 ///
  |manufact==8|manufact==9|manufact==10|manufact==11|manufact==12|manufact==13 ///
  |manufact==15|manufact==16|manufact==18|manufact==19|manufact==20|manufact==21

** Generate cutoff for local regulations removing GF privilege.
** Assume that regulations requiring emissions less than 0.9 lbs/MMBtu effectively 
** require scrubber
gen applic_regT=(applic_reg>1.11)
gen applic_regT2=(applic_reg>1.21)

** Label variables for outreg2 command
label variable pre_1963  "Pre 1963 (GF)"
label variable post_1963_Gf "Post 1963 (GF)"
label variable post_1963_Gf "NAAQS"
label variable post_1963_Gf "MMBTU"
label variable applic_reg_pre1963 "Pre 1963 (GF) x MMBTU"
label variable applic_reg_post1963GF "Post 1963 (GF) x MMBTU"
label variable pre1963_in_nonnat_GF "Pre 1963 (GF) x NAAQS"
label variable post1963_GF_in_nonnat_const "Pre 1963 (GF) x NAAQS"
label variable GfA "GF altern"
label variable so2_nonat_GfA "GF altern x NAAQS"
label variable applic_reg_GfA "GF altern x MMBTU"
label variable capacity_gfA "GF altern x capacity"


** Export -----------------------------------------------------------------------------------------

** Save the file ready for processing
save "$path_data\regressions_ready_data.dta", replace


*** END CODE ***

