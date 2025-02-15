#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create universe from 1970-present                 #
# Author: Bridget Pals                              #
# Date: 2/1/2019 edited 12/30/2023                  #
# Purpose: Combine EIA-767, EIA-923, and EIA-869 to #
# get location, heat rate, nameplate, and scrubber  #
# information about coal plants.                    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# libraries needed
library(dplyr)    # basic data cleaning
library(readxl)   # read in excel files
#library(zoo)      # to fill in NAs for county/state

#setwd("~/Documents/research/research_revesz")

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

mysum <- function(x) (sum(as.numeric(x), na.rm = TRUE))

mymean <- function(x) (mean(as.numeric(x), na.rm = TRUE))

months <- c("january", "february", "march", "april", "may", "june", "july", "august",
            "september", "october", "november", "december")

shortmonths <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

monthnum <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

coal <- c("COL", "BIT", "LIG", "SUB", "ANT", "RC", "WC")

coal_full <- data.frame()

files <- c(paste0(1971:1995,"u"), paste0(1996:2000, "mu"))

for (i in files) {
  
  print("***********************************************")
  print(paste("--------------------", i, "--------------------"))
  print("***********************************************")
  
  # https://www.eia.gov/electricity/data/eia923/F759layout_um.txt
  
  allplts <- read_excel(paste0("orig_data/eia_923/f759",i,".xls"), col_types = "text")
  
  names(allplts)  
  print("# Plants by FUELDESC")
  
  print(table(allplts$FUELDESC))
  
  print(table(allplts$EFFDATE))
  print(table(allplts$YEAR))
    
  ## until 1995, essentially no plants are missing FUELDESC. Starting in
  ## 1996, hunderds are missing each year. We will check number of unique
  ## plants, by year, to see the extent to which this is impacting our universe
  print(paste("# Plants missing FUELDESC - ", sum(is.na(allplts$FUELDESC))))
  
  # print(table(allplts$FUELNM))
  # print(paste("# PLANTS missing FUELNM - ", sum(is.na(allplts$FUELNM))))
  # 
  # print(table(allplts$FUELTYP))
  # print(paste("# PLANTS missing FUELTYP - ", sum(is.na(allplts$FUELTYP))))
  
  # limit only to plants with a coal fueltype
  coal_sub <- allplts %>%
    filter(FUELDESC %in% coal) 
  
  names(coal_sub) <- tolower(names(coal_sub)) 
  
  # it appears the data may be at a boiler level, given that utility, plant,
  # fuel, and capacity give a unique identifier, almost universally.
  
  # there are occasional facilities that are not uniquely identified by
  # these values. For those, we will sum the numeric vars so that they
  # are at the boiler level, as well.
  print(nrow(coal_sub %>% unique_id(pcode, cocode, fueldesc, capacity)))
  
  nums <- c("01", "02", "03", "04", "05", "06", 
            "07", "08", "09", "10", "11", "12")
  num_vars <- c(paste0("gen", nums), paste0("con", nums), paste0("stk", nums))
  
  coal_sub <- coal_sub %>%
    group_by(pcode, cocode, fipst, utilcode, fueldesc, capacity) %>%
    summarize_at(num_vars, mysum)
  
  print(nrow(coal_sub %>% unique_id(pcode, utilcode, fueldesc, capacity)))
  
  ## note: the capacity variable is very often blank or equal to zero.
  ## For now, I am preserving the capacity variable, especially as I think
  ## it may be tied either to a boiler or a generator (data may not be at
  ## plant level, after all)
  
  ## updated note: Sylwia has a colleague with access to a (stable) capacity variable
  ## it makes more sense to depend on that, rather that to try to piece it together
  
  coal_sub <- coal_sub %>%
    ## create variable for annual generation and a year variable
    mutate(plt_anngen = sum(gen01, gen02, gen03, gen04, gen05, gen06,
             gen07, gen08, gen09, gen10, gen11, gen12, na.rm = TRUE),
           plt_annstk = sum(stk01, stk02, stk03, stk04, stk05, stk06,
                        stk07, stk08, stk09, stk10, stk11, stk12, na.rm = TRUE),
           plt_anncon = sum(con01, con02, con03, con04, con05, con06,
                        con07, con08, con09, con10, con11, con12, na.rm = TRUE),
           ## the effdate of the 1995 dataset is 1994, (same for the years prior with an effdate) 
           ## so we should set the year as the date before the report year, presumably
           year = (as.numeric(substr(i,1,4))-1)) %>%
    rename(plant_code = pcode, company_code = cocode, state = fipst, utility_code = utilcode)
  
  coal_full <- bind_rows(coal_full, coal_sub)
  
}

## we check there aren't any wild variances year to year - there appears to be
## a relatively constant decline
table(coal_full$year)

## our data is currently at the plant/fuel-type level. We collapse to the plant level

## we want to preserve the amount of different types of coal present, just in caes
## it's useful later
early_plants <- coal_full %>% 
  mutate(fuel_col = ifelse(fueldesc == "COL", plt_anncon, 0),
         fuel_bit = ifelse(fueldesc == "BIT", plt_anncon, 0),
         fuel_lig = ifelse(fueldesc == "LIG", plt_anncon, 0),
         fuel_sc = ifelse(fueldesc == "SC", plt_anncon, 0),
         fuel_sub = ifelse(fueldesc == "SUB", plt_anncon, 0),
         fuel_wc = ifelse(fueldesc == "WC", plt_anncon, 0)) %>%
  select(-fueldesc)

num_vars <- c(num_vars, "plt_anngen", "plt_anncon", "plt_annstk", 
              grep("fuel_", names(early_plants), value = T))

## the .txt file explaining the dataset explains that within each state the
## company code and plant code uniquely identifies plants
early_plants <- early_plants %>%
  filter(plant_code != 9999) %>%
  group_by(plant_code, company_code, state, year) %>%
  summarize_at(num_vars, mysum) %>%
  ungroup() 

table(early_plants$year)

early_plants %>% unique_id(plant_code, state, company_code, year)

write.csv(early_plants, "use_data/plants_fuel_1970_2000")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-- 2001-2018 -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

coal_full <- data.frame()
stocks_full <- data.frame()

## file names vary slightly by year, but format remains same over these periods
files <- c("y2001", "y2002", "_2003", "_2004", "_2005", "_2006", "_2007",
           "eia923December2008.xls", "EIA923 SCHEDULES 2_3_4_5 M Final 2009 REVISED 05252011.xls",
           "EIA923 SCHEDULES 2_3_4_5 Final 2010.xls","EIA923_Schedules_2_3_4_5_2011_Final_Revision.xlsx",
           "EIA923_Schedules_2_3_4_5_M_12_2012_Final_Revision.xlsx", "EIA923_Schedules_2_3_4_5_2013_Final_Revision.xlsx",
           "EIA923_Schedules_2_3_4_5_M_12_2014_Final_Revision.xlsx", "EIA923_Schedules_2_3_4_5_M_12_2015_Final_Revision.xlsx",
           "EIA923_Schedules_2_3_4_5_M_12_2016_Final_Revision.xlsx", "EIA923_Schedules_2_3_4_5_M_12_2017_Final_Revision.xlsx",
           "EIA923_Schedules_2_3_4_5_M_12_2018_Final_Revision.xlsx")

years <- c(2001:2018)

for (i in c(1:18)) {
  
  print("***********************************************")
  print(paste("------------------ Coal", years[i], "------------------"))
  print("***********************************************")
  
  # Additional background on datasets available here:
  # https://www.eia.gov/electricity/data/guide/pdf/guide.pdf, page 4
  
  if (years[i] %in% c(2001:2007)) {
    allplts <- read_excel(paste0("orig_data/eia_923/f906920_", years[i],
                                 "/f906920", files[i], ".xls"), sheet = 1, skip = 7)
  } else if (years[i] %in% c(2008:2010)) {
    allplts <- read_excel(paste0("orig_data/eia_923/f923_", years[i], "/", files[i]), 
                          sheet = 1, skip = 7)
  } else {
    allplts <- read_excel(paste0("orig_data/eia_923/f923_", years[i], "/", files[i]), 
                          sheet = 1, skip = 5)
  }
  
  # certain years have these characters randomly inserted in certain var names
  names(allplts) <- gsub("\r\n", " ", names(allplts))
  
  # we have different capitalization conventions each year, so, for ease, we bring all to
  # lowercase
  names(allplts) <- tolower(names(allplts))
  
  print(table(allplts$`reported fuel type code`))
  
  print(paste("# Plants missing FUELDESC - ", sum(is.na(allplts$`reported fuel type code`))))
  
  allplts %>% filter(`plant id` == "99999") %>% select(`plant id`, `plant name`) %>% unique()
  
  # limit only to plants with a coal fueltype and those not coded "99999" in Plant ID (which
  # is just state-level statistics)
  coal_sub <- allplts %>%
    filter(`reported fuel type code` %in% coal) %>%
    filter(`plant id` != "99999")
  
  orig_rows <- nrow(coal_sub)
  print(orig_rows)
  
  ## let's do some due diligence on the identifying level
  ## Plant ID and Fuel description does not quite identify the data.
  print(nrow(coal_sub %>% unique_id(`plant id`, `reported fuel type code`)))
  
  ## if we add in EIA sector number - which defines as utility or non-utility - and the
  ## reported prime mover (e.g. what type of turbine the boiler is driving) we get
  ## full identification
  print(coal_sub %>% unique_id(`plant id`, `reported fuel type code`, `eia sector number`, `reported prime mover`))
  ## in 2017, there are two duplicates - I think we can add them without catastrophe

  coal_sub <- coal_sub %>%
    rename(plant_id = 'plant id', fueldesc = 'reported fuel type code')
  
  ## For our purposes, we can combine the different EIA sector data and prime mover data
  ## as we are primarily concerned with the total emissions (we don't really care where it's 
  ## going), so we can sum all of the variables
  
  ## first, we cross-identify vars from the 1970-2001 datasets, and here
  ## from our data documentation, we know the "genXX" vars referred to net generation
  for (m in c(1:12)) {
    names(coal_sub) <- gsub(months[m], monthnum[m], names(coal_sub))
    names(coal_sub) <- gsub(shortmonths[m], monthnum[m], names(coal_sub))
  }
  
  ## The net generation vars are equivalent to the "gen" vars in earlier years
  names(coal_sub) <- gsub("netgen", "gen", names(coal_sub))
  
  ## the "quantity" var refers to the total quantity consumed, which aligns with the "con"
  ## var of earlier years.
  names(coal_sub) <- gsub("quantity", "con", names(coal_sub))
  
  ## we remove spaces from varnames
  names(coal_sub) <- gsub(" ", "", names(coal_sub))
  names(coal_sub) <- gsub("gen_", "gen", names(coal_sub))
  names(coal_sub) <- gsub("con_", "con", names(coal_sub))
  
  keepvars <- c(paste0("gen", monthnum), paste0("con", monthnum))
  
  ## everything is in consistent units so we can add up rows!
  ## n.b. according to the data documentation, unit options are 
  ## tons, barrels, and mcf, so we do not need to worry about a
  ## tons/short tons distinction
  print(table(coal_sub$`physicalunitlabel`))

  print(table(coal_sub$year))
  sum(is.na(coal_sub$year))
  
  coal_sub <- coal_sub %>%
    select(plant_id, fueldesc, all_of(keepvars)) %>%
    group_by(plant_id, fueldesc) %>%
    ## we assume NA = 0 for the purpose of summing together these variables
    summarize_at(keepvars, mysum) %>%
    select(plant_id, fueldesc, keepvars) %>%
    unique() %>%
    mutate(plt_anngen = sum(gen01, gen02, gen03, gen04, gen05, gen06,
                        gen07, gen08, gen09, gen10, gen11, gen12, na.rm = TRUE),
           plt_anncon = sum(con01, con02, con03, con04, con05, con06,
                        con07, con08, con09, con10, con11, con12, na.rm = TRUE)) %>%
    mutate(fuel_col = ifelse(fueldesc == "COL", plt_anncon, 0),
           fuel_bit = ifelse(fueldesc == "BIT", plt_anncon, 0),
           fuel_lig = ifelse(fueldesc == "LIG", plt_anncon, 0),
           fuel_sc = ifelse(fueldesc == "SC", plt_anncon, 0),
           fuel_sub = ifelse(fueldesc == "SUB", plt_anncon, 0),
           fuel_wc = ifelse(fueldesc == "WC", plt_anncon, 0)) %>%
    select(-fueldesc)
  
  keepvars <- c(keepvars, "plt_anngen", "plt_anncon", 
                grep("fuel_", names(coal_sub), value = T))
  
  coal_sub <- coal_sub %>%
    group_by(plant_id) %>%
    mutate_at(keepvars, mysum) %>% ungroup() %>% unique()
  
  coal_sub %>% unique_id(plant_id)
  
  ## because through year 2000 both an effective date and year is listed and the effective date
  ## is the end of the prior calendar year, we assume this continues to be the case and the data
  ## comes from the year prior
  coal_sub <- mutate(coal_sub, year = years[i]-1)
  
  print(table(coal_sub$year))
  
  coal_full <- bind_rows(coal_full, coal_sub)
  
}

## now we add in the "stocks" variables, which are on a different
## tab in the spreadsheet. Stocks data begins being collected only
## in 2002.
for (i in c(2:18)) {
  
  print("***********************************************")
  print(paste("----------------- Stocks", years[i], "-----------------"))
  print("***********************************************")
  
  ## we also want to add in our stock variables. There is no such data in 2001.
  if (years[i] %in% c(2002:2007)) {
    stocks <- read_excel(paste0("orig_data/eia_923/f906920_", years[i],
                                "/f906920", files[i], ".xls"), sheet = 4, skip = 5)
  } else if (years[i] %in% c(2008:2010)) {
    stocks <- read_excel(paste0("orig_data/eia_923/f923_", years[i], "/", files[i]), 
                         sheet = 4, skip = 5)
  } else if (years[i] %in% c(2011:2016)) {
    stocks <- read_excel(paste0("orig_data/eia_923/f923_", years[i], "/", files[i]), 
                         sheet = 4, skip = 5)
  } else if (years[i] == 2017) {
    stocks <- read_excel(paste0("orig_data/eia_923/f923_", years[i], "/", files[i]), 
                         sheet = 5, skip = 5)
  } else if (years[i] == 2018) {
    stocks <- read_excel(paste0("orig_data/eia_923/f923_", years[i], "/", files[i]), 
                         sheet = 6, skip = 5)
  }  
  
  names(stocks)
  names(stocks) <- tolower(names(stocks))
  names(stocks) <- gsub("\r\n", " ", names(stocks))
  names(stocks) <- gsub("\n", " ", names(stocks))
  
  stocks <- stocks %>% rename(plant_id = 'plant id', plant_name = 'plant name')
  
  for (m in c(1:12)) {
    names(stocks) <- gsub(months[m], monthnum[m], names(stocks))
  }
  
  names(stocks) <- gsub("quantity", "stk", names(stocks))
  stocks <- rename(stocks, fueldesc = 'reported fuel type code')
  
  stocks %>% filter(plant_id == "999999") %>% select(plant_id, plant_name) %>% unique()
  
  stocks <- stocks %>%
    filter(fueldesc %in% coal) %>%
    filter(plant_id != "999999")
  
  print(stocks %>% unique_id(plant_id, fueldesc))
  ## there are only 4 plant-year obs not uniquely identified by this - I am going to
  ## assume it is safe to simply aggregate those duplicates.
  
  ## since everything is in the same units, we can add it up!
  print(table(stocks$`physical unit label`))
  
  stk_vars <- paste0("stk", monthnum)
  
  names(stocks) <- gsub(" ", "", names(stocks))
  
  ## we just add up to the plant id var. We've used consumption - not stocks - to retain
  ## the fuel type information, so we do not need to worry about keeping fuel-specific
  ## stock data
  stocks <- stocks %>%
    select(plant_id, stk_vars) %>%
    group_by(plant_id) %>%
    summarize_at(stk_vars, mysum) %>%
    mutate(plt_annstk = sum(stk01, stk02, stk03, stk04, stk05, stk06,
                        stk07, stk08, stk09, stk10, stk11, stk12, na.rm = TRUE))
  
  stocks %>% unique_id(plant_id)
  
  ## effective date is reporting from year prior
  stocks <- stocks %>% mutate(year = (years[i]-1))
  
  stocks_full <- bind_rows(stocks, stocks_full)
  
}

stocks_full %>% unique_id(plant_id, year)
coal_full %>% unique_id(plant_id, year)

coal_full <- left_join(coal_full, stocks_full)

coal_full %>% unique_id(plant_id, year)

coal_full <- coal_full %>% 
  mutate(plant_code = as.character(plant_id)) %>% select(-plant_id)

early_plants %>% unique_id(plant_code, year)

names(early_plants)
names(coal_full)

## we keep only the annual metrics - the monthly aren't going to get us anything
## and they're super messy
all_fuel <- bind_rows(early_plants, coal_full) %>%
  select(plant_code, year, plt_anngen, plt_anncon, plt_annstk,
         fuel_col, fuel_bit, fuel_lig, fuel_sc, fuel_sub, fuel_wc) %>%
  mutate(fuel_col = sum(fuel_col, fuel_bit, fuel_lig, fuel_sc, fuel_sub, fuel_wc, na.rm = TRUE))

all_fuel %>% unique_id(plant_code, year)

names(all_fuel) <- gsub("fuel_", "plt_fuel_", names(all_fuel))

nrow(all_fuel)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#-- Add in Fuel Comp Info: 1985-2005 --------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fuel_85_18 <- data.frame()

## file names vary slightly by year, but format remains same over these periods
files <- c("eia923December2008.xls", "EIA923 SCHEDULES 2_3_4_5 M Final 2009 REVISED 05252011.xls",
           "EIA923 SCHEDULES 2_3_4_5 Final 2010.xls","EIA923_Schedules_2_3_4_5_2011_Final_Revision.xlsx",
           "EIA923_Schedules_2_3_4_5_M_12_2012_Final_Revision.xlsx", "EIA923_Schedules_2_3_4_5_2013_Final_Revision.xlsx",
           "EIA923_Schedules_2_3_4_5_M_12_2014_Final_Revision.xlsx", "EIA923_Schedules_2_3_4_5_M_12_2015_Final_Revision.xlsx",
           "EIA923_Schedules_2_3_4_5_M_12_2016_Final_Revision.xlsx", "EIA923_Schedules_2_3_4_5_M_12_2017_Final_Revision.xlsx",
           "EIA923_Schedules_2_3_4_5_M_12_2018_Final_Revision.xlsx")

l <- 1
## note, because Form 767 was superseded by Form 923, and Form 923 only began reporting in 2008, there
## is a gap in data.
for (i in c(1985:2005, 2008:2018)) {
  
# boiler fuel ----------------------------------------
if (i %in% c(1985:2000)) {
  fuel <- read_excel(paste0("orig_data/eia_767/f767_", i, "/Boiler_Fuel.xls")) %>% unique()
} else if (i %in% c(2001:2003)) {
  fuel <- read_excel(paste0("orig_data/eia_767/f767_", i, "/F767_BOILER_FUEL.xls")) %>% unique()
} else if (i == 2004) {
  fuel <- read_excel(paste0("orig_data/eia_767/f767_2004/2004 F767_BOILER_FUEL.xls")) %>% unique()
} else if (i == 2005) {
  fuel <- read_excel("orig_data/eia_767/f767_2005/2005 EIA-767 Master Files/F767_BOILER_FUEL.xls") %>% unique()
} else if (i %in% c(2008:2010)) {
  fuel <- read_excel(paste0("orig_data/eia_923/f923_", i, "/", files[l]), 
                        sheet = 6, skip = 7)
} else if (i %in% c(2011:2016)) {
  fuel <- read_excel(paste0("orig_data/eia_923/f923_", i, "/", files[l]), 
                        sheet = 6, skip = 5)
} else if (i == 2017) {
  fuel <- read_excel(paste0("orig_data/eia_923/f923_", i, "/", files[l]), 
                        sheet = 7, skip = 5)
} else if (i == 2018) {
  fuel <- read_excel(paste0("orig_data/eia_923/f923_", i, "/", files[l]), 
                        sheet = 8, skip = 5)
}

  if ("YEAR" %in% names(fuel)) {
    fuel <- fuel %>% select(-YEAR)
  }
  
  fuel <- fuel %>% mutate(year = i)
  
  if (i %in% c(2008:2018)) {
    l <- l + 1
    
    names(fuel) <- gsub("\r\n", " ", names(fuel))
    names(fuel) <- gsub(" ", "_", names(fuel))
    names(fuel) <- tolower(names(fuel))
    names(fuel) <- gsub("apirl", "april", names(fuel))

    fuel <- fuel %>%
      rename(jan_quantity = quantity_of_fuel_consumed_january,
             feb_quantity = quantity_of_fuel_consumed_february,
             mar_quantity = quantity_of_fuel_consumed_march,
             apr_quantity = quantity_of_fuel_consumed_april,
             may_quantity = quantity_of_fuel_consumed_may,
             jun_quantity = quantity_of_fuel_consumed_june,
             jul_quantity = quantity_of_fuel_consumed_july,
             aug_quantity = quantity_of_fuel_consumed_august,
             sep_quantity = quantity_of_fuel_consumed_september,
             oct_quantity = quantity_of_fuel_consumed_october,
             nov_quantity = quantity_of_fuel_consumed_november,
             dec_quantity = quantity_of_fuel_consumed_december,
             jan_heat_content = mmbtu_per_unit_january,
             feb_heat_content = mmbtu_per_unit_february,
             mar_heat_content = mmbtu_per_unit_march,
             apr_heat_content = mmbtu_per_unit_april,
             may_heat_content = mmbtu_per_unit_may,
             jun_heat_content = mmbtu_per_unit_june,
             jul_heat_content = mmbtu_per_unit_july,
             aug_heat_content = mmbtu_per_unit_august,
             sep_heat_content = mmbtu_per_unit_september,
             oct_heat_content = mmbtu_per_unit_october,
             nov_heat_content = mmbtu_per_unit_november,
             dec_heat_content = mmbtu_per_unit_december,
             jan_sulfur_content = sulfur_content_january,
             feb_sulfur_content = sulfur_content_february,
             mar_sulfur_content = sulfur_content_march,
             apr_sulfur_content = sulfur_content_april,
             may_sulfur_content = sulfur_content_may,
             jun_sulfur_content = sulfur_content_june,
             jul_sulfur_content = sulfur_content_july,
             aug_sulfur_content = sulfur_content_august,
             sep_sulfur_content = sulfur_content_september,
             oct_sulfur_content = sulfur_content_october,
             nov_sulfur_content = sulfur_content_november,
             dec_sulfur_content = sulfur_content_december,
             jan_ash_content = ash_content_january,
             feb_ash_content = ash_content_february,
             mar_ash_content = ash_content_march,
             apr_ash_content = ash_content_april,
             may_ash_content = ash_content_may,
             jun_ash_content = ash_content_june,
             jul_ash_content = ash_content_july,
             aug_ash_content = ash_content_august,
             sep_ash_content = ash_content_september,
             oct_ash_content = ash_content_october,
             nov_ash_content = ash_content_november,
             dec_ash_content = ash_content_december)

    ## NOTE, THESE YEARS LACK A UTILITY CODE, BUT PLANT CODE UNIQUELY
    ## IDENTIFIES WE WILL HAVE TO FILL IN UTILITY CODE LATER. FOR NOW
    ## TO LET THE CODE WORK AND SINCE WE AREN'T AGGREGATING
    ## BY UTILITY CODE, I AM JUST MAKING UP NONSENSE CODES
    ## THAT I'LL FIX AT THE MERGE
    
    fuel <- fuel %>%
      rename(plant_code = plant_id,
             fuel_code = reported_fuel_type_code,
             total_quantity = total_fuel_consumption_quantity) %>%
      mutate(utility_code = "STANDIN")

    numeric <- grep("quantity|ash|sulfur|heat", names(fuel))
    fuel[numeric] <- sapply(fuel[numeric], as.numeric)
    
    
  } else {
    l <- l
  }

names(fuel) <- tolower(names(fuel))

#fuel %>% unique_id(utility_code, plant_code, boiler_id, fuel_code)
print(table(fuel$fuel_code))

## we keep only rows with a fuel code that corresponds to coal
fuel <- fuel %>% filter(fuel_code %in% coal)

## this dataset is defined at the boiler/fuel level
print(fuel %>% unique_id(utility_code, plant_code, boiler_id, fuel_code))

if ("sampling_procedure" %in% names(fuel)) {
  fuel <- select(fuel, -sampling_procedure)
}

if ("method_analysis" %in% names(fuel)) {
  fuel <- select(fuel, -method_analysis)
}

if ("lab_performing_analysis" %in% names(fuel)) {
  fuel <- select(fuel, -lab_performing_analysis)
}


if ("preprintgroup" %in% names(fuel)) {
  fuel <- select(fuel, -preprintgroup)
}

## we want to preserve the amount of different types of coal present, just in caes
## it's useful later
fuel <- fuel %>% 
  mutate(fuel_col = ifelse(fuel_code == "COL", total_quantity, 0),
         fuel_bit = ifelse(fuel_code == "BIT", total_quantity, 0),
         fuel_lig = ifelse(fuel_code == "LIG", total_quantity, 0),
         fuel_sc = ifelse(fuel_code == "SC", total_quantity, 0),
         fuel_sub = ifelse(fuel_code == "SUB", total_quantity, 0),
         fuel_wc = ifelse(fuel_code == "WC", total_quantity, 0)) %>%
  select(-fuel_code)

## I'm sure there exists a more elegant way to do this, but this is what I've got
## this section takes a weighted average of the heat/ash/sulfur content of the
## different fuel types used by each boiler 

keepvars <- c("utility_code", "plant_code", "boiler_id", "year",
              grep("quantity", names(fuel), value = TRUE),
              grep("content", names(fuel), value = TRUE))

fuel <- fuel %>%
  mutate(jan_heat_content_wt = jan_heat_content * jan_quantity,
         feb_heat_content_wt = feb_heat_content * feb_quantity,
         mar_heat_content_wt = mar_heat_content * mar_quantity,
         apr_heat_content_wt = apr_heat_content * apr_quantity,
         may_heat_content_wt = may_heat_content * may_quantity,
         jun_heat_content_wt = jun_heat_content * jun_quantity,
         jul_heat_content_wt = jul_heat_content * jul_quantity,
         aug_heat_content_wt = aug_heat_content * aug_quantity,
         sep_heat_content_wt = sep_heat_content * sep_quantity,
         oct_heat_content_wt = oct_heat_content * oct_quantity,
         nov_heat_content_wt = nov_heat_content * nov_quantity,
         dec_heat_content_wt = dec_heat_content * dec_quantity,
         jan_ash_content_wt = jan_ash_content * jan_quantity,
         feb_ash_content_wt = feb_ash_content * feb_quantity,
         mar_ash_content_wt = mar_ash_content * mar_quantity,
         apr_ash_content_wt = apr_ash_content * apr_quantity,
         may_ash_content_wt = may_ash_content * may_quantity,
         jun_ash_content_wt = jun_ash_content * jun_quantity,
         jul_ash_content_wt = jul_ash_content * jul_quantity,
         aug_ash_content_wt = aug_ash_content * aug_quantity,
         sep_ash_content_wt = sep_ash_content * sep_quantity,
         oct_ash_content_wt = oct_ash_content * oct_quantity,
         nov_ash_content_wt = nov_ash_content * nov_quantity,
         dec_ash_content_wt = dec_ash_content * dec_quantity,
         jan_sulfur_content_wt = jan_sulfur_content * jan_quantity,
         feb_sulfur_content_wt = feb_sulfur_content * feb_quantity,
         mar_sulfur_content_wt = mar_sulfur_content * mar_quantity,
         apr_sulfur_content_wt = apr_sulfur_content * apr_quantity,
         may_sulfur_content_wt = may_sulfur_content * may_quantity,
         jun_sulfur_content_wt = jun_sulfur_content * jun_quantity,
         jul_sulfur_content_wt = jul_sulfur_content * jul_quantity,
         aug_sulfur_content_wt = aug_sulfur_content * aug_quantity,
         sep_sulfur_content_wt = sep_sulfur_content * sep_quantity,
         oct_sulfur_content_wt = oct_sulfur_content * oct_quantity,
         nov_sulfur_content_wt = nov_sulfur_content * nov_quantity,
         dec_sulfur_content_wt = dec_sulfur_content * dec_quantity) %>%
  group_by(utility_code, plant_code, boiler_id, year) %>%
  mutate_all(mysum) %>%
  ungroup() %>% unique() %>%
  mutate(jan_heat_content = jan_heat_content_wt/jan_quantity,
         feb_heat_content = feb_heat_content_wt/feb_quantity,
         mar_heat_content = mar_heat_content_wt/mar_quantity,
         apr_heat_content = apr_heat_content_wt/apr_quantity,
         may_heat_content = may_heat_content_wt/may_quantity,
         jun_heat_content = jun_heat_content_wt/jun_quantity,
         jul_heat_content = jul_heat_content_wt/jul_quantity,
         aug_heat_content = aug_heat_content_wt/aug_quantity,
         sep_heat_content = sep_heat_content_wt/sep_quantity,
         oct_heat_content = oct_heat_content_wt/oct_quantity,
         nov_heat_content = nov_heat_content_wt/nov_quantity,
         dec_heat_content = dec_heat_content_wt/dec_quantity,
         jan_ash_content = jan_ash_content_wt/jan_quantity,
         feb_ash_content = feb_ash_content_wt/feb_quantity,
         mar_ash_content = mar_ash_content_wt/mar_quantity,
         apr_ash_content = apr_ash_content_wt/apr_quantity,
         may_ash_content = may_ash_content_wt/may_quantity,
         jun_ash_content = jun_ash_content_wt/jun_quantity,
         jul_ash_content = jul_ash_content_wt/jul_quantity,
         aug_ash_content = aug_ash_content_wt/aug_quantity,
         sep_ash_content = sep_ash_content_wt/sep_quantity,
         oct_ash_content = oct_ash_content_wt/oct_quantity,
         nov_ash_content = nov_ash_content_wt/nov_quantity,
         dec_ash_content = dec_ash_content_wt/dec_quantity,
         jan_sulfur_content = jan_sulfur_content_wt/jan_quantity,
         feb_sulfur_content = feb_sulfur_content_wt/feb_quantity,
         mar_sulfur_content = mar_sulfur_content_wt/mar_quantity,
         apr_sulfur_content = apr_sulfur_content_wt/apr_quantity,
         may_sulfur_content = may_sulfur_content_wt/may_quantity,
         jun_sulfur_content = jun_sulfur_content_wt/jun_quantity,
         jul_sulfur_content = jul_sulfur_content_wt/jul_quantity,
         aug_sulfur_content = aug_sulfur_content_wt/aug_quantity,
         sep_sulfur_content = sep_sulfur_content_wt/sep_quantity,
         oct_sulfur_content = oct_sulfur_content_wt/oct_quantity,
         nov_sulfur_content = nov_sulfur_content_wt/nov_quantity,
         dec_sulfur_content = dec_sulfur_content_wt/dec_quantity) %>%
  ungroup() %>% select(keepvars)

## we are going to generate annual variables as well, in case they are
## more predictive (due to less noise)

print(fuel %>% unique_id(utility_code, plant_code, boiler_id))

fuel <- fuel %>%
  mutate(ann_heat_content = (jan_heat_content * jan_quantity +
                                feb_heat_content * feb_quantity + 
                                mar_heat_content * mar_quantity +
                                apr_heat_content * apr_quantity +
                                may_heat_content * may_quantity +
                                jun_heat_content * jun_quantity +
                                jul_heat_content * jul_quantity +
                                aug_heat_content * aug_quantity +
                                sep_heat_content * sep_quantity +
                                oct_heat_content * oct_quantity +
                                nov_heat_content * nov_quantity +
                                dec_heat_content * dec_quantity)/total_quantity,
         ann_ash_content = (jan_ash_content * jan_quantity +
                              feb_ash_content * feb_quantity + 
                              mar_ash_content * mar_quantity +
                              apr_ash_content * apr_quantity +
                              may_ash_content * may_quantity +
                              jun_ash_content * jun_quantity +
                              jul_ash_content * jul_quantity +
                              aug_ash_content * aug_quantity +
                              sep_ash_content * sep_quantity +
                              oct_ash_content * oct_quantity +
                              nov_ash_content * nov_quantity +
                              dec_ash_content * dec_quantity)/total_quantity,
         ann_sulfur_content = (jan_sulfur_content * jan_quantity +
                                 feb_sulfur_content * feb_quantity + 
                                 mar_sulfur_content * mar_quantity +
                                 apr_sulfur_content * apr_quantity +
                                 may_sulfur_content * may_quantity +
                                 jun_sulfur_content * jun_quantity +
                                 jul_sulfur_content * jul_quantity +
                                 aug_sulfur_content * aug_quantity +
                                 sep_sulfur_content * sep_quantity +
                                 oct_sulfur_content * oct_quantity +
                                 nov_sulfur_content * nov_quantity +
                                 dec_sulfur_content * dec_quantity)/total_quantity) %>%
  unique()

fuel$utility_code <- as.character(fuel$utility_code)
fuel$plant_code <- as.character(fuel$plant_code)

fuel_85_18 <- bind_rows(fuel, fuel_85_18)

}

## lets get some of these vars at the plant level
fuel_85_18 <- fuel_85_18 %>%
  mutate(plt_ann_sulfur_content_wt = ann_sulfur_content * total_quantity,
         plt_ann_heat_content_wt = ann_heat_content * total_quantity,
         plt_ann_ash_content_wt = ann_ash_content * total_quantity) %>%
  group_by(plant_code, year) %>%
  mutate(plt_total_quantity = sum(total_quantity, na.rm = TRUE),
         plt_ann_sulfur_content_wt = sum(plt_ann_sulfur_content_wt, na.rm = TRUE),
         plt_ann_heat_content_wt = sum(plt_ann_heat_content_wt, na.rm = TRUE),
         plt_ann_ash_content_wt = sum(plt_ann_ash_content_wt, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(plt_ann_sulfur_content = plt_ann_sulfur_content_wt/plt_total_quantity,
         plt_ann_heat_content = plt_ann_heat_content_wt/plt_total_quantity,
         plt_ann_ash_content = plt_ann_ash_content_wt/plt_total_quantity) %>%
  select(-plt_ann_sulfur_content_wt, -plt_ann_ash_content_wt, -plt_ann_heat_content_wt)

names(fuel_85_18)

fuel_85_18 <- fuel_85_18 %>%
  select(plant_code, boiler_id, year, total_quantity,
         ann_heat_content, ann_ash_content, ann_sulfur_content,
         plt_ann_sulfur_content, plt_ann_heat_content, plt_ann_ash_content)

unique_id(fuel_85_18, year, plant_code, boiler_id)

fuel_85_18$plant_code <- as.numeric(fuel_85_18$plant_code)

write.csv(fuel_85_18, "use_data/boilers_fuel_comp_1985_2018.csv")

  all_fuel %>% unique_id(plant_code, year)

  all_fuel$plant_code <- as.numeric(all_fuel$plant_code)
  
write.csv(all_fuel, "use_data/all_fuel_1970_2018.csv")