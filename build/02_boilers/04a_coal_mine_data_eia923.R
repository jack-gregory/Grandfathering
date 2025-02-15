#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Create universe from 1970-present                 #
# Author: Bridget Pals                              #
# Date: 2/1/2019                                    #
# Purpose: Combine EIA-767, EIA-923, and EIA-869 to #
# get location, heat rate, nameplate, and scrubber  #
# information about coal plants.                    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# libraries needed
library(dplyr)    # basic data cleaning
library(readxl)   # read in excel files
library(tidyr)  # reshape wide/long

#setwd("~/Documents/research/research_revesz")
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

mysum <- function(x) (sum(as.numeric(x), na.rm = TRUE))

mymean <- function(x) (mean(as.numeric(x), na.rm = TRUE))

months <- c("january", "february", "march", "april", "may", "june", "july", "august",
            "september", "october", "november", "december")

shortmonths <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

monthnum <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

coal <- c("COL", "BIT", "LIG", "SUB", "ANT", "RC", "WC")

coal_full <- data.frame()

## NAME CROSSWALK
vn_mines <- read.csv("use_data/crosswalks/mine_var_names.csv") %>% unique() 

vn_mines$orig_name <- gsub("\n", "", vn_mines$orig_name)

files <- c(1972:2018)

for (i in files) {
  
  print("***********************************************")
  print(paste("--------------------", i, "--------------------"))
  print("***********************************************")
  
  # https://www.eia.gov/electricity/data/eia923/F759layout_um.txt
  
  if (i < 2008) {
    allplts <- read_excel(paste0("orig_data/eia_423/f423",i,".xls"), col_types = "text")
  } else if (i == 2008) {
    allplts <- read_excel(paste0("orig_data/eia_423/f423",i,".xls"), col_types = "text",
                          skip = 9)
  } else if (i > 2008 & i <= 2011) {
    allplts <- read_excel(paste0("orig_data/eia_423/f423",i,".xls"), col_types = "text",
                          skip = 6)
  } else if (i == 2012) {
    allplts <- read_excel("orig_data/eia_923/f923_2012/EIA923_Schedules_2_3_4_5_M_12_2012_Final_Revision.xlsx", 
                          col_types = "text", sheet = 8, skip = 4)
  } else if (i == 2013) {
    allplts <- read_excel("orig_data/eia_923/f923_2013/EIA923_Schedules_2_3_4_5_2013_Final_Revision.xlsx", 
                          col_types = "text", sheet = 8, skip = 4)
  } else if (i == 2014) {
    allplts <- read_excel("orig_data/eia_923/f923_2014/EIA923_Schedules_2_3_4_5_M_12_2014_Final_Revision.xlsx", 
                          col_types = "text", sheet = 8, skip = 4)
  } else if (i == 2015) {
    allplts <- read_excel("orig_data/eia_923/f923_2015/EIA923_Schedules_2_3_4_5_M_12_2015_Final_Revision.xlsx", 
                          col_types = "text", sheet = 8, skip = 4)
  } else if (i == 2016) {
    allplts <- read_excel("orig_data/eia_923/f923_2016/EIA923_Schedules_2_3_4_5_M_12_2016_Final_Revision.xlsx", 
                          col_types = "text", sheet = 8, skip = 4)
  }else if (i == 2017) {
    allplts <- read_excel("orig_data/eia_923/f923_2017/EIA923_Schedules_2_3_4_5_M_12_2017_Final_Revision.xlsx", 
                          col_types = "text", sheet = 9, skip = 4)
  } else if (i == 2018) {
    allplts <- read_excel("orig_data/eia_923/f923_2018/EIA923_Schedules_2_3_4_5_M_12_2018_Final_Revision.xlsx", 
                          col_types = "text", sheet = 10, skip = 4)
  }
  
  names(allplts)  
  
  names(allplts) <- gsub("\r\n", "", names(allplts))
  names(allplts) <- gsub("\n", "", names(allplts))
  
  orig_name <- as.data.frame(names(allplts), stringsAsFactors = FALSE)
  names(orig_name) <- "orig_name"
  
  newname <- left_join(orig_name, vn_mines)
  
  print(paste("Number of NA in Name XWalk", sum(is.na(newname))))
  
  names(allplts) <- newname$newname
  
  print("# Plants by FUELDESC")
 
  names(allplts) <- tolower(names(allplts))
  
  print(table(allplts$generic_fuel))
  
  allplts <- allplts %>%
    ## coal is coded as "1", "coal" or "COL" depending on the year
    filter(generic_fuel %in% c("1", "coal", "COL", "Coal"))
  
  print(nrow(allplts))
  
  coal_full <- bind_rows(allplts, coal_full)
   
}

  names(coal_full) <- tolower(names(coal_full))
  
  
  coal_full$sulfur_content <- as.numeric(as.character(coal_full$sulfur_content))
  coal_full$heat_content <- as.numeric(as.character(coal_full$heat_content))
  coal_full$ash_content <- as.numeric(as.character(coal_full$ash_content))
  coal_full$quantity_received <- as.numeric(as.character(coal_full$quantity_received))
  coal_full$fuel_cost <- as.numeric(as.character(coal_full$fuel_cost))
  
  ## check for NAs
  sum(is.na(coal_full$mine_county))
  
  coal_full$mine_county[is.na(coal_full$mine_county)] <- "unk"
  coal_full$mine_county[coal_full$mine_county =="999"] <- "unk"
  coal_full$mine_county[coal_full$mine_county == "99900"] <- "unk"
  coal_full$mine_county[coal_full$mine_county == "NA"] <- "unk"
  coal_full$mine_county[coal_full$mine_county == "0"] <- "unk"
  coal_full$mine_county[coal_full$mine_county == "00000"] <- "unk"

  coal_full$year <- as.numeric(as.character(coal_full$year))
  
  table(coal_full$year)
  
  coal_full <- coal_full %>%
    mutate(year = ifelse(year < 1000, year + 1900, year))
  
  ## I check for changes in conventions over time
  for (i in c(1972:2018)) {
    sub <- filter(coal_full, year == i)
    print(hist(sub$sulfur_content[sub$sulfur_content <= 10], main = paste("Year =", i)))
    
  }
  
  ## check heat content - there is a break in data reporting in about 2001 shifting
  ## from BTU to mmBTU 
  for (i in c(1972:2018)) {
    sub <- filter(coal_full, year == i)
    print(hist(sub$heat_content[sub$heat_content], main = paste("Year =", i)))
  }
  
  ## early years were measured in BTU/pound and later years in mmBTU/ton. So, we are
  ## going to convert early years to mmBTU/ton. There is some overlap and messiness
  ## in years where the data was changing over (1998-2001), so we cutoff based on the
  ## underlying value. for mmBTU/ton, the value won't exceed 100 and then 
  ## multiply by 2000 lbs/ton and divide by 1000000btu/mmbtu
  coal_full <- coal_full %>%
    mutate(heat_content = ifelse(heat_content > 100, 
                                 heat_content*2000/1000000, heat_content))
  
  ## reshape wide
  coal_wide <- coal_full %>%
    arrange(plant_code, year, mine_county) %>%
    group_by(plant_code, year, mine_county) %>%
    summarise(sulfur_content = weighted.mean(sulfur_content, quantity_received),
              fuel_cost = weighted.mean(fuel_cost, quantity_received),
              heat_content = weighted.mean(heat_content, quantity_received),
              quantity_received = sum(quantity_received, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(lbs_so2_mmbtu = (sulfur_content * 2 * 10000)/heat_content) %>%
    arrange(plant_code, year, desc(quantity_received))
  
  test <- coal_wide %>% group_by(plant_code, year) %>%
    summarise(n = n())
  
  coal_wide <- coal_wide %>%
    group_by(plant_code, year) %>%
    mutate(id = 1:n()) %>%
    ungroup()
  
  hist(coal_wide$id)
  hist(coal_wide$id[coal_wide$id < 100])
  print(table(coal_wide$id))
  
  ## remove extra var, we will only keep top ten mines by quantity per plant
  coal_wide <- coal_wide %>% 
    filter(id <= 10)
  
  coal_wide <- pivot_wider(coal_wide,
                           id_cols = c(plant_code, year),
                           names_from = id,
                           values_from = c(quantity_received, sulfur_content, 
                                           heat_content, fuel_cost, mine_county))
  
  coal_collapsed <- coal_full %>%
    group_by(plant_code, year) %>%
    summarise(sulfur_content_tot = weighted.mean(sulfur_content, quantity_received),
              fuel_cost_tot = weighted.mean(fuel_cost, quantity_received),
              heat_content_tot = weighted.mean(heat_content, quantity_received),
              quantity_received_tot = sum(quantity_received, na.rm = TRUE)) %>%
    mutate(lbs_so2_mmbtu_tot = (sulfur_content_tot * 2 * 10000)/heat_content_tot)
  
  coal_wide <- left_join(coal_collapsed, coal_wide)

  write.csv(coal_full, "use_data/mine_data_long.csv")  
  write.csv(coal_wide, "use_data/coal_char_data_by_plant_year.csv")
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # -- Merge to County Data --------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # counties <- read.csv("use_data/ctrpoints_counties.csv", stringsAsFactors = FALSE) %>%
  #   select(-X) %>%
  #   mutate(join = 1)
  # 
  # counties$fips_cnty <- as.character(counties$fips_cnty)
  # 
  # head(coal_full$mine_county)
  # table(coal_full$mine_county)
  # 
  # mine_counties <- coal_full %>% 
  #   select(msha_id, mine_name, mine_state, mine_county, mine_county_name) %>%
  #   unique() %>% rename(fips_cnty = mine_county) 
  # 
  # join_counties <- left_join(mine_counties, counties)
  # 
  # ## we are missing about 2% of observations  
  # sum(is.na(join_counties$join))/nrow(join_counties)
  # 
  # mine_counties <- mine_counties %>%
  #   mutate(last_digits = substr(fips_cnty, nchar(fips_cnty) - 1, nchar(fips_cnty))) %>%
  #   mutate(fips_cnty2 = ifelse(last_digits == "00", substr(fips_cnty, 1, nchar(fips_cnty)-2),
  #                              fips_cnty))
  # 
  # mine_counties <- mine_counties %>%
  #   mutate(first_digits = substr(fips_cnty2, 1, 2)) %>%
  #   mutate(fips_cnty3 = ifelse(first_digits == "00", substr(fips_cnty2, 3, nchar(fips_cnty2)),
  #                              fips_cnty2)) %>%
  #   select(-fips_cnty) %>%
  #   rename(fips_cnty = fips_cnty3)
  # 
  # join_counties2 <- left_join(mine_counties, counties)
  # 
  # ## we are missing about 2% of observations  
  # sum(is.na(join_counties2$join))/nrow(join_counties2)  
  # 
