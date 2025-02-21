## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 06_coal_mine_data_eia923
## Bridget Pals
## 01 February 2019
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script merges EIA forms 423 and 923 to get coal mine data.


### START CODE ###


# COAL --------------------------------------------------------------------------------------------

coal_full <- data.frame()

## Name crosswalk
vn_mines <- read.csv(here::here("data/xwalk/mine_var_names.csv")) %>% unique() 

vn_mines$orig_name <- gsub("\n", "", vn_mines$orig_name)

files <- c(1972:2018)

for (i in files) {
  
  print("***********************************************")
  print(paste("--------------------", i, "--------------------"))
  print("***********************************************")
  
  if (i<=2011) {
    zip_file <- here::here("data/eia/f923", paste0("f423_", i, ".zip"))
  } else {
    zip_file <- here::here("data/eia/f923", paste0("f923_", i, ".zip"))
  }
  l.zip <- zip::zip_list(zip_file) %>%
    dplyr::pull(filename) %>%
    as.list()
  zip::unzip(zip_file, exdir=fs::path_dir(zip_file))
  
  if (i < 2008) {
    allplts <- read_excel(here::here("data/eia/f923/f423",i,".xls"), col_types = "text")
  } else if (i == 2008) {
    allplts <- read_excel(here::here("data/eia/f923/f423",i,".xls"), col_types = "text", skip = 9)
  } else if (i > 2008 & i <= 2011) {
    allplts <- read_excel(here::here("data/eia/f923/f423",i,".xls"), col_types = "text", skip = 6)
  } else if (i == 2012) {
    allplts <- read_excel(here::here("data/eia/f923/EIA923_Schedules_2_3_4_5_M_12_2012_Final_Revision.xlsx"), 
                          col_types = "text", sheet = 8, skip = 4)
  } else if (i == 2013) {
    allplts <- read_excel(here::here("data/eia/f923/EIA923_Schedules_2_3_4_5_2013_Final_Revision.xlsx"), 
                          col_types = "text", sheet = 8, skip = 4)
  } else if (i == 2014) {
    allplts <- read_excel(here::here("data/eia/f923/EIA923_Schedules_2_3_4_5_M_12_2014_Final_Revision.xlsx"), 
                          col_types = "text", sheet = 8, skip = 4)
  } else if (i == 2015) {
    allplts <- read_excel(here::here("data/eia/f923/EIA923_Schedules_2_3_4_5_M_12_2015_Final_Revision.xlsx"), 
                          col_types = "text", sheet = 8, skip = 4)
  } else if (i == 2016) {
    allplts <- read_excel(here::here("data/eia/f923/EIA923_Schedules_2_3_4_5_M_12_2016_Final_Revision.xlsx"), 
                          col_types = "text", sheet = 8, skip = 4)
  }else if (i == 2017) {
    allplts <- read_excel(here::here("data/eia/f923/EIA923_Schedules_2_3_4_5_M_12_2017_Final_Revision.xlsx"), 
                          col_types = "text", sheet = 9, skip = 4)
  } else if (i == 2018) {
    allplts <- read_excel(here::here("data/eia/f923/EIA923_Schedules_2_3_4_5_M_12_2018_Final_Revision.xlsx"), 
                          col_types = "text", sheet = 10, skip = 4)
  }
  
  l.zip %>%
    purrr::map_chr(\(x) fs::path(fs::path_dir(zip_file), x)) %>%
    fs::file_delete()
  
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
    ## Depending on the year, coal is coded as "1" / "coal" / "COL".
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

## Check for NAs
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

## Check for changes in conventions over time
for (i in c(1972:2018)) {
  sub <- filter(coal_full, year == i)
  print(hist(sub$sulfur_content[sub$sulfur_content <= 10], main = paste("Year =", i)))
  
}

## Check heat content.  There is a break in data reporting around 2001 shifting
## from BTU to mmBTU.
for (i in c(1972:2018)) {
  sub <- filter(coal_full, year == i)
  print(hist(sub$heat_content[sub$heat_content], main = paste("Year =", i)))
}

## Early years were measured in BTU/pound and later years in mmBTU/ton. So, we 
## convert early years to mmBTU/ton.  There is some overlap and messiness in years 
## where the data was transitioning (i.e., 1998-2001), so we cutoff based on the
## underlying value.  For mmBTU/ton, the value will not  exceed 100 and then 
## multiply by 2000 lbs/ton and divide by 1000000btu/mmbtu.
coal_full <- coal_full %>%
  mutate(heat_content = ifelse(heat_content > 100, 
                               heat_content*2000/1000000, heat_content))

## Reshape wide
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

## Remove extra var, where we keep only top ten mines by quantity per plant.
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

write.csv(coal_full, here::here("data/mine_data_long.csv"))  
write.csv(coal_wide, here::here("data/coal_char_data_by_plant_year.csv"))


### END CODE ###

