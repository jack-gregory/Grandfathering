library(dplyr)
library(maptools)
library(rgeos)
library(reshape2)

setwd("~/Documents/research/research_revesz")

statefips <- read.csv("use_data/crosswalks/state_fips_xwalk.csv") %>% 
  rename(st_abbr = state_Code, fips_st = fips)
counties <- readShapePoly("orig_data/cb_2018_us_county_500k/cb_2018_us_county_500k")

ids <- as.data.frame(counties) %>% select(fips_st = STATEFP, fips_cnty = COUNTYFP, county = NAME)

centers <- gCentroid(counties, byid = TRUE)

centers <- as.data.frame(centers)

data <- cbind(ids, as.data.frame(centers))

data <- data %>% rename(cty_lat = x, cty_lon = y)

unique(data$fips)

data$fips_st <- as.character(data$fips_st)
data$fips_st <- as.numeric(data$fips_st)

data$fips_cnty <- as.character(data$fips_cnty)
data$fips_cnty <- as.numeric(data$fips_cnty)

data <- data %>% left_join(statefips)

sum(is.na(data$state_name))

data$county <- trimws(data$county)
data$county <- gsub(" County", "", data$county)
data$county <- gsub(" county", "", data$county)
data$county <- gsub("&", "and", data$county)
data$county <- gsub("[[:punct:]]", "", data$county)
data$county <- toupper(data$county)

data$county[data$county == "BALTIMORE" & data$cty_lon < 39.4] <- "BALTIMORE CITY"

data$county[data$county == "FAIRFAX" & data$fips_cnty == 600] <- "FAIRFAX CITY"
data$county[data$county == "FRANKLIN" & data$fips_cnty == 620] <- "FRANKLIN CITY"
data$county[data$county == "RICHMOND" & data$fips_cnty == 760] <- "RICHMOND CITY"
data$county[data$county == "ROANOKE" & data$fips_cnty == 770] <- "ROANOKE CITY"
data$county[data$county == "ST LOUIS" & data$fips_cnty == 510] <- "ST LOUIS CITY"

write.csv(data, "use_data/ctrpoints_counties.csv")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# -- Merge in Attainment Data ----------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

phist <- read.csv("orig_data/phistory.csv", stringsAsFactors = FALSE) %>%
  select(-exportdt)

phist_old <- read.csv("orig_data/phistory_1978_1990.csv", stringsAsFactors = FALSE)

phist_long <- melt(phist, id.vars = c("pollutant", "revoked_naaqs", "state_name",
                                  "st_abbr", "fips_state", "fips_cnty",
                                  "countyname"))

phist_old_long <- melt(phist_old, 
                       id.vars = c("pollutant", "state_name",
                                   "st_abbr", "fips_state", "fips_cnty",
                                   "countyname"))

phist_long <- bind_rows(phist_long, phist_old_long)

phist_long$countyname <- trimws(phist_long$countyname)
phist_long$countyname <- gsub(" County", "", phist_long$countyname)
phist_long$countyname <- gsub(" county", "", phist_long$countyname)
phist_long$countyname <- gsub("&", "and", phist_long$countyname)
phist_long$countyname <- gsub("[[:punct:]]", "", phist_long$countyname)
phist_long$countyname <- toupper(phist_long$countyname)

phist_long$st_abbr <- trimws(phist_long$st_abbr)

phist_long$variable <- gsub("pw_", "", phist_long$variable)
phist_long <- phist_long %>% rename(year = variable, attainment = value)

phist_long <- phist_long %>%
  mutate(so2_nonattain = ifelse(attainment %in% c("P", "W") & 
                                  pollutant %in% c("Sulfur Dioxide (1971)","Sulfur Dioxide (2010)"), 1, 0),
         other_nonattain = ifelse(attainment  %in% c("P", "W") & 
                                    !pollutant %in% c("Sulfur Dioxide (1971)","Sulfur Dioxide (2010)"), 1, 0))

ids <- select(phist_long, fips_cnty, year, st_abbr)

nonattainment <- phist_long %>%
  group_by(fips_cnty, st_abbr, year) %>%
  summarise(so2_nonattain = max(so2_nonattain),
            other_nonattain = max(other_nonattain)) %>%
  ungroup()

## we check for merge and then go in manually
test <- left_join(nonattainment, data)

#View(test %>% filter(is.na(state_name)))

## For the handful of counties that did not match, I manually checked the county
## name in the nonattainment file (phist_long) and then recoded appropriately

## Dade City is located in Pasco County
nonattainment$fips_cnty[nonattainment$fips_cnty == 25 & nonattainment$st_abbr == "FL"] <- 101

## Clifton Forge City is located within Alleghany County, VA 
nonattainment$fips_cnty[nonattainment$fips_cnty == 560 & nonattainment$st_abbr == "VA"] <- 5

## Morongo Band of Indians reservation is located within Riverside County
nonattainment$fips_cnty[nonattainment$fips_cnty == 582 & nonattainment$st_abbr == "CA"] <- 65

# Pechanga Band is also located within Riverside
nonattainment$fips_cnty[nonattainment$fips_cnty == 586 & nonattainment$st_abbr == "CA"] <- 65

test <- left_join(nonattainment, data)
sum(is.na(test$state_name))

## remaining unmatched counties are in Guam
#View(test %>% filter(is.na(state_name)))

nonattainment <- left_join(nonattainment, data) %>% 
  select(fips_cnty, st_abbr, year, so2_nonattain, other_nonattain, county)

write.csv(nonattainment, "use_data/nonattainment_by_cty_year.csv")


