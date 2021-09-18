#Prepare long dataset on power plant closures
# We prepare the dataset by merging two dataset 
# 1) the data on retirements that was hand-picked from a report attached to a proceeding
# 2) retirements data created by processing our all_years_all_plants_and_features.xlsx file (EIA-based data)


#old_retir<-read.csv(file.choose(),  header = TRUE)

library(tidyr)
library(dplyr)
library(readxl)
#library(padr) # to create missing years
library(ggplot2)
library(taRifx)



Retirements_Before_1986 <- read_excel("G:/Meine Ablage/research/gf/Retirements_Before_1986.xlsx")
#View(Retirements_Before_1986)

retirements_old<-Retirements_Before_1986 %>%
  mutate(ID=paste(Plt_Name,Unit_ID))

#Expand the dataset for power plants retired before 1986
yr<-c(1880:2017)
retirements_aux<-retirements_old %>% expand(yr, nesting(ID))
retirements_old<-inner_join(retirements_aux, retirements_old, by="ID")%>%
  filter(yr>=Year_in_Service, yr<=Retirement_Year)%>%
  mutate(survive= case_when(
    yr == Retirement_Year ~ 0,
    yr< Retirement_Year ~ 1
  ))   %>%
  arrange(ID, yr)%>%
  mutate(plt_state=State)%>%
  mutate(inservice_y=Year_in_Service, grand=1)%>%
  select(ID,yr, survive, plt_state, inservice_y, grand)

#############################################
#Work on the dataset we created based on EIA
#############################################

#load the grandfathering data 
gf_status<- read_excel("G:/Meine Ablage/research/gf/gf_status_born_around_1978.xlsx")

#load the data on boilers
all_years_all_plants_and_features <- read_excel("G:/Meine Ablage/research/gf/all_years_all_plants_and_features.xlsx")

plants_new<-all_years_all_plants_and_features %>%
  mutate(ID=paste(plant_code,boiler_id)) %>%
  group_by(ID)%>%
  mutate(last_year=max(year))%>%
  ungroup() %>%
  filter(last_year==year, year<2018, is.na(inservice_y)==0, inservice_y!="NA") %>%
  left_join(y=gf_status, by=c("plant_code","boiler_id"))%>%
  mutate(grand=case_when(
    (gf == 1 | inservice_y<1980)~ 1,
    inservice_y> 1985 ~ 0
  )) %>%
  select(ID,plt_state, inservice_y, grand,plant_code,boiler_id, last_year ) %>%
  arrange(inservice_y)

plants_new_aux<-plants_new %>% expand(yr, nesting(ID))
plants_new<-inner_join(plants_new_aux, plants_new, by="ID")%>%
  select(ID,yr, plt_state, grand, inservice_y,last_year)%>%
  filter(yr>=inservice_y, yr<=last_year)%>%
  mutate(survive= case_when(
    yr == last_year ~ 0,
    yr< last_year ~ 1
  )) %>%
  arrange(ID, yr) %>%
  select(-last_year)

#############################################
#Join the two datasets
#############################################
retire_all<-rbind(plants_new,retirements_old)
retire_all$inservice_y<-destring(retire_all$inservice_y)

retired<-retire_all%>%
  mutate(age=yr-inservice_y)%>%
  filter(survive==0, yr!=inservice_y)




#Red line shows the maximum age that the power plants could have reached
ggplot(retired, aes(x = inservice_y, y = age)) +
  geom_jitter(aes(colour=as.factor(grand))) +geom_abline(intercept = 2017, slope = -1, color="red", 
  linetype="dashed", size=1.5) +geom_abline(intercept = 1978, slope = -1, color="blue", 
                                            linetype="dashed", size=1.5)


#Show the age of power plants retiring at a given period
require(stats)
reg<-lm(age ~ yr, data = retired)
reg
coeff=coefficients(reg)
eq = paste0("y = ", round(coeff[2],1), "*x  ", round(coeff[1],1))
plot_age_year<-ggplot(retired, aes(x = yr, y = age)) +
  geom_jitter(aes(colour=as.factor(grand))) 
plot_age_year + geom_abline(intercept = -395.1187 , slope = 0.22)+geom_vline(xintercept = 1978)
  ggtitle(eq)

  
  retired_sh<-retired%>%
    filter(yr>1973) %>%
    mutate(grp = yr>1982) 
ggplot(retired_sh, aes(x = yr, y = age, color=grp))+
  geom_point() +
  # geom_smooth()
  geom_smooth(method="lm")



#Later add capacity, SO2 attainment and utility sector
#merge with gf information
rm(all_years_all_plants_and_features, plants_new_aux, gf_status, retirements, plants_new, retirements_aux, retirements_old, Retirements_Before_1986)

######################
### Run the analyses
###################

#retirements$Year_in_Service <- as.Date(paste(retirements$Year_in_Service, 1,1, sep="-"))
#retirements$Retirement_Year <- as.Date(paste(retirements$Retirement_Year, 12,30, sep="-"))
#retirements$ID<-c() 
#retirements$year <- mapply(seq,retirements$Year_in_Service,retirements$Retirement_Year,SIMPLIFY=FALSE)