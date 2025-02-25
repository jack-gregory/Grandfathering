## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Data
## Jack Gregory
## 25 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# IMPORT DATA -------------------------------------------------------------------------------------
## Import pre-processed GF data

## Define import files
l.file <- list(
  gf = here("data/use_data/regressions_ready_data.dta")
)

## Import GF dataset
df.gf <- read_dta(l.file$gf) %>%
  
  ## Filter unnecessary years and a lone plant located in HI
  filter(!(year<1985 | year==2006)) %>%
  filter(plant_code!=10673) %>%
  
  ## Select relevant variables
  select(## ... General
         ID,
         plant_code,
         boiler_id,
         UNIT,
         year,
         states,
         ut_type,
         manufact,
         DURATION,
         survive,
         SO2,
         Gf,
         GfA,
         so2_nonattain,
         so2_nonat_Gf,
         applic_reg,
         applic_reg_Gf,
         ARP_subject,
         ARPprice,
         age,
         inservice_y,
         capacity,
         capacity_gf,
         efficiency_100_pct_load,
         state_cap_growth,
         coal2gas_price,
         d_growth,
         sulfur_content_tot,
         sulfur_net_iv,
         ## ... Anticipation
         pre_1963,
         post_1963_Gf,
         capacity_pre_1963,
         capacity_post_1963_GF,
         pre1963_in_nonnat_GF,
         post1963_GF_in_nonnat_const,
         applic_reg_pre1963,
         applic_reg_post1963GF,
         ## ... Non-linear
         age_sq,
         ## ... GF vs GF
         applic_regT,
         ## ... Spillover
         fips_st,
         fips_cnty) %>%
  
  ## Create additional variables
  mutate(across(contains("capacity"), \(x) x/1e3)) %>%
  mutate(UNIT = ifelse(UNIT=="", NA, UNIT),
         survive = survive * 1e2,
         
         pre_Gf = (inservice_y<1957) * Gf,
         post_Gf = (inservice_y>=1957) * Gf,
         capacity_pre_Gf = capacity * pre_Gf,
         capacity_post_Gf = capacity * post_Gf,
         so2_nonat_pre_Gf = so2_nonattain * pre_Gf,
         so2_nonat_post_Gf = so2_nonattain * post_Gf,
         applic_reg_pre_Gf = applic_reg * pre_Gf,
         applic_reg_post_Gf = applic_reg * post_Gf,
         
         AR = 1 - applic_regT,
         capacity_AR = so2_nonattain * AR,
         so2_nonat_AR = applic_reg * AR,
         applic_reg_AR = capacity * AR,
         
         fips = paste0(formatC(as.numeric(fips_st), width=2, format="d", flag="0"),
                       formatC(as.numeric(fips_cnty), width=3, format="d", flag="0"))) %>%
  group_by(fips, year) %>%
  mutate(count = n(),
         share_gf = sum((Gf==1), na.rm=TRUE)/n()) %>%
  ungroup()


### END CODE ###

