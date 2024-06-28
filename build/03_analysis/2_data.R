## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Data
## Jack Gregory
## 25 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# IMPORT DATA -------------------------------------------------------------------------------------
## Import pre-processed GF data from <../data/gf_original> folder.

## Define import files
l.file <- list(
  gf = path(l.path$data, "gf_original/regressions_ready_data3.dta")
)

## Import GF dataset
df.gf <- read_dta(l.file$gf) %>%
  
  ## Filter unnecessary years and a lone plant located in HI
  filter(!(year<1985 | year==2006)) %>%
  filter(plant_code!=10673) %>%
  
  ## Select relevant variables
  select(ID,
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
         sulfur_net_iv) %>%
  
  ## Create additional variables
  mutate(UNIT = ifelse(UNIT=="", NA, UNIT),
         # DURATION = DURATION / 10^3,
         survive = survive * 10^2,
         capacity = capacity / 10^3,
         capacity_gf = capacity_gf / 10^3
         )


### END CODE ###

