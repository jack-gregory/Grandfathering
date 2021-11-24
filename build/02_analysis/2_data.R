## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Data
## Jack Gregory
## 26 September 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# IMPORT DATA -------------------------------------------------------------------------------------
## Import pre-processed GF data from <../data/gf_original> folder.

## Define import files
l.file <- list(
  gf = path(l.path$data, "gf_original/regressions_ready_data.dta")
)

## Import GF dataset
df.gf <- read_dta(l.file$gf) %>%
  
  ## Filter unnecessary years and a lone plant located in HI
  filter(!(year<1985 | year==2006)) %>%
  filter(plant_code!=10673) %>%
  
  ## Select relevant variables
  select(plant_code,
         boiler_id,
         UNIT,
         year,
         states,
         ut_type,
         DURATION,
         survive,
         SO2,
         grand_NSR_const,
         grand_NSR_alt,
         so2_nonattain,
         grand_NSR_in_nonnat_const,
         grand_NSR_in_nonnat_alt,
         applic_reg,
         applic_reg_const,
         ARP_subject,
         ARPprice_sp,
         age,
         inservice_y,
         max_boi_nameplate,
         state_cap_growth,
         coal2gas_price,
         d_growth,
         sulfur_content_tot,
         sulfur_dist,
         incomePC,
         share_white) %>%
  
  ## Create additional variables
  mutate(UNIT = ifelse(UNIT=="", NA, UNIT),
         # DURATION = DURATION / 10^3,
         survive = survive * 10^2,
         max_boi_nameplate = max_boi_nameplate / 10^3,
         capacity_gf = max_boi_nameplate * grand_NSR_const,
         share_nonwhite = 1 - share_white,
         NSR_nonwhite = share_nonwhite * grand_NSR_const,
         NSR_income = incomePC * grand_NSR_const)


### END CODE ###

