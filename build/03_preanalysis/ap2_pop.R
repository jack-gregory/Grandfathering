## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 02_analysis/ap2_pop.R
## Jack Gregory
## 1 July 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script produces the county population estimates for the AP2 emission damages model.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  01Jul2024 Jack Gregory  Initial version
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  26Aug2020 Jack Gregory  New draft; ...


### START CODE ###


# (1) PREAMBLE ------------------------------------------------------------------------------------

## (1a) Initiate 
## ... Packages
source(fs::path(here::here(), "src/preamble.R"))

## ... Paths
source(fs::path(here::here(), "src/def_paths.R"))

## ... Files
l.file <- list(
  ap = fs::path(l.path$data, "ap/pop_2011.csv"),
  fips = fs::path(l.path$data, "ap/AP2_Fips_List.dta"),
  duke90 = fs::path(l.path$data, "duke/POP1990.xlsx"),
  duke00 = fs::path(l.path$data, "duke/POP2000.xlsx")
)


# (2) DATA ---------------------------------------------------------------------------------------

## (2a) AP population data
df.ap2 <- readr::read_csv(l.file$ap, col_names=FALSE)


## (2b) AP FIPS data
df.fips <- haven::read_dta(l.file$fips) |>
  distinct()


## (2c) Duke population estimates
## Prepare initial table bind on rows
df.duke <- purrr::map_dfr(
    l.file[stringr::str_detect(names(l.file), "duke")],
    ~readxl::read_excel(.x)
  ) |>
  dplyr::select(fips:ctyname, total = tot_pop) |>
  dplyr::rename_with(toupper) |>
  ## ... eliminate duplicates
  dplyr::distinct() |>
  ## ... convert FIPS codes to match AP2 (incl. reassigning Miami-Dade FIPS code)
  ## ... NB -- Florida, 1997: Dade county (FIPS 12025) is renamed as Miami-Dade county (FIPS 12086)
  dplyr::mutate(FIPS = as.numeric(FIPS),
                FIPS = ifelse(FIPS==12086, 12025, FIPS))

## Address missing FIPS code
## NB -- Virginia, 2001: The independent city of Clifton Forge (FIPS 51560) merges into Alleghany county (FIPS 51005)
## ... Isolate FIPS codes
df.vi <- df.duke |>
  dplyr::filter(FIPS %in% c(51005,51560)) |>
  dplyr::group_by(YEAR) |>
  dplyr::mutate(COMBINED = sum(TOTAL),
                RATIO = TOTAL/COMBINED,
                PERIOD = ifelse(YEAR>=2000, "≥2000", "<2000")) |>
  dplyr::ungroup()

## ... Calculate the population ratio for <2000
df.vi_ratio <- df.vi |>
  dplyr::group_by(FIPS, STNAME, CTYNAME, PERIOD) |>
  dplyr::summarise(RATIO = mean(RATIO), .groups="drop")

## ... Create replacement rows
df.vi_replace <- df.vi |>
  tidyr::complete(FIPS, YEAR) |>
  tidyr::fill(STNAME, CTYNAME, .direction="downup") |>
  dplyr::filter(PERIOD=="≥2000" | is.na(PERIOD)) |>
  dplyr::arrange(YEAR, FIPS) |>
  dplyr::group_by(YEAR) |>
  tidyr::fill(COMBINED, .direction="down") |>
  dplyr::ungroup() |>
  dplyr::select(-TOTAL, -RATIO, -PERIOD) |>
  dplyr::left_join(df.vi_ratio |>
                     dplyr::filter(PERIOD=="<2000") |>
                     dplyr::select(FIPS, RATIO),
                   by="FIPS") |>
  dplyr::mutate(TOTAL = COMBINED*RATIO)

## ... Replace rows in main dataframe
df.duke <- df.duke |>
  dplyr::filter(!(FIPS==51005 & YEAR>=2000)) |>
  dplyr::bind_rows(dplyr::select(df.vi_replace, -COMBINED, -RATIO))
rm(list=ls(pattern="df.vi"))


# (3) TABLES --------------------------------------------------------------------------------------

## (3a) Calculate AP2 population ratios

df.ratio <- dplyr::bind_cols(df.fips, df.ap2) |>
  tidyr::pivot_longer(cols=-fips, names_to="COL", names_prefix="X", values_to="VAL") |>
  dplyr::rename_with(toupper) |>
  dplyr::group_by(FIPS) |>
  dplyr::mutate(TOTAL = sum(VAL),
                RATIO = VAL/TOTAL) |>
  dplyr::ungroup()


## (3b) Apply ratios to annual duke total population estimates
df.out <- df.duke |>
  dplyr::right_join(dplyr::select(df.ratio, -VAL, -TOTAL), by="FIPS", relationship="many-to-many") |>
  dplyr::mutate(VAL = TOTAL*RATIO)
  

## (3c) Write annual model input files
## Generate list of output dataframes
l.out <- df.out |>
  tidyr::pivot_wider(id_cols=c(FIPS, YEAR), names_from=COL, names_prefix="X", values_from=VAL) |>
  dplyr::filter(!is.na(YEAR)) |>
  dplyr::arrange(YEAR, FIPS) |>
  dplyr::group_split(YEAR)

## Check that they all have the correct number of counties
stopifnot(all(purrr::map_lgl(l.out, ~nrow(.x)==nrow(df.fips))))

## Write annual csv files
purrr::walk(l.out, 
           ~{year <- unique(.x$YEAR)
             .x |>
               dplyr::select(-FIPS, -YEAR) |>
               readr::write_csv(file=fs::path(l.path$data, "ap/estimate", paste0("epop_", year, ".csv")),
                                col_names=FALSE)})


### END CODE ###

