## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 02_analysis/local_reg.R
## Jack Gregory
## 29 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script produces the "local regulations" plot for the Grandfathering project.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  20Sep2021 Jack Gregory  Initial version
## 1.1  29Jun2024 Jack Gregory  New draft; Update for JAERE R&R submission
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  26Aug2020 Jack Gregory  New draft; ...


### START CODE ###


# (1) PREAMBLE ------------------------------------------------------------------------------------

## (1a) Initiate 
## ... Packages
source(fs::path(here::here(), "src/preamble.R"))

# install.packages(setdiff(c("sf"), rownames(installed.packages())))
# library(sf)

install.packages(setdiff(c("geofacet"), rownames(installed.packages())))
library(geofacet)

## ... Paths
source(fs::path(here::here(), "src/def_paths.R"))

## ... Files
l.file <- list(
  gf = path(l.path$data, "gf_original/regressions_ready_data3.dta"),
  state = path(l.path$data, "eia/shp/USA_States_Generalized.shp")
)

## ... Definitions
date <- format(Sys.Date(), "%Y%m%d")
# date <- "20240629"
fs::dir_create(path(l.path$out, date))


# (2) DATA ---------------------------------------------------------------------------------------

## (2a) Grandfathering data
df.gf <- haven::read_dta(l.file$gf) %>%
  select(ORISPL = plant_code, 
         UNIT = boiler_id,
         STATE = plt_state,
         YEAR = year,
         LOCALREG = applic_reg) %>%
  arrange(ORISPL, UNIT, YEAR)


## (2b) State spatial data
df.state <- attributes(df.gf$STATE_CODE)$labels %>%
  as.list() %>%
  as_tibble() %>%
  pivot_longer(cols=everything(), names_to="STATE", values_to="STATE_CODE")

# ## State shapefile
# ## Import state shapefile
# df.state <- read_sf(l.file$state) %>%
#   filter(!(STATE_ABBR %in% c("AK","HI")))
# 
# ## Display coordinate reference system
# st_crs(df.state)
# 
# ## Plot
# ggplot(data=df.state) +
#   geom_sf(color="grey40", fill="grey90", size=0.3) +
#   coord_sf()


# (3) PLOT ---------------------------------------------------------------------------------------

## (3a) ggplot theme function
theme_lr <- function() {
  theme_classic() +
  theme(strip.background = element_rect(fill="grey85", color=NA),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(colour="grey85", size=0.3),
        # panel.grid.minor.y = element_line(colour="grey85", size=0.3),
        axis.line.x = element_line(size=0.4),
        axis.ticks.x = element_line(size=0.4),
        axis.text.x = element_text(angle=50, hjust=1),
        legend.position = "right",
        plot.caption = element_text(hjust=0))
}


## (3b) Create missing state datafame
df.state_miss <- tribble(
  ~STATE, ~YEAR, 
  "California", 1985,
  "California", 2017,
  "District of Columbia", 1985,
  "District of Columbia", 2017,
  "Idaho", 1985,
  "Idaho", 2017,
  "Maine", 1985,
  "Maine", 2017,
  "Ohio", 1985,
  "Ohio", 2017,
  "Vermont", 1985,
  "Vermont", 2017
)
states <- sort(unique(df.state_miss$STATE))


## (3c) Build local regulation plot
df.gf %>%
  # left_join(df.state, by=c("STATE_CODE")) %>%
  filter(YEAR>=1985 & YEAR!=2006 & YEAR<=2017) %>%
  filter(STATE!="Hawaii") %>%
  group_by(STATE, YEAR) %>%
  summarise(LOCALREG = mean(LOCALREG, na.rm=TRUE),
            .groups="drop") %>%
  bind_rows(df.state_miss) %>%
  group_by(STATE) %>%
  complete(YEAR = seq(min(YEAR), max(YEAR))) %>%
  fill(LOCALREG, .direction="down") %>%
  ungroup() %>%
  mutate(xmin = ifelse(STATE %in% states, 
                       -Inf, NA),
         xmax = ifelse(STATE %in% states, 
                       Inf, NA),
         ymin = ifelse(STATE %in% states, 
                       -Inf, NA),
         ymax = ifelse(STATE %in% states, 
                       Inf, NA)) %>%
  mutate_at(vars(ymin, ymax), as.numeric) %>%
  ggplot() +
    geom_line(aes(x=YEAR, y=LOCALREG), color="#20A387", size=0.75) +
    geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey85") +
    facet_geo(~ STATE, grid="us_state_contiguous_grid1") +
    labs(#title="Mean local regulation by state and year",
         y="Local regulation [lbs/mmBTu]",
         x="") +
    theme_lr()

ggsave(path(l.path$out, date, "fig.local_reg.pdf"),
       width=11, height=8, units="in")


### END CODE ###

