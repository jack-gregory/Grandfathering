## (1a) Initiate 
## ... Packages
source(fs::path(here::here(), "src/preamble.R"))
# library(sf)
library(geofacet)

## ... Paths
source(fs::path(here::here(), "src/def_paths.R"))

## ... Files
l.file <- list(
  gf = path(l.path$data, "gf_original/regressions_ready_data.dta"),
  state = path(l.path$data, "eia/shp/USA_States_Generalized.shp")
)

## ... Data
## Grandfathering
df.gf <- haven::read_dta(l.file$gf) %>%
  select(ORISPL = plant_code, 
         UNIT = boiler_id,
         STATE_CODE = states,
         YEAR = year,
         LOCALREG = leg_lbs_mmbtu_calc) %>%
  arrange(ORISPL, UNIT, YEAR) 

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



## Plot maps ...
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
  "Pennsylvania", 1985,
  "Pennsylvania", 2017,
  "Vermont", 1985,
  "Vermont", 2017
)

df.gf %>%
  left_join(df.state, by=c("STATE_CODE")) %>%
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
  mutate(xmin = ifelse(STATE %in% c("California","District of Columbia","Idaho","Maine","Ohio","Pennsylvania","Vermont"), 
                       -Inf, NA),
         xmax = ifelse(STATE %in% c("California","District of Columbia","Idaho","Maine","Ohio","Pennsylvania","Vermont"), 
                       Inf, NA),
         ymin = ifelse(STATE %in% c("California","District of Columbia","Idaho","Maine","Ohio","Pennsylvania","Vermont"), 
                       -Inf, NA),
         ymax = ifelse(STATE %in% c("California","District of Columbia","Idaho","Maine","Ohio","Pennsylvania","Vermont"), 
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

