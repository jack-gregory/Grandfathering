## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Plot -- Local Regs
## Jack Gregory
## 29 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# (1) PREAMBLE ------------------------------------------------------------------------------------

## Initiate 
## ... Packages
pkgs <- c(
  "here",                             # File system
  "geofacet"                          # Spatial data
)
install.packages(setdiff(pkgs, rownames(installed.packages())))
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)

source(here("src/preamble.R"))

## ... Definitions
date <- format(Sys.Date(), "%Y%m%d")
fs::dir_create(path(l.path$out, date))

## ... Files
l.file <- list(
  gf = here("data/use_data/regressions_ready_data.dta"),
  state = here("data/eia/shp/USA_States_Generalized.shp"),
  fig3 = here("out", date, "fig3.pdf")
)


# (2) DATA ---------------------------------------------------------------------------------------

## (2a) Grandfathering data
df.gf <- haven::read_dta(l.file$gf) %>%
  select(ORISPL = plant_code, 
         UNIT = boiler_id,
         STATE = plt_state,
         YEAR = year,
         LOCALREG = applic_reg) %>%
  arrange(ORISPL, UNIT, YEAR)


# (3) PLOT ---------------------------------------------------------------------------------------

## (3a) ggplot theme function
theme_lr <- function() {
  theme_classic() +
  theme(strip.background = element_rect(fill="grey85", color=NA),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(colour="grey85", size=0.3),
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
    facet_geo(~ STATE, grid=us_state_grid1[!(us_state_grid1$code %in% c("AK","DC","HI")), ]) +
    labs(#title="Mean local regulation by state and year",
         y="Local regulation [lbs/mmBTu]",
         x="") +
    theme_lr()

ggsave(l.file$fig3, width=11, height=8, units="in")


### END CODE ###

