## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 02_analysis/retire_age.R
## Jack Gregory
## 23 November 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script produces the "retirement age" plot for the Grandfathering project.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  23Nov2021 Jack Gregory  Initial version
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
  gf = path(l.path$data, "gf_original/regressions_ready_data.dta")
)

## ... Definitions
# date <- format(Sys.Date(), "%Y%m%d")
date <- "20211123"
fs::dir_create(path(l.path$out, date))


# (2) DATA ---------------------------------------------------------------------------------------

## (2a) Grandfathering data
df.gf <- haven::read_dta(l.file$gf) %>%
  select(ORISPL = plant_code, 
         UNIT = boiler_id, 
         GF = grand_NSR_const,
         VINTAGE = inservice_y,
         YEAR = year, 
         AGE = age,
         SURVIVE = survive) %>%
  arrange(ORISPL, UNIT, YEAR) 


# (3) PLOT ---------------------------------------------------------------------------------------

## (3a) ggplot theme function
theme_ra <- function() {
  theme_classic() +
  theme(strip.background = ggplot2::element_rect(fill="grey85", color=NA),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour="grey85", size=0.3),
        panel.grid.minor.y = ggplot2::element_line(colour="grey85", size=0.3),
        axis.line.x = ggplot2::element_line(size=0.4),
        axis.ticks.x = ggplot2::element_line(size=0.4),
        axis.text.x = element_text(angle=90, vjust=0.1),
        legend.position = "right",
        plot.caption = ggplot2::element_text(hjust=0, size=8))
}


## (3b) Count plot w/ ten-year bins
df.gf %>% 
  filter(SURVIVE==0) %>% 
  filter(YEAR>=1985 & YEAR<=2017) %>%
  filter(!is.na(VINTAGE) & !is.na(AGE)) %>%
  arrange(ORISPL, UNIT, YEAR) %>%
  group_by(ORISPL, UNIT) %>%
  summarise_all(first) %>%
  
  # group_by(GF) %>%
  # summarize(N = n())
  
  ungroup() %>%
  mutate(GF = factor(GF, levels=c(0,1), labels=c("Not grandfathered","Grandfathered")),
         VINTAGE_BIN = cut(VINTAGE, breaks=seq(1930,2010,10), include.lowest=TRUE, right=FALSE,
                           labels=c("[1930,1940)","[1940,1950)","[1950,1960)","[1960,1970)",
                                    "[1970,1980)","[1980,1990)","[1990,2000)","[2000,2010)")),
         AGE_BIN = cut(AGE, breaks=seq(0,80,10), include.lowest=TRUE, right=FALSE,
                       labels=c("0 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59",
                                "60 - 69","70 - 79"))) %>%
  ggplot() +
    geom_bar(aes(x=AGE_BIN, fill=fct_rev(VINTAGE_BIN)), stat="count", width=1.0, size=0, alpha=0.7) +
    scale_fill_viridis_d(option="viridis", direction=-1, begin=0.05, end=0.95, na.value="grey70") +
    facet_wrap(.~ GF, scales="fixed") +
    labs(title="Boiler age distribution at retirement by grandfathering status",
         subtitle="10-year bins",
         y="Number of boilers",
         x="Retirement age",
         fill="Vintage") +
    theme_ra() +
    guides(fill = guide_legend(override.aes=list(color=NA)))

ggsave(path(l.path$out, date, "fig.retire_age_count.pdf"),
       width=8, height=4.5, units="in")


## (3c) Share plot w/ ten-year bins
df.gf %>% 
  filter(SURVIVE==0) %>% 
  filter(YEAR>=1985 & YEAR<=2017) %>%
  filter(!is.na(VINTAGE) & !is.na(AGE)) %>%
  arrange(ORISPL, UNIT, YEAR) %>%
  group_by(ORISPL, UNIT) %>%
  summarise_all(first) %>%
  ungroup() %>%
  mutate(GF = factor(GF, levels=c(0,1), labels=c("Not grandfathered","Grandfathered")),
         VINTAGE_BIN = cut(VINTAGE, breaks=seq(1930,2010,10), include.lowest=TRUE, right=FALSE,
                           labels=c("[1930,1940)","[1940,1950)","[1950,1960)","[1960,1970)",
                                    "[1970,1980)","[1980,1990)","[1990,2000)","[2000,2010)")),
         AGE_BIN = cut(AGE, breaks=seq(0,80,10), include.lowest=TRUE, right=FALSE,
                       labels=c("0 - 9","10 - 19","20 - 29","30 - 39","40 - 49","50 - 59",
                                "60 - 69","70 - 79"))) %>%
  group_by(GF, VINTAGE_BIN, AGE_BIN) %>%
  summarise(N = n(), .groups="drop") %>%
  group_by(GF) %>%
  mutate(TOT = sum(N, na.rm=TRUE),
         SHARE = N / TOT * 100) %>%
  ggplot() +
    geom_bar(aes(x=AGE_BIN, y=SHARE, fill=fct_rev(VINTAGE_BIN)), 
             stat="identity", width=1.0, size=0, alpha=0.7) +
    scale_fill_viridis_d(option="viridis", direction=-1, begin=0.05, end=0.95, na.value="grey70") +
    facet_wrap(.~ GF, scales="fixed") +
    labs(#title="Boiler age distribution at retirement by grandfathering status",
         #subtitle="10-year bins",
         y="Share of boilers [% GF status]",
         x="Retirement age",
         fill="Vintage") +
    theme_ra() +
    guides(fill = guide_legend(override.aes=list(color=NA)))

ggsave(path(l.path$out, date, "fig.retire_age_share.pdf"),
       width=8, height=4.5, units="in")


### END CODE ###

