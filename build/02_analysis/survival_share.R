## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 02_analysis/survival_share.R
## Jack Gregory
## 19 September 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script produces the "survival share" plot for the Grandfathering project.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  19Sep2021 Jack Gregory  Initial version
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
  gf = path(l.path$data, "gf_original/regressions_ready_data2.dta")
)

## ... Definitions
date <- format(Sys.Date(), "%Y%m%d")
# date <- "20210918"
fs::dir_create(path(l.path$out, date))


# (2) DATA ---------------------------------------------------------------------------------------

## (2a) Grandfathering data
df.gf <- haven::read_dta(l.file$gf) %>%
  select(ORISPL = plant_code, 
         UNIT = boiler_id, 
         VINTAGE = inservice_y, 
         YEAR = year, 
         SURVIVE = survive) %>%
  arrange(ORISPL, UNIT, YEAR) 


# (2b) Data investigation
# df.gf %>% 
#   group_by(VINTAGE, YEAR) %>%
#   summarise(N = n(), .groups="drop") %>%
#   filter(VINTAGE %in% c(1977,1978)) %>%
#   mutate(VINTAGE = factor(VINTAGE)) %>%
#   ggplot() +
#     geom_line(aes(x=YEAR, y=N, color=VINTAGE))
# 
# 
# df.gf %>% 
#   # mutate(VINTAGE_BIN = cut(VINTAGE, breaks=seq(1925, 2015, 5))) %>%
#   mutate(VINTAGE_BIN = cut(VINTAGE, breaks=seq(1932, 2011, 10))) %>%
#   # filter(!is.na(VINTAGE_BIN)) %>%
#   group_by(VINTAGE_BIN, YEAR) %>%
#   summarise(N = n(), .groups="drop") %>%
#   # filter(VINTAGE %in% c(1977,1978)) %>%
#   ggplot() +
#     geom_line(aes(x=YEAR, y=N, color=VINTAGE_BIN))
# 
# 
# df.gf %>% 
#   group_by(VINTAGE, YEAR) %>%
#   summarise(N = n()) %>%
#   group_by(VINTAGE) %>%
#   mutate(N_MAX = max(N, na.rm=TRUE)) %>%
#   filter(YEAR==2014) %>%
#   mutate(SHARE = N/N_MAX) %>%
#   ggplot() +
#     geom_line(aes(x=VINTAGE, y=SHARE))


# (3) PLOT ---------------------------------------------------------------------------------------

## (3a) ggplot theme function
theme_ss <- function() {
  theme_classic() +
  theme(strip.background = ggplot2::element_rect(fill="grey85", color=NA),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour="grey85", size=0.3),
        panel.grid.minor.y = ggplot2::element_line(colour="grey85", size=0.3),
        axis.line.x = ggplot2::element_line(size=0.4),
        axis.ticks.x = ggplot2::element_line(size=0.4),
        axis.text.x = element_text(angle=60, vjust=0.5),
        legend.position = "right",
        plot.caption = ggplot2::element_text(hjust=0, size=8))
}


# ## (3b) Plot w/ five-year bins
# df.gf %>% 
#   mutate(VINTAGE_BIN = cut(VINTAGE, breaks=seq(1932, 2011, 5))) %>%
#   filter(!is.na(VINTAGE_BIN)) %>%
#   group_by(VINTAGE_BIN, YEAR) %>%
#   summarise(N = n(), .groups="drop") %>%
#   group_by(VINTAGE_BIN) %>%
#   mutate(N_MAX = max(N, na.rm=TRUE)) %>%
#   filter(YEAR==2014) %>%
#   mutate(SHARE = N/N_MAX,
#          VINTAGE_NUM = as.numeric(VINTAGE_BIN)) %>%
#   ggplot() +
#     geom_area(aes(x=VINTAGE_NUM, y=N_MAX/100, fill="grey90"), 
#               color="grey80", alpha=0.4) +
#     geom_col(aes(x=VINTAGE_NUM, y=SHARE, fill="#20A387"), 
#              width=0.75, color="#20A387", size=0.5, alpha=0.8) +
#     geom_vline(aes(xintercept=10.5), color="#33638D", linetype="dashed", size=0.5) +
#     scale_fill_identity(guide="legend", name="", labels=c("Share","Number")) +
#     scale_y_continuous(name="Survival share",
#                        sec.axis = sec_axis(~.*100, name="Number of boilers")) +
#     scale_x_continuous(breaks=seq(1,15),
#                        labels=c("(1932,1937]","(1937,1942]","(1942,1947]","(1947,1952]",
#                                 "(1952,1957]","(1957,1962]","(1962,1967]","(1967,1972]",
#                                 "(1972,1977]","(1977,1982]","(1982,1987]","(1987,1992]",
#                                 "(1992,1997]","(1997,2002]","(2002,2007]")) +
#     labs(title="Survival share by vintage in 2014",
#          subtitle="5-year bins",
#          x="Vintage",
#          caption="Note: Vertical dashed line represents the approximate NSR cutoff.") +
#     theme_ss() +
#     guides(fill = guide_legend(override.aes=list(color=NA)))
# 
# ggsave(path(l.path$out, date, "fig.survival_share_5yr.pdf"),
#        width=7, height=4, units="in")


## (3c) Plot w/ ten-year bins
df.gf %>% 
  mutate(VINTAGE_BIN = cut(VINTAGE, breaks=seq(1932, 2012, 10)),
         VINTAGE_BIN = factor(
           VINTAGE_BIN,
           levels=c("(1.93e+03,1.94e+03]","(1.94e+03,1.95e+03]","(1.95e+03,1.96e+03]",
                    "(1.96e+03,1.97e+03]","(1.97e+03,1.98e+03]","(1.98e+03,1.99e+03]",
                    "(1.99e+03,2e+03]","(2e+03,2.01e+03]"),
           labels=c("(1932,1942]","(1942,1952]","(1952,1962]","(1962,1972]","(1972,1982]",
                    "(1982,1992]","(1992,2002]","(2002,2012]"))) %>%
  filter(!is.na(VINTAGE_BIN)) %>%
  group_by(VINTAGE_BIN, YEAR) %>%
  summarise(N = n()) %>%
  group_by(VINTAGE_BIN) %>%
  mutate(N_MAX = max(N, na.rm=TRUE)) %>%
  filter(YEAR==2014) %>%
  mutate(SHARE = N/N_MAX,
         VINTAGE_NUM = as.numeric(VINTAGE_BIN)) %>%
  ggplot() +
    geom_area(aes(x=VINTAGE_NUM, y=N_MAX/250, fill="#433E85"), 
              color="#433E85", alpha=0.2) +
    geom_col(aes(x=VINTAGE_NUM, y=SHARE, fill="#20A387"), 
             width=0.75, color="#20A387", size=0.5, alpha=0.8) +
    # geom_vline(aes(xintercept=5.5), color="#33638D", linetype="dashed", size=0.5) +
    scale_fill_identity(guide="legend", name="", labels=c("Share","Number")) +
    scale_y_continuous(name="Survival share", labels=label_percent(), limits=c(0,1), breaks=seq(0,1,0.2),
                       sec.axis = sec_axis(~.*250, name="Number of boilers")) +
    scale_x_continuous(breaks=seq(1,8),
                       labels=c("(1932,1942]","(1942,1952]","(1952,1962]","(1962,1972]",
                                "(1972,1982]","(1982,1992]","(1992,2002]","(2002,2012]")) +
    labs(#title="Survival share by vintage in 2014",
         #subtitle="10-year bins",
         x="Vintage"#,
         #caption="Note: Vertical dashed line represents the approximate NSR cutoff."
         ) +
    theme_ss() +
    guides(fill = guide_legend(override.aes=list(color=NA)))

ggsave(path(l.path$out, date, "fig.survival_share_10yr.pdf"),
       width=7, height=4, units="in")


### END CODE ###

