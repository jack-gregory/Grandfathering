## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 02_analysis/survival_share.R
## Jack Gregory
## 29 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script produces the "survival share" plot for the Grandfathering project.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  19Sep2021 Jack Gregory  Initial version
## 1.1  29Jun2024 Jack Gregory  New draft; Update for JAERE R&R submission


### START CODE ###


# (1) PREAMBLE ------------------------------------------------------------------------------------

## (1a) Initiate 
## ... Packages
pkgs <- c(
  "here"                              # File system
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
  fig1 = here("out", date, "fig1.pdf")
)


# (2) DATA ---------------------------------------------------------------------------------------

## (2a) Grandfathering data
df.gf <- haven::read_dta(l.file$gf) %>%
  select(ORISPL = plant_code, 
         UNIT = boiler_id, 
         VINTAGE = inservice_y, 
         YEAR = year, 
         SURVIVE = survive) %>%
  arrange(ORISPL, UNIT, YEAR) 


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


## (3b) Plot w/ ten-year bins
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
    geom_area(aes(x=VINTAGE_NUM, y=N_MAX/250, fill="white"), 
              color="#433E85", alpha=0, size=1) +
    geom_col(aes(x=VINTAGE_NUM, y=SHARE, fill="#20A387"), 
             width=0.75, color="#20A387", size=0.5, alpha=0.8) +
    scale_fill_identity(guide="legend", name="", labels=c("Share","Number")) +
    scale_y_continuous(name="Survival share", limits=c(0,1), breaks=seq(0,1,0.2), 
                       sec.axis = sec_axis(~.*250, name="Number of boilers")) +
    scale_x_continuous(breaks=seq(1,8),
                       labels=c("(1932,1942]","(1942,1952]","(1952,1962]","(1962,1972]",
                                "(1972,1982]","(1982,1992]","(1992,2002]","(2002,2012]")) +
    labs(#title="Survival share by vintage in 2014",
         #subtitle="10-year bins",
         x="Vintage") +
    theme_ss() +
    theme(axis.title.y = element_text(colour="#20A387", face="bold"),
          axis.title.y.right = element_text(colour="#433E85", face="bold"),
          axis.title.x = element_text(face="bold")) +
    guides(fill = guide_legend(override.aes=list(color=c(NA,"#433E85"), alpha=c(0.8,0.2))))

ggsave(l.file$fig1, width=7, height=4, units="in")


### END CODE ###

