## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## 02_analysis/damages.R
## Jack Gregory
## 30 November 2022
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION ------------------------------------------------------------------------------------
## This script produces the "additional emissions & damages" plot for the Grandfathering project.


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  30Nov2022 Jack Gregory  Initial version
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
  data = path(l.path$data, "gf_original/additional_emissions_data.dta")
)

## ... Definitions
date <- format(Sys.Date(), "%Y%m%d")
# date <- "20210918"
fs::dir_create(path(l.path$out, date))


# (2) DATA ---------------------------------------------------------------------------------------
## NB - The data is generated in Stata using <additional_emissions_*.do> stored on Box.

df <- haven::read_dta(l.file$data) |>
  dplyr::filter(!is.na(year))


# (3) PLOT ---------------------------------------------------------------------------------------

## (3a) ggplot theme function
theme_damages <- function() {
  theme_classic() +
  theme(strip.background = ggplot2::element_rect(fill="grey85", color=NA),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(colour="grey85", size=0.3),
        panel.grid.minor.y = ggplot2::element_line(colour="grey85", size=0.3),
        axis.line.x = ggplot2::element_line(size=0.4),
        axis.ticks.x = ggplot2::element_line(size=0.4),
        legend.position = "bottom",
        plot.caption = ggplot2::element_text(hjust=0, size=8))
}


## (3b) Plot
df %>% 
  dplyr::select(YEAR = year, EMISSIONS = total_em, DAMAGES = total_dam) |>
  dplyr::filter(YEAR>=1998 & YEAR<=2017) |>
  ggplot(aes(x=YEAR)) +
    geom_line(aes(y=EMISSIONS, color="#433E85")) +
    geom_line(aes(y=DAMAGES/50, color="#20A387")) +
    scale_color_identity(guide="legend", name="", labels=c("Emissions avoided without NSR","Damages avoided without NSR")) +
    scale_y_continuous(name="Emissions avoided [mn short tons]", 
                       sec.axis = sec_axis(~.*50, name="Damages avoided [bn USD]")) +
    labs(#title="Survival share by vintage in 2014",
         #subtitle="10-year bins",
         # x="Vintage"#,
         #caption="Note: Vertical dashed line represents the approximate NSR cutoff."
         ) +
    theme_damages() +
    theme(axis.title.y = element_text(colour="#20A387", face="bold"),
          axis.title.y.right = element_text(colour="#433E85", face="bold"),
          axis.title.x = element_text(face="bold")) +
    guides(color = guide_legend(override.aes=list(size=3)))

ggsave(path(l.path$out, date, "fig.damages.pdf"),
       width=7, height=4, units="in")


### END CODE ###

