## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/plots.R
## Jack Gregory
## 2 March 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION -----------------------------------------------------------------------------------
## This script provides a set of ggplot2 theme functions.

## It has the following dependencies:
##  - ggplot2


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  02Mar2024 Jack Gregory  Initial version
## ...
## 2.0  05Aug2020 Jack Gregory  New version; ...
## 2.1  17Jul2017 Jack Gregory  New draft; ...
## ...


### START CODE ###


# theme_sulfur ------------------------------------------------------------------------------------
## ggplot2 theme for plots.

theme_sulfur <- function() {
  theme_classic() +
    theme(strip.background = element_rect(fill="grey85", color=NA),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_line(colour="grey85", size=0.3),
          panel.grid.minor.y = element_line(colour="grey85", size=0.3),
          axis.line.x = element_line(size=0.4),
          axis.ticks.x = element_line(size=0.4),
          legend.position = "right",
          plot.caption = element_text(hjust=0))
}


# theme_maps ------------------------------------------------------------------------------------
## ggplot2 theme for maps.

theme_maps <- function() {
  theme_void() +
    theme(legend.position = "right",
          legend.text = element_text(size=7),
          plot.caption = element_text(hjust=0))
}


### END CODE ###

