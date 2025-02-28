## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/text_colour.R
## Jack Gregory
## 25 March 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION -----------------------------------------------------------------------------------
## This script provides a text colouring function for workbooks within the Grandfathering project.

## It has the following dependencies:
##  - knitr
##  - glue


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  25Mar2021 Jack Gregory  Initial version


### START CODE ###


# text_colour -------------------------------------------------------------------------------------
## Automatically define coloured text in Rmd html and pdf documents.

## colour = Colour as a string in set {"red","blue","green"}
## text = Text to be coloured as a string

text_colour <- function(colour, text) {
  
  stopifnot(
    is.character(colour),
    colour %in% c("red","blue","green"),
    is.character(text)
  )
  
  if (knitr::is_html_output()) {
    glue::glue('<span style="color: {colour};">{text}</span>')
  } else if (knitr::is_latex_output()) {
    glue::glue('\\textcolor{<colour>}{<text>}',
               .open="<", .close=">")
  }
}


### END CODE ###

