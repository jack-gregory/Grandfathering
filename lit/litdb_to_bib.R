## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## lit/litdb_to_bib.R
## Jack Gregory
## 22 May 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## INTRODUCTION -----------------------------------------------------------------------------------
## This script is an intermediate step in compiling the Grandfathering paper in LaTeX.  It converts 
## a literature database housed in the MS Excel file <LitDatabase.xlsx> into a LaTeX bib file.

## TODO:
##  (1) Address tags with same authors and years (i.e. "Bartik_etal:2020")
##  (2) Address with non-traditional authors (i.e. "The Opportunity Insights Team")
##  (3) Address authors names with accented letters (i.e. Deschênes, O)
##  (4) Add assertions based on assertr


## VERSION HISTORY --------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  22May2021 Jack Gregory  Initial version; Based on BWP <litdb_to_bib.R>


### START CODE ###


## (1) SETUP --------------------------------------------------------------------------------------

## (1a) Initialize packages
source(here::here("src/preamble.R"))


## (1b) Definitions
## Files
l.file <- list(
  litdb = "LitDatabase.xlsx",
  bib = "gf.bib"
)


## (2) IMPORT & CLEAN DATA ------------------------------------------------------------------------

## (2a) Import Lit Database
## Create list of sheets
l.sheet <- readxl::excel_sheets(here("lit", l.file$litdb)) %>%
  as.list() %>%
  purrr::discard(stringr::str_detect(., "^Notes$"))

## Import sheets
df.lit <- l.sheet %>%
  map_dfr(~readxl::read_excel(here("lit", l.file$litdb), sheet=.x) %>% 
            dplyr::select(PUB = Pub,
                          AUTHORS = `Author(s)`, 
                          YEAR = Year,
                          TITLE = Title,
                          ENTITY = `Journal/Organization`,
                          NUMBER = Number))


## (2b) Clean Lit Database
df.lit <- df.lit %>%
  ## Remove extraneous white space
  dplyr::mutate_all(stringr::str_squish) %>%
  ## Set publication categories
  dplyr::mutate(PUB_GRP = case_when(PUB=="Report" ~ "Working Paper",
                                    TRUE ~ PUB))
l.lit <- df.lit %>%
  ## Split datafram by PUB
  dplyr::group_by(PUB_GRP) %>%
  dplyr::group_split() %>%
  ## Set element names
  purrr::set_names(nm=df.lit$PUB_GRP %>% unique() %>% sort() %>% stringr::str_replace(" ", "")) %>%
  ## Set element classes
  purrr::map2(df.lit$PUB_GRP %>% unique() %>% sort() %>% stringr::str_replace(" ", "") %>% as.list(),
              ~structure(.x, class=c(.y, class(.x))))


## (3) FUNCTIONS ----------------------------------------------------------------------------------

## (3a) Create lit_to_bib generic
lit_to_bib <- function(df, file) {
  UseMethod("lit_to_bib")
}


## (3b) Create lit_to_bid.Paper method
lit_to_bib.Paper <- function(df, file) {
  
  ## Data consistency checks
  stopifnot(
    is.data.frame(df),
    is.character(df$PUB),
    is.character(df$AUTHORS),
    is.character(df$YEAR),
    is.character(df$TITLE),
    is.character(df$ENTITY),
    is.character(df$NUMBER)
  )
  
  cat("Prepare paper citations\n")
  
  ## Convert df to TeX
  df <- df %>%
    dplyr::mutate(
      TAG = ifelse(stringr::str_count(AUTHORS, ";")<=2,
                   AUTHORS %>% stringr::str_extract_all("(^|; )[\\w'\\-\\s]+,") %>%
                     purrr::map_chr(~stringr::str_c(.x, collapse="") %>%
                                      stringr::str_replace_all("[;,\\s]", "")),
                   AUTHORS %>% stringr::str_extract("^[\\w'\\-\\s]+,") %>%
                     purrr::map_chr(~stringr::str_replace_all(.x, "[,\\s]", "") %>%
                                      glue::glue("_etal"))),
      AUTHORS = stringr::str_replace_all(AUTHORS, ";", " and"),
      NUMBER = stringr::str_replace_all(NUMBER, c("\\("=": ", "\\)"=""))
    ) %>%
    tidyr::separate(NUMBER, into=c("VOL","NUM","PAGE"), sep=": ") %>%
    dplyr::mutate(
      PP = case_when(is.na(PAGE) & stringr::str_detect(NUM, "-") ~ NUM, 
                     is.na(PAGE) ~ "",
                     TRUE ~ PAGE),
      NUM = ifelse(is.na(PAGE) & stringr::str_detect(NUM, "-"), "", NUM)
    ) %>%
    dplyr::arrange(TAG, YEAR, TITLE) %>%
    dplyr::group_by(TAG, YEAR) %>%
    dplyr::mutate(N = n(),
                  NTH = row_number(),
                  REP = ifelse(N==1, "",
                               chartr(paste0(seq(1,9), collapse=""), 
                                      paste0(letters[1:9], collapse=""), 
                                      as.character(NTH)))) %>%
    dplyr::ungroup() %>%
    dplyr::select(TAG,
                  YR = YEAR,
                  REP, AUTHORS, TITLE,
                  JOURNAL = ENTITY,
                  VOL, NUM, PP) #%>%
  #dplyr::arrange(AUTHORS, YR, TITLE)
  
  ## Output TeX to covid.bib
  df %>% 
    glue::glue_data("
                    @article{<TAG>:<YR><REP>,
                    \tauthor\t= {<AUTHORS>}, 
                    \ttitle\t\t= {<TITLE>},
                    \tjournal\t= {<JOURNAL>}, 
                    \tyear\t\t= {<YR>}, 
                    \tvolume\t= {<VOL>}, 
                    \tnumber\t= {<NUM>}, 
                    \tpages\t\t= {<PP>}
                    }\n
                    ", .open="<", .close=">") %>%
    readr::write_lines(file, append=TRUE)
}


## (3c) Create lit_to_bid.WorkingPaper method
lit_to_bib.WorkingPaper <- function(df, file) {
  
  ## Data consistency checks
  stopifnot(
    is.data.frame(df),
    is.character(df$PUB),
    is.character(df$AUTHORS),
    is.character(df$YEAR),
    is.character(df$TITLE),
    is.character(df$ENTITY),
    is.character(df$NUMBER)
  )
  
  cat("Prepare working paper citations\n")
  
  ## Convert df to TeX
  regex_mth <- month.name %>%
    stringr::str_c(collapse="|") %>%
    paste0("(", ., ")")
  df <- l.lit$WorkingPaper %>%
    dplyr::mutate(
      TAG = ifelse(stringr::str_count(AUTHORS, ";")<=2,
                   AUTHORS %>% stringr::str_extract_all("(^|; )[\\w'\\-\\s]+,") %>%
                     purrr::map_chr(~stringr::str_c(.x, collapse="") %>%
                                      stringr::str_replace_all("[;,\\s]", "")),
                   AUTHORS %>% stringr::str_extract("^[\\w'\\-\\s]+,") %>%
                     purrr::map_chr(~stringr::str_replace_all(.x, "[,\\s]", "") %>%
                                      glue::glue("_etal"))),
      AUTHORS = stringr::str_replace_all(AUTHORS, ";", " and"),
      INST = ifelse(is.na(ENTITY), "", ENTITY)
    ) %>%
    tidyr::separate(NUMBER, into=c("TYPE","NUM","MONTH"), sep="; ") %>%
    dplyr::mutate(
      MTH = ifelse(is.na(MONTH), stringr::str_extract(NUM, regex_mth), MONTH),
      NUM = ifelse(is.na(MONTH), "", NUM)
    ) %>%
    dplyr::arrange(TAG, YEAR, TITLE) %>%
    dplyr::group_by(TAG, YEAR) %>%
    dplyr::mutate(N = n(),
                  NTH = row_number(),
                  REP = ifelse(N==1, "",
                               chartr(paste0(seq(1,9), collapse=""), 
                                      paste0(letters[1:9], collapse=""), 
                                      as.character(NTH)))) %>%
    dplyr::ungroup() %>%
    dplyr::select(TAG,
                  YR = YEAR,
                  REP, AUTHORS, TITLE, INST, MTH, TYPE, NUM) %>%
    dplyr::arrange(AUTHORS, YR, TITLE)
  
  ## Output TeX to covid.bib
  df %>% 
    glue::glue_data("
                    @techreport{<TAG>:<YR><REP>,
                    \tauthor\t= {<AUTHORS>}, 
                    \ttitle\t\t= {<TITLE>},
                    \tinstitution\t= {<INST>}, 
                    \tyear\t\t= {<YR>}, 
                    \tmonth\t\t= {<MTH>}, 
                    \ttype\t\t= {<TYPE>}, 
                    \tnumber\t= {<NUM>}
                    }\n
                    ", .open="<", .close=">") %>%
    readr::write_lines(file, append=TRUE)
}


## (4) CONVERT TO TEX -----------------------------------------------------------------------------


## (4a) File header
## Prepare file header
f_header <- glue::glue("
                      %% Grandfathering Project
                      %% BibTeX File
                      %% Compiled on {base::Sys.time()} using <litdb_to_bib.R>
                      %% Written by Jack Gregory\n
                      ")

## Write file header
f_header %>% readr::write_lines(here("lit", l.file$bib), append=FALSE)


## (4b) Citations
## Prepare & write citations
l.lit %>%
  keep(names(.)!="Note") %>%
  walk(~lit_to_bib(.x, here("lit", l.file$bib)) %>% suppressWarnings())


### END CODE ###

