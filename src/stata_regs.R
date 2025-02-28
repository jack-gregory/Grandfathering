## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## src/stata_regs.R
## Jack Gregory
## 28 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# INTRODUCTION -----------------------------------------------------------------------------------
## This script provides functions necessary to perform analysis regressions in Stata.

## It has the following dependencies:
##  - dplyr
##  - glue
##  - purrr
##  - rlang


# VERSION HISTORY ---------------------------------------------------------------------------------
## V    DATE      EDITOR        NOTES
## 1.0  28Jun2024 Jack Gregory  Initial version


### START CODE ###


## stata_reg --------------------------------------------------------------------------------------
## Perform ols and iv regressions in Stata.

## fml  = formula as a list of stata variables concatenated by spaces
## df   = dataframe containing regression data
## type = regression type in set {reg, iv}

## Method set & use function
stata_reg <- function(fml, df, type, bootstrap=FALSE) {
  
  ## Assertions
  stopifnot(
    is.character(fml),
    is.data.frame(df),
    is.character(type),
    type %in% c("reg","iv"),
    is.logical(bootstrap)
  )
  if (type=="reg" && bootstrap==TRUE) {
    stop('The bootstrap option can only be applied with the "iv" type.')
  }
  
  ## Method
  if (type=="iv") {
    fml <- structure(fml, class=c(class(fml), "iv"))
  } else {
    fml <- structure(fml, class=c(class(fml), "reg"))
  }
  
  ## Use method
  UseMethod("stata_reg", fml)
}

## Method: reg
stata_reg.reg <- function(fml, df, ...) {
  
  cat("\nstata::reg  ", fml, "\n", sep="")
  
  ## Create Stata call
  stata_do <- 
    glue('eststo: reg {fml}, vce(cluster ID)
          regsave, tstat pval ci detail(scalars)
         ')
  
  ## Create filter based on LHS var
  if (str_detect(fml, "^survive")) {
    filter_survive <- expr(year<=2017)
  } else {
    filter_survive <- expr(TRUE)
  }
  
  ## Perform regression
  stata(stata_do,
        data.in = filter(df, !!filter_survive),
        data.out = TRUE)
}

## Method: iv
stata_reg.iv <- function(fml, df, ..., bootstrap) {
  
  cat("\nstata::iv  ", fml, "\n", sep="")
  
  ## Create Stata call
  fml <- str_split(fml, " ", n=2, simplify=TRUE) %>%
    as.list() %>%
    set_names(nm=c("lhs","rhs"))
  if (bootstrap) {
    stata_do <-
      glue("cap program drop gf_bs
            program define gf_bs, rclass
            
              cap drop sulf_cont_iv ARP_iv_sulf_cont
          
            	reg sulfur_content_tot sulfur_net_iv {fml$rhs}, vce(cluster ID)
            	predict sulf_cont_iv
            	gen ARP_iv_sulf_cont = sulf_cont_iv * ARPprice
          	
            	reg {fml$lhs} sulf_cont_iv ARP_iv_sulf_cont {fml$rhs}, vce(cluster ID)
            
            end
            
            bootstrap _b, reps(100) seed(100) cluster(ID): gf_bs
            regsave, tstat pval ci detail(scalars)
           ")
  } else {
    stata_do <- 
      glue("eststo: reg sulfur_content_tot sulfur_net_iv {fml$rhs}, vce(cluster ID)
            predict sulf_cont_iv
            gen ARP_iv_sulf_cont = sulf_cont_iv * ARPprice
  
            eststo: reg {fml$lhs} sulf_cont_iv ARP_iv_sulf_cont {fml$rhs}, vce(cluster ID)
            regsave, tstat pval ci detail(scalars)
           ")
  }
  
  ## Create filter based on LHS var
  if (fml$lhs=="survive") {
    filter_survive <- expr(year<=2017)
  } else {
    filter_survive <- expr(TRUE)
  }
  
  ## Perform regression
  stata(stata_do,
        data.in = filter(df, !!filter_survive),
        data.out = TRUE)
}


### END CODE ###

