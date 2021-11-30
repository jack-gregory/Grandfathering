## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Regressions
## Jack Gregory
## 26 September 2021
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# for the restricted sample - can we automatically remove the survival panel?
### START CODE ###


# DEFINE ------------------------------------------------------------------------------------------

## Create output folder
 date <- format(Sys.Date(), "%Y%m%d")
#date <- "20211123"
dir_create(path(l.path$out, date))

## Add output files to l.file
l.file$tbl_main <- path(l.path$out, date, "tbl.reg_main.tex")
l.file$tbl_main_fs <- path(l.path$out, date, "tbl.reg_main_fs.tex")
l.file$tbl_restr <- path(l.path$out, date, "tbl.reg_restr.tex")


# MAIN REGRESSIONS --------------------------------------------------------------------------------

## stata_reg ------------------------------------------------------------------

## fml = formula as a list of stata variables concatenated by spaces
## df = dataframe containing regression data
## type = regression type in set {reg, iv}

## Method set & use function
stata_reg <- function(fml, df, type) {
  
  ## Assertions
  stopifnot(
    is.character(fml),
    is.data.frame(df),
    is.character(type),
    type %in% c("reg","iv")
  )
  
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
    glue('eststo: reg {fml}, vce(robust)
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
        data.in = df %>% filter(!!filter_survive),
        data.out = TRUE)
}

## Method: iv
stata_reg.iv <- function(fml, df, ...) {
  
  cat("\nstata::iv  ", fml, "\n", sep="")
  
  ## Create Stata call
  fml <- str_split(fml, " ", n=2, simplify=TRUE) %>%
    as.list() %>%
    set_names(nm=c("lhs","rhs"))
  stata_do <- 
    glue("eststo: reg sulfur_content_tot sulfur_dist {fml$rhs}, vce(robust)
          predict sulf_cont_iv
          gen ARP_iv_sulf_cont = sulf_cont_iv * ARPprice_sp

          eststo: reg {fml$lhs} sulf_cont_iv ARP_iv_sulf_cont {fml$rhs}, vce(robust)
          regsave, tstat pval ci detail(scalars)
         ")
  
  ## Create filter based on LHS var
  if (fml$lhs=="survive") {
    filter_survive <- expr(year<=2017)
  } else {
    filter_survive <- expr(TRUE)
  }
  
  ## Perform regression
  stata(stata_do,
        data.in = df %>% filter(!!filter_survive),
        data.out = TRUE)
}


## Define version -------------------------------------------------------------

## Define regression version
# ver <- "main"
ver <- "restr"

## Define GF variable
 gf_ver <- "const"
# gf_ver <- "alt"
gf <- glue("grand_NSR_{gf_ver}")


## Build specifications -------------------------------------------------------

## Left-hand side vars
l.lhs <- list(
  "DURATION",
  "survive",
  "SO2"
)

## Right-hand side vars
ctrl_boiler <- "age max_boi_nameplate"
ctrl_env <- glue("so2_nonattain grand_NSR_in_nonnat_{gf_ver} applic_reg applic_reg_const ARPprice_sp")
ctrl_mkt <- "state_cap_growth coal2gas_price d_growth"
fes <- "i.year i.states i.ut_type"

l.rhs <- list(
  glue("{gf} {ctrl_boiler} {fes}"),
  glue("{gf} {ctrl_boiler} capacity_gf {ctrl_env} {fes}"),
  glue("{gf} {ctrl_boiler} capacity_gf {ctrl_env} {fes}"),
  glue("{gf} {ctrl_boiler} capacity_gf {ctrl_env} {fes} ARPprice_sp c.ARPprice_sp#c.sulfur_content_tot"),
  glue("{gf} {ctrl_boiler} capacity_gf {ctrl_env} {fes}"),
  glue("{gf} {ctrl_boiler} capacity_gf {ctrl_env} {fes}"),
  glue("{gf} {ctrl_boiler} capacity_gf {ctrl_env} i.year i.states")
)

## Market controls
l.ctrl <- list(
  0,0,1,1,1,1,1
)

## Regression conditions
if (ver=="main") {
  ## Main regs
  # "condition ut_type!=." does not matter - same observation set
  l.cond <- list(
    "if (ut_type==4 | ut_type==5 | ut_type==2)",
    "if (ut_type==4 | ut_type==5 | ut_type==2)",
    "if (ut_type==4 | ut_type==5 | ut_type==2)",
    "if (ut_type==4 | ut_type==5 | ut_type==2)",
    "if (ut_type==4 | ut_type==5 | ut_type==2)",
    "if year<2019",
    "if ut_type==4"
  )
} else {
  # Robustness regs based on time discontinuity
  l.cond <- list(
    "if ((ut_type==4 | ut_type==5 |ut_type==2) & inservice_y>=1970 & inservice_y<1995)",
    "if ((ut_type==4 | ut_type==5 |ut_type==2) & inservice_y>=1970 & inservice_y<1995)",
    "if ((ut_type==4 | ut_type==5 |ut_type==2) & inservice_y>=1970 & inservice_y<1995)",
    "if ((ut_type==4 | ut_type==5 |ut_type==2) & inservice_y>=1970 & inservice_y<1995)",
    "if ((ut_type==4 | ut_type==5 |ut_type==2) & inservice_y>=1970 & inservice_y<1995)",
    "if (year<2019 & inservice_y>=1970 & inservice_y<1995)",
    "if (ut_type==4 & inservice_y>=1970 & inservice_y<1995)"
  )
}

## Regression type
l.type <- list(
  "reg","reg","reg","reg","iv","iv","iv"
)


## Perform regressions --------------------------------------------------------

## Build regression dataframe
df.reg <- tibble(l.type, l.rhs, l.ctrl, l.cond) %>%
  rowid_to_column(var="model_id") %>%
  nest(data=everything()) %>%
  mutate(N = 3) %>%
  uncount(N) %>%
  bind_cols(tibble(l.lhs), .) %>%
  unnest(cols=c(l.lhs, data)) %>%
  rename_with(.fn=~str_replace(., "l\\.", "")) %>%
  mutate(ctrl = ifelse(lhs!="SO2" & ctrl==1, ctrl_mkt, ""),
         sample = ifelse(lhs=="survive", "& max_boi_nameplate>0.075", ""),
         cond = as.character(cond),
         fml = paste(lhs, rhs, ctrl, cond, sample, sep=" ")) %>%
  mutate_at(vars(-model_id), as.character) %>%
  mutate(model = map2(fml, type,
                      ~stata_reg(.x, df.gf, .y)),
         model = map(model, 
                     ~select(.x, var, coef, stderr, tstat, pval, ci_lower, ci_upper, 
                             N, r2, r2_a))) %>%
  unnest(model)

## Summarize formulae
df.reg_summary <- df.reg %>% 
  distinct(lhs, model_id, type, fml)


## Build regression table (longtable) -----------------------------------------
## Construct column names
tbl_colnames <- df.reg %>%
  mutate(owner = case_when(str_detect(fml, "ut_type==5") ~ "IOU+",
                           str_detect(fml, "ut_type==4") ~ "IOU",
                           TRUE ~ "All")) %>%
  distinct(model_id, type, owner) %>%
  mutate(model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}"),
         type = ifelse(type=="reg", "OLS", "IV"),
         type = paste0("\\multicolumn{1}{c}{\\textit{", type, "}}"),
         owner = paste0("\\multicolumn{1}{c}{\\textit{", owner, "}}")) %>%
  mutate_all(~paste0(., " &")) %>%
  add_row(model_id=" &", type=" &", owner=" &", .before=1) %>%
  add_row(model_id="\\\\\\\\[-1.8ex]", type="\\\\\\\\[-1.8ex]", owner="\\\\") %>%
  # mutate(hline = ifelse(order(model_id)<=1, "\\hline \\\\[-1.8ex]", "")) %>%
  t() %>%
  as_tibble() %>%
  mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))

## Construct table coefficients
l.var <- list(glue("grand_NSR_{gf_ver}"),"max_boi_nameplate","capacity_gf","so2_nonattain",
              glue("grand_NSR_in_nonnat_{gf_ver}"),"applic_reg","applic_reg_const", "sulf_cont_iv")
l.var_labs <- list("GF","size","GF $\\times$ size","NAAQS","GF $\\times$ NAAQS",
                   "MMBTU","GF $\\times$ MMBTU", "SO2cont IV")
tbl_coef <- df.reg %>%
  filter(var %in% unlist(l.var)) %>%
  mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
  mutate(coef = case_when(
    pval<0.001 ~ paste0(coef, "^{***}"),
    pval<0.01 ~ paste0(coef, "^{**}"),
    pval<0.05 ~ paste0(coef, "^{*}"),
    TRUE ~ as.character(coef)),
    tstat = paste0("(", tstat, ")")) %>%
  select(lhs, model_id, var, coef, tstat) %>%
  pivot_longer(cols=c(coef, tstat), names_to="stat", values_to="val") %>%
  pivot_wider(names_from=model_id, names_prefix="model", values_from=val, values_fill="") %>%
  mutate(lhs = factor(lhs, levels=c("DURATION","survive","SO2"),
                      labels=c("Utilization","Survival","Emissions")),
         var = factor(var, levels=unlist(l.var), labels=unlist(l.var_labs)),
         stat = factor(stat, levels=c("coef","tstat"))) %>%
  arrange(lhs, var, stat) %>%
  relocate(stat, .after=lhs) %>%
  mutate(var = as.character(var), 
         var = case_when(stat=="coef" ~ var, TRUE ~ "")) %>%
  mutate_at(vars(last_col()), ~case_when(var=="GF" ~ paste0(., " \\rule{0pt}{2.6ex} \\\\"),
                                         stat=="coef" ~ paste0(., " \\\\"), 
                                         TRUE ~ paste0(., " \\\\ [1.0ex]"))) %>%
  mutate_at(vars(3:last_col(offset=1)), ~paste0(., " &")) %>%
  select(-stat)

## Construct table summary
l.sum <- list("fe_yr","fe_st","fe_ut","ctrl_mkt","ctrl_so2","N","r2")
l.sum_labs <- list("Year FE","State FE","Utility FE","Market Controls","Sulfur Controls",
                   "Observations","R$^{2}$")
tbl_summary <- df.reg %>%
  distinct(lhs, model_id, type, fml, N, r2) %>%
  mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
  mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
  mutate(fe_yr = ifelse(str_detect(fml, "i.year"), "X", ""),
         fe_st = ifelse(str_detect(fml, "i.states"), "X", ""),
         fe_ut = ifelse(str_detect(fml, "i.ut_type"), "X", ""),
         ctrl_mkt = ifelse(str_detect(fml, ctrl_mkt), "X", ""),
         ctrl_so2 = ifelse((type=="iv"|str_detect(fml, "sulfur_content_tot")), "X", "")) %>%
  select(lhs, model_id, starts_with("fe"), starts_with("ctrl"), N, r2) %>%
  mutate_at(vars(starts_with("fe"), starts_with("ctrl")), 
            ~case_when(is.na(.) ~ "\\textit{}",
                       TRUE ~ paste0("\\textit{", ., "}"))) %>%
  mutate_at(vars(-lhs, -model_id), ~ paste0("\\multicolumn{1}{c}{", ., "}")) %>%
  pivot_longer(cols=c(-lhs, -model_id), names_to="var", values_to="val") %>%
  pivot_wider(names_from=model_id, names_prefix="model", values_from=val) %>%
  mutate(lhs = factor(lhs, levels=c("DURATION","survive","SO2"),
                      labels=c("Utilization","Survival","Emissions")),
         var = factor(var, levels=unlist(l.sum), labels=unlist(l.sum_labs))) %>%
  arrange(lhs, var) %>%
  mutate_at(vars(2:last_col(offset=1)), ~paste0(., " &")) %>%
  mutate_at(vars(last_col()), ~paste0(., " \\\\"))

## Prepare file header, table header and footer, and subtable headers
if (ver=="main") {
  f_header <- c("%% Grandfathering Project",
              "%% Main regression table",
              glue::glue("%% Compiled on {base::Sys.time()} using <3_regs.R>"),
              "%% Written by Jack Gregory\n")
} else {
  f_header <- c("%% Grandfathering Project",
                "%% Restricted regression table",
                glue::glue("%% Compiled on {base::Sys.time()} using <3_regs.R>"),
                "%% Written by Jack Gregory\n")
}
tbl_header1 <- c("\\begin{center}",
                 "\\begin{singlespace}",
                 "\\begin{scriptsize}\n",
                 paste0("\\begin{longtable}[c]{@{\\extracolsep{-0.3ex}}l*{", 
                        ncol(tbl_colnames)-2, "}{D{.}{.}{-2}}@{}}"),
                 "\t\\caption{Main regression results}",
                 paste0("\t\\label{tbl:reg_", ver, "}\n"),
                 "\\\\",
                 "\\hline\\hline")
tbl_header2 <- c("\\midrule \\\\",
                 "\\endfirsthead",
                 "\\hline\\hline")
tbl_header3 <- c("\\midrule \\\\",
                 "\\endhead")
subtbl_headerA <- c(paste0("\\multicolumn{", ncol(tbl_colnames)-1, 
                           "}{l}{\\textbf{Panel A:  Utilization}} \\\\ [1.0ex]"),
                    "\\hline")
subtbl_headerB <- c("\\hline \\\\",
                    paste0("\\multicolumn{", ncol(tbl_colnames)-1, 
                           "}{l}{\\textbf{Panel B:  Survival}} \\\\ [1.0ex]"),
                    "\\hline")
subtbl_headerC <- c("\\hline \\\\",
                    "\\pagebreak",
                    paste0("\\multicolumn{", ncol(tbl_colnames)-1, 
                           "}{l}{\\textbf{Panel C:  Emissions}} \\\\ [1.0ex]"),
                    "\\hline")
tbl_midder <- c("\\hline \\\\ [-1.8ex]")
tbl_footer <- c("\\hline\\hline",
                paste0("\\textit{Notes:} & \\multicolumn{", ncol(tbl_colnames)-2,
                       "}{l}{*** p$<$0.001;  ** p$<$0.01;  * p$<$0.05; ",
                       "\\textit{t}-statistics in parentheses.  } \\\\"),
                "\\\\",
                "\\end{longtable}\n",
                "\\end{scriptsize}",
                "\\end{singlespace}",
                "\\end{center}")

#Add to notes: Market controls include electricity demand growth, 
#coal-gas price ratios and generation capacity growth in the state. 
#Sulfur controls include..
## Output TeX table file
file <- l.file[[paste0("tbl_", ver)]]
f_header %>% write_lines(file, append=FALSE)
tbl_header1 %>% write_lines(file, append=TRUE)
tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, quote_escape=FALSE)
tbl_header2 %>% write_lines(file, append=TRUE)
tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, quote_escape=FALSE)
tbl_header3 %>% write_lines(file, append=TRUE)

subtbl_headerA %>% write_lines(file, append=TRUE)
tbl_coef %>% 
  filter(lhs=="Utilization") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, quote_escape=FALSE)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_summary %>% 
  filter(lhs=="Utilization") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, quote_escape=FALSE)

subtbl_headerB %>% write_lines(file, append=TRUE)
tbl_coef %>% 
  filter(lhs=="Survival") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, quote_escape=FALSE)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_summary %>% 
  filter(lhs=="Survival") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, quote_escape=FALSE)

subtbl_headerC %>% write_lines(file, append=TRUE)
tbl_coef %>%
  filter(lhs=="Emissions") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, quote_escape=FALSE)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_summary %>%
  filter(lhs=="Emissions") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, quote_escape=FALSE)

tbl_footer %>% write_lines(file, append=TRUE)


## Build regression table (table) ---------------------------------------------
# ## Construct column names
# tbl_colnames <- df.reg %>%
#   distinct(model_id) %>%
#   mutate(model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}")) %>%
#   mutate_all(~paste0(., " &")) %>%
#   add_row(model_id=" &", .before=1) %>%
#   add_row(model_id="\\\\\\\\[-1.8ex]") %>%
#   # mutate(hline = ifelse(order(model_id)<=1, "\\hline \\\\[-1.8ex]", "")) %>%
#   t() %>%
#   as_tibble() %>%
#   mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))
# 
# ## Construct table coefficients
# l.var <- list("grand_NSR_const","max_boi_nameplate","capacity_gf","so2_nonattain",
#            "grand_NSR_in_nonnat_const","applic_reg","applic_reg_const")
# l.var_labs <- list("GF","size","GF $\\times$ size","NAAQS","GF $\\times$ NAAQS",
#                 "MMBTU","GF $\\times$ MMBTU")
# tbl_coef <- df.reg %>%
#   filter(var %in% unlist(l.var)) %>%
#   mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
#   mutate(coef = case_when(
#     pval<0.001 ~ paste0(coef, "^{***}"),
#     pval<0.01 ~ paste0(coef, "^{**}"),
#     pval<0.05 ~ paste0(coef, "^{*}"),
#     TRUE ~ as.character(coef)),
#     tstat = paste0("(", tstat, ")")) %>%
#   select(lhs, model_id, var, coef, tstat) %>%
#   # mutate(space = "") %>%
#   pivot_longer(cols=c(coef, tstat), names_to="stat", values_to="val") %>%
#   pivot_wider(names_from=model_id, names_prefix="model", values_from=val, values_fill="") %>%
#   mutate(lhs = factor(lhs, levels=c("DURATION","survive","SO2"),
#                       labels=c("Utilization","Survival","Emissions")),
#          var = factor(var, levels=unlist(l.var), labels=unlist(l.var_labs)),
#          stat = factor(stat, levels=c("coef","tstat"))) %>%
#   arrange(lhs, var, stat) %>%
#   relocate(stat, .after=lhs) %>%
#   mutate(var = as.character(var),
#          var = case_when(stat=="coef" ~ var, TRUE ~ "")) %>%
#   mutate_at(vars(3:last_col(offset=1)), ~paste0(., " &")) %>%
#   mutate_at(vars(last_col()), ~ifelse(stat=="coef", paste0(., " \\\\"), paste0(., " \\\\ [1.0ex]"))) %>%
#   select(-stat)
# 
# ## Construct table summary
# l.sum <- list("fe_yr","fe_st","fe_ut","ctrl_mkt","ctrl_so2","N","r2")
# l.sum_labs <- list("Year FE","State FE","Utility FE","Market Controls","Sulfur Controls",
#                    "Observations","R$^{2}$")
# tbl_summary <- df.reg %>%
#   distinct(lhs, model_id, type, fml, N, r2) %>%
#   mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
#   mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
#   mutate(fe_yr = ifelse(str_detect(fml, "i.year"), "X", ""),
#          fe_st = ifelse(str_detect(fml, "i.states"), "X", ""),
#          fe_ut = ifelse(str_detect(fml, "i.ut_type"), "X", ""),
#          ctrl_mkt = ifelse(str_detect(fml, ctrl_mkt), "X", ""),
#          ctrl_so2 = ifelse(type=="iv", "X", "")) %>%
#   select(lhs, model_id, starts_with("fe"), starts_with("ctrl"), N, r2) %>%
#   mutate_at(vars(starts_with("fe"), starts_with("ctrl")),
#             ~case_when(is.na(.) ~ "\\textit{}",
#                        TRUE ~ paste0("\\textit{", ., "}"))) %>%
#   mutate_at(vars(-lhs, -model_id), ~ paste0("\\multicolumn{1}{c}{", ., "}")) %>%
#   pivot_longer(cols=c(-lhs, -model_id), names_to="var", values_to="val") %>%
#   pivot_wider(names_from=model_id, names_prefix="model", values_from=val) %>%
#   mutate(lhs = factor(lhs, levels=c("DURATION","survive","SO2"),
#                       labels=c("Utilization","Survival","Emissions")),
#          var = factor(var, levels=unlist(l.sum), labels=unlist(l.sum_labs))) %>%
#   arrange(lhs, var) %>%
#   mutate_at(vars(2:last_col(offset=1)), ~paste0(., " &")) %>%
#   mutate_at(vars(last_col()), ~paste0(., " \\\\"))
# 
# ## Prepare file header and table header and footer
# f_header <- c("%% Grandfathering Project",
#               "%% Main regression table",
#               glue::glue("%% Compiled on {base::Sys.time()} using <3_regs.R>"),
#               "%% Written by Jack Gregory",
#               "\n")
# tbl_header <- c("\\begin{table}[ht]",
#                 "\\centering",
#                 "\t\\caption{Main regression results} ",
#                 "\t\\label{tbl:reg_main}",
#                 "\\scriptsize\n",
#                 paste0("\\begin{tabular}{@{\\extracolsep{-0.7ex}}l*{", ncol(tbl_colnames)-2,
#                        "}{D{.}{.}{-2}}@{}}"),
#                 "\\\\[-1.8ex] \\hline",
#                 "\\hline \\\\[-1.8ex]")
# subtbl_header <- c("\\multicolumn{", ncol(tbl_colnames)-1, "}{l}{\\textbf{Panel A:  Utilization [...]}} \\\\",
#                    "\\hline")
# tbl_midder <- c("\\hline \\\\[-1.8ex]")
# tbl_footer <- c("\\hline",
#                 "\\hline \\\\[-1.8ex]",
#                 paste0("\\textit{Notes:} & \\multicolumn{", ncol(tbl_colnames)-2,
#                        "}{l}{*** p$<$0.001;  ** p$<$0.01;  * p$<$0.05; ",
#                        "\\textit{t}-statistics in parentheses.} \\\\"),
#                 "\\end{tabular}",
#                 "\\end{table}")
# 
# ## Output TeX table file
# f_header %>% write_lines(l.file$tbl_main, append=FALSE)
# tbl_header %>% write_lines(l.file$tbl_main, append=TRUE)
# tbl_colnames %>% write_tsv(l.file$tbl_main, append=TRUE, col_names=FALSE, quote_escape=FALSE)
# subtbl_header %>% write_lines(l.file$tbl_main, append=TRUE)
# tbl_coef %>%
#   filter(lhs=="Utilization") %>%
#   select(-lhs) %>%
#   write_tsv(l.file$tbl_main, append=TRUE, col_names=FALSE, quote_escape=FALSE)
# tbl_midder %>% write_lines(l.file$tbl_main, append=TRUE)
# tbl_summary %>%
#   filter(lhs=="Utilization") %>%
#   select(-lhs) %>%
#   write_tsv(l.file$tbl_main, append=TRUE, col_names=FALSE, quote_escape=FALSE)
# tbl_footer %>% write_lines(l.file$tbl_main, append=TRUE)


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl|subtbl)_"))
rm(list=ls(pattern="^ctrl_"), fes)
rm(l.lhs, l.rhs, l.ctrl, l.cond, l.type, l.var, l.var_labs, l.sum, l.sum_labs)


# FIRST STAGE REGRESSIONS-------------------------------------------------------------------------
# changes needed:
# have two  sets of regressions (separately for utilization/survival and emissions)
# change the footnote (like in overleaf)
# automate the first stage with the second stage

## Build specifications -------------------------------------------------------

## Left-hand side vars
l.lhs <- list(
  "DURATION",
  "survive",
  "SO2"
)

## Right-hand side vars
ctrl_boiler <- "age max_boi_nameplate"
ctrl_env <- "so2_nonattain grand_NSR_in_nonnat_const applic_reg applic_reg_const ARPprice_sp"
ctrl_mkt <- "state_cap_growth coal2gas_price d_growth"
fes <- "i.year i.states i.ut_type"

l.rhs <- list(
  glue("grand_NSR_const {ctrl_boiler} capacity_gf {ctrl_env} {fes}"),
  glue("grand_NSR_const {ctrl_boiler} capacity_gf {ctrl_env} {fes}"),
  glue("grand_NSR_const {ctrl_boiler} capacity_gf {ctrl_env} i.year i.states")
)

## Regression conditions
l.cond <- list(
  "if (ut_type==4 | ut_type==5 | ut_type==2)",
  "if ut_type!=.",
  "if ut_type==4"
)

## Regression type
l.type <- list(
  "reg","reg","reg"
)


## Perform regressions --------------------------------------------------------

## Build regression dataframe
df.reg <- tibble(l.type, l.rhs, l.cond) %>%
  rowid_to_column(var="model_id") %>%
  nest(data=everything()) %>%
  mutate(N = 3) %>%
  uncount(N) %>%
  bind_cols(tibble(l.lhs), .) %>%
  unnest(cols=c(l.lhs, data)) %>%
  rename_with(.fn=~str_replace(., "l\\.", "")) %>%
  mutate(ctrl = ifelse(lhs!="SO2", ctrl_mkt, ""),
         cond = as.character(cond),
         fml = paste("sulfur_content_tot sulfur_dist", rhs, ctrl, cond, sep=" ")) %>%
  mutate_at(vars(-model_id), as.character) %>%
  mutate(model = map2(fml, type,
                      ~stata_reg(.x, df.gf, .y)),
         model = map(model, 
                     ~select(.x, var, coef, stderr, tstat, pval, ci_lower, ci_upper, 
                             N, r2, r2_a))) %>%
  unnest(model)

## Build regression table (table) ---------------------------------------------
## Construct column names
tbl_colnames <- df.reg %>%
  mutate(owner = case_when(str_detect(fml, "ut_type==5") ~ "IOU+",
                           str_detect(fml, "if ut_type==4") ~ "IOU",
                           TRUE ~ "All")) %>%
  distinct(lhs, model_id, type, owner) %>%
  mutate(lhs = factor(lhs, levels=c("DURATION","survive","SO2"),
                      labels=c("Utilization","Survival","Emissions")),
         lhs = ifelse(model_id==max(model_id), paste0("\\multicolumn{3}{c}{", lhs, "}"), ""),
         model_id = model_id + 3,
         model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}"),
         type = ifelse(type=="reg", "OLS", "IV"),
         type = paste0("\\multicolumn{1}{c}{\\textit{", type, "}}"),
         owner = paste0("\\multicolumn{1}{c}{\\textit{", owner, "}}")) %>%
  mutate_all(~paste0(., " &")) %>%
  mutate(lhs = ifelse(lhs==" &", "", lhs)) %>%
  add_row(lhs=" &", model_id=" &", type=" &", owner=" &", .before=1) %>%
  add_row(lhs="\\\\ \\cmidrule{2-4}\\cmidrule{5-7}\\cmidrule{8-10}", 
          model_id="\\\\\\\\ [-1.8ex]", type="\\\\\\\\ [-1.8ex]", owner="\\\\") %>%
  t() %>%
  as_tibble() %>%
  mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))

## Construct table coefficients
l.var <- list("sulfur_dist","grand_NSR_const","max_boi_nameplate","capacity_gf","so2_nonattain",
              "grand_NSR_in_nonnat_const","applic_reg","applic_reg_const")
l.var_labs <- list("sulfur","GF","size","GF $\\times$ size","NAAQS","GF $\\times$ NAAQS","MMBTU",
                   "GF $\\times$ MMBTU")
tbl_coef <- df.reg %>%
  filter(var %in% unlist(l.var)) %>%
  mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
  mutate(coef = case_when(pval<0.001 ~ paste0(coef, "^{***}"),
                          pval<0.01 ~ paste0(coef, "^{**}"),
                          pval<0.05 ~ paste0(coef, "^{*}"),
                          TRUE ~ as.character(coef)),
         tstat = paste0("(", tstat, ")")) %>%
  select(lhs, model_id, var, coef, tstat) %>%
  pivot_longer(cols=c(coef, tstat), names_to="stat", values_to="val") %>%
  pivot_wider(names_from=c(lhs, model_id), names_sep="_", values_from=val) %>%
  mutate(var = factor(var, levels=unlist(l.var), labels=unlist(l.var_labs)),
         stat = factor(stat, levels=c("coef","tstat"))) %>%
  arrange(var, stat) %>%
  relocate(stat) %>%
  mutate(var = as.character(var),
         var = case_when(stat=="coef" ~ var, TRUE ~ "")) %>%
  mutate_at(vars(2:last_col(offset=1)), ~paste0(., " &")) %>%
  mutate_at(vars(last_col()), ~ifelse(stat=="coef", paste0(., " \\\\"), paste0(., " \\\\ [1.0ex]"))) %>%
  select(-stat)

## Construct table summary
l.sum <- list("fe_yr","fe_st","fe_ut","ctrl_mkt","N","r2")
l.sum_labs <- list("Year FE","State FE","Utility FE","Market Controls",
                   "Observations","R$^{2}$")
tbl_summary <- df.reg %>%
  distinct(lhs, model_id, type, fml, N, r2) %>%
  mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
  mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
  mutate(fe_yr = ifelse(str_detect(fml, "i.year"), "X", ""),
         fe_st = ifelse(str_detect(fml, "i.states"), "X", ""),
         fe_ut = ifelse(str_detect(fml, "i.ut_type"), "X", ""),
         ctrl_mkt = ifelse(str_detect(fml, ctrl_mkt), "X", "")) %>%
  select(lhs, model_id, starts_with("fe"), starts_with("ctrl"), N, r2) %>%
  mutate_at(vars(starts_with("fe"), starts_with("ctrl")),
            ~case_when(is.na(.) ~ "\\textit{}",
                       TRUE ~ paste0("\\textit{", ., "}"))) %>%
  mutate_at(vars(-lhs, -model_id), ~ paste0("\\multicolumn{1}{c}{", ., "}")) %>%
  pivot_longer(cols=c(-lhs, -model_id), names_to="var", values_to="val") %>%
  pivot_wider(names_from=c(lhs, model_id), names_sep="_", values_from=val) %>%
  mutate(var = factor(var, levels=unlist(l.sum), labels=unlist(l.sum_labs))) %>%
  arrange(var) %>%
  mutate_at(vars(1:last_col(offset=1)), ~paste0(., " &")) %>%
  mutate_at(vars(last_col()), ~paste0(., " \\\\"))

## Prepare file header and table header and footer
f_header <- c("%% Grandfathering Project",
              "%% Main regression first-stage table",
              glue::glue("%% Compiled on {base::Sys.time()} using <3_regs.R>"),
              "%% Written by Jack Gregory",
              "\n")
tbl_header <- c("\\begin{table}[ht]",
                "\\centering",
                "\t\\caption{Main regression first-stage results} ",
                "\t\\label{tbl:reg_main_fs}",
                "\\scriptsize\n",
                paste0("\\begin{tabular}{@{\\extracolsep{0.5ex}}l*{", ncol(tbl_colnames)-2,
                       "}{D{.}{.}{-2}}@{}}"),
                "\\\\[-1.8ex] \\hline",
                "\\hline \\\\[-1.8ex]")
tbl_midder <- c("\\hline \\\\[-1.8ex]")
tbl_footer <- c("\\hline",
                "\\hline \\\\[-1.8ex]",
                paste0("\\textit{Notes:} & \\multicolumn{", ncol(tbl_colnames)-2,
                       "}{l}{*** p$<$0.001;  ** p$<$0.01;  * p$<$0.05; ",
                       "\\textit{t}-statistics in parentheses.} \\\\"),
                "\\end{tabular}",
                "\\end{table}")

## Output TeX table file
f_header %>% write_lines(l.file$tbl_main_fs, append=FALSE)
tbl_header %>% write_lines(l.file$tbl_main_fs, append=TRUE)
tbl_colnames %>% write_tsv(l.file$tbl_main_fs, append=TRUE, col_names=FALSE, quote_escape=FALSE)
tbl_midder %>% write_lines(l.file$tbl_main_fs, append=TRUE)
tbl_coef %>% write_tsv(l.file$tbl_main_fs, append=TRUE, col_names=FALSE, quote_escape=FALSE)
tbl_midder %>% write_lines(l.file$tbl_main_fs, append=TRUE)
tbl_summary %>% write_tsv(l.file$tbl_main_fs, append=TRUE, col_names=FALSE, quote_escape=FALSE)
tbl_footer %>% write_lines(l.file$tbl_main_fs, append=TRUE)


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl)_"))
rm(list=ls(pattern="^ctrl_"), fes)
rm(l.lhs, l.rhs, l.cond, l.type, l.var, l.var_labs, l.sum, l.sum_labs)


# MAIN REGRESSIONS (fixest) ----------------------------------------------------------------------

library(fixest)

## Stata global definitions
# global controls_env so2_nonattain applic_reg ARPprice_sp
# global controls_env_interact grand_NSR_in_nonnat_const  applic_reg_const
# global controls_gen_common age max_boi_nameplate 

## fixest conversions of Stata commands
# (1) reg `i' grand_NSR_const $controls_gen_common i.year i.states i.ut_type, vce(robust)
feols(fml=DURATION ~ grand_NSR_const + age + max_boi_nameplate | year + states + ut_type,
      data=df.gf,
      se="hetero") %>%
  summary()

# (2) reg `i' grand_NSR_const capacity_gf $controls_env $controls_env_interact $controls_gen_common \\\
#       i.year i.states i.ut_type, vce(robust)
feols(fml=DURATION ~ grand_NSR_const + age + max_boi_nameplate + 
        i(grand_NSR_const, max_boi_nameplate, 0) + so2_nonattain + applic_reg + ARPprice_sp + 
        grand_NSR_in_nonnat_const + applic_reg_const | year + states + ut_type,
      data=df.gf,
      se="hetero") %>%
  summary()

feols(fml=DURATION ~ grand_NSR_const + age + max_boi_nameplate + 
        i(grand_NSR_const, max_boi_nameplate, 0) + so2_nonattain + applic_reg + ARPprice_sp + 
        i(grand_NSR_const, so2_nonattain, 0) + i(grand_NSR_const, applic_reg, 0) | 
        year + states + ut_type,
      data=df.gf,
      se="hetero") %>%
  summary()


### END CODE ###

