## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Regs -- Robust
## Jack Gregory
## 25 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# PREAMBLE ----------------------------------------------------------------------------------------

## Initiate
## ... Functions
source(here("src/stata_regs.R"))

## ... Date
date <- format(Sys.Date(), "%Y%m%d")
dir_create(here("out", date))

## ... Files
l.file <- list(
  tbl3 = here("out", date, "tbl3.tex"),
  tbl8 = here("out", date, "tbl8.tex"),
  tbl9 = here("out", date, "tbl9.tex")
)


# ROBUST REGRESSIONS ------------------------------------------------------------------------------

## Build specifications -------------------------------------------------------

## Left-hand side vars
l.lhs <- list(
  "DURATION",
  "survive",
  "SO2"
)

## Model names
l.name <- list(
  "Anticipation",
  "Non-linear Age",
  "Spillover",
  "GF vs GF (1)",
  "GF vs GF (2)"
)

## Right-hand side vars
## ... General
ctrl_boiler <- "age capacity efficiency_100_pct_load"
ctrl_mkt <- "state_cap_growth coal2gas_price d_growth"
ctrl_env <- "so2_nonattain applic_reg"
fes <- "i.year i.states i.ut_type i.manufact"

## ... Anticipation
ctrl_ant <- paste("pre_1963 post_1963_Gf",
                  "capacity_pre_1963 capacity_post_1963_GF",
                  "pre1963_in_nonnat_GF post1963_GF_in_nonnat_const",
                  "applic_reg_pre1963 applic_reg_post1963GF",
                  sep=" ")

## ... Anticipation Placebo
ctrl_plb <- paste("pre_Gf post_Gf",
                  "capacity_pre_Gf capacity_post_Gf",
                  "so2_nonat_pre_Gf so2_nonat_post_Gf",
                  "applic_reg_pre_Gf applic_reg_post_Gf",
                  sep=" ")


## ... Non-linear Age
ctrl_gf_int <- "capacity_gf so2_nonat_Gf applic_reg_Gf"
ctrl_sq <- "age_sq"

## ... GF vs GF
ctrl_ar_int <- "capacity_AR so2_nonat_AR"

## ... Build
l.rhs <- list(
  glue("{ctrl_boiler} {ctrl_env} {ctrl_ant} {fes}"),
  glue("Gf {ctrl_boiler} {ctrl_env} {ctrl_gf_int} {ctrl_sq} {fes}"),
  glue("Gf {ctrl_boiler} {ctrl_env} {ctrl_gf_int} {fes}"),
  glue("AR {ctrl_boiler} so2_nonattain {fes}"),
  glue("AR {ctrl_boiler} so2_nonattain {ctrl_ar_int} {fes}")
)

## Market controls
l.ctrl <- list(
  1,1,1,1,1
)

## Regression conditions
l.cond <- list(
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  paste0("if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006) ",
         "& ((Gf==1 & share_gf>=0.7) | (Gf==0 & share_gf<=0.3))"),
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)"
)
cond_survive <- "& ((capacity>0.075 & ut_type==4) | ((ut_type==5|ut_type==2) & year>=1990)) & year<=2017"

## Regression type
l.type <- list(
  "reg","reg","reg","reg","reg"
)


## Perform regressions --------------------------------------------------------

## Build regression dataframe
df.reg <- tibble(l.name, l.type, l.rhs, l.ctrl, l.cond) %>%
  rowid_to_column(var="model_id") %>%
  nest(data=everything()) %>%
  mutate(N = length(l.lhs)) %>%
  uncount(N) %>%
  bind_cols(tibble(l.lhs), .) %>%
  unnest(cols=c(l.lhs, data)) %>%
  rename_with(.fn=~str_replace(., "l\\.", "")) %>%
  mutate(ctrl = ifelse(lhs!="SO2" & ctrl==1, ctrl_mkt, ""),
         cond = as.character(cond),
         sample = ifelse(lhs=="survive", cond_survive, ""),
         fml = paste(lhs, rhs, ctrl, cond, sample, sep=" ")) %>%
  mutate_at(vars(-model_id), as.character) %>%
  mutate(bs = ifelse(type=="iv", TRUE, FALSE)) %>%
  mutate(model = pmap(list(fml, type, bs),
                      ~stata_reg(fml=..1, df=df.gf, type=..2, bootstrap=..3)),
         model = map(model, 
                     ~select(.x, var, coef, stderr, tstat, pval, ci_lower, ci_upper, 
                             N, r2, r2_a))) %>%
  unnest(model)


# ROBUSTNESS TABLE --------------------------------------------------------------------------------

## Build regression table -----------------------------------------------------

## Construct column names
tbl_colnames <- df.reg %>%
  filter(name=="Non-linear Age" | (name=="Spillover" & lhs=="DURATION")
         ) %>%
  distinct(name, lhs) %>%
  mutate(name = factor(name, levels=c("Non-linear Age","Spillover")),
         lhs = factor(lhs, levels=c("DURATION","survive","SO2"), labels=c("Utilization","Survival","Emissions"))) %>%
  arrange(name, lhs) %>%
  rowid_to_column(var="model_id") %>%
  relocate(model_id, .after=lhs) %>%
  mutate(name = case_when(
           name=="Non-linear Age" & lhs=="Utilization" ~ paste0("\\multicolumn{3}{c}{\\textit{", name, "}}"),
           name=="Spillover" & lhs=="Utilization" ~ paste0("\\multicolumn{1}{c}{\\textit{", name, "}}"),
           .default=""),
         lhs = paste0("\\multicolumn{1}{c}{\\textit{", lhs, "}}"),
         model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}")) %>%
  mutate_all(~ifelse(.!="", paste0(., " &"), "")) %>%
  add_row(name=" &", lhs=" &", model_id=" &", .before=1) %>%
  add_row(name="\\\\\\\\[-1.8ex] \\cmidrule{2-4}\\cmidrule{5-5}", lhs="\\\\", model_id="\\\\\\\\[-1.8ex]") %>%
  t() %>%
  as_tibble() %>%
  mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))

## Construct table coefficients
l.var <- list("Gf","capacity","capacity_gf","so2_nonattain","so2_nonat_Gf","applic_reg","applic_reg_Gf")
l.var_labs <- list("GF","size","GF $\\times$ size","NAAQS","GF $\\times$ NAAQS","MMBTU","GF $\\times$ MMBTU")
tbl_coef <- df.reg %>%
  filter(name=="Non-linear Age" | (name=="Spillover" & lhs=="DURATION")
         ) %>%
  filter(var %in% unlist(l.var)) %>%
  mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
  mutate(
    name = factor(name, levels=c("Non-linear Age","Spillover")),
    lhs = factor(lhs, levels=c("DURATION","survive","SO2"), labels=c("Utilization","Survival","Emissions")),
    coef = case_when(
      pval<0.01 ~ paste0(coef, "^{***}"),
      pval<0.05 ~ paste0(coef, "^{**}"),
      pval<0.10 ~ paste0(coef, "^{*}"),
      TRUE ~ as.character(coef)),
    tstat = paste0("(", tstat, ")")) %>%
  arrange(name, lhs) %>%
  select(lhs, model_id, var, coef, tstat) %>%
  pivot_longer(cols=c(coef, tstat), names_to="stat", values_to="val") %>%
  pivot_wider(names_from=c(lhs,model_id), names_sep="_", values_from=val, values_fill="") %>%
  mutate(var = factor(var, levels=unlist(l.var), labels=unlist(l.var_labs)),
         stat = factor(stat, levels=c("coef","tstat"))) %>%
  arrange(var, stat) %>%
  relocate(stat) %>%
  mutate(var = as.character(var), 
         var = case_when(stat=="coef" ~ var, TRUE ~ "")) %>%
  mutate_at(vars(last_col()), ~case_when(var=="GF" ~ paste0(., " \\rule{0pt}{2.6ex} \\\\"),
                                         stat=="coef" ~ paste0(., " \\\\"), 
                                         TRUE ~ paste0(., " \\\\ [1.0ex]"))) %>%
  mutate_at(vars(2:last_col(offset=1)), ~paste0(., " &")) %>%
  select(-stat)

## Construct table summary
l.sum <- list("N","r2")
l.sum_labs <- list("Observations","R$^{2}$")
tbl_summary <- df.reg %>%
  filter(name=="Non-linear Age" | (name=="Spillover" & lhs=="DURATION")
         ) %>%
  distinct(name, lhs, model_id, type, fml, N, r2) %>%
  mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
  mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
  mutate(name = factor(name, levels=c("Non-linear Age","Spillover")),
         lhs = factor(lhs, levels=c("DURATION","survive","SO2"),labels=c("Utilization","Survival","Emissions"))) %>%
  arrange(name, lhs) %>%
  select(lhs, model_id, starts_with("fe"), starts_with("ctrl"), N, r2) %>%
  mutate_at(vars(starts_with("fe"), starts_with("ctrl")), 
            ~case_when(is.na(.) ~ "\\textit{}",
                       TRUE ~ paste0("\\textit{", ., "}"))) %>%
  mutate_at(vars(-lhs, -model_id), ~ paste0("\\multicolumn{1}{c}{", ., "}")) %>%
  pivot_longer(cols=c(-lhs, -model_id), names_to="var", values_to="val") %>%
  pivot_wider(names_from=c(lhs,model_id), names_sep="_", values_from=val) %>%
  mutate(var = factor(var, levels=unlist(l.sum), labels=unlist(l.sum_labs))) %>%
  arrange(var) %>%
  mutate_at(vars(1:last_col(offset=1)), ~paste0(., " &")) %>%
  mutate_at(vars(last_col()), ~paste0(., " \\\\"))

## Prepare file header, table header and footer, and subtable headers
f_header <- c("%% Grandfathering Project",
              "%% Robustness regression table",
              glue::glue("%% Compiled on {base::Sys.time()}"),
              "%% Written by Jack Gregory\n")
tbl_header1 <- c("\\begin{center}",
                 "\\begin{singlespace}",
                 "\\begin{scriptsize}\n",
                 paste0("\\begin{longtable}[c]{@{\\extracolsep{0.0ex}}l*{", 
                        ncol(tbl_colnames)-2, "}{D{.}{.}{-2}}@{\\extracolsep{0.0ex}}}"),
                 "\t\\caption{Robustness regression results}",
                 "\t\\label{tbl:reg_robust}\n",
                 "\\\\",
                 "\\hline\\hline \\\\ [-1.2ex]")
tbl_header2 <- c("\\midrule \\\\",
                 "\\endfirsthead",
                 "\\hline\\hline \\\\ [-1.2ex]")
tbl_header3 <- c("\\midrule \\\\",
                 "\\endhead")
tbl_midder <- c("\\hline \\\\ [-1.8ex]")
txt_footer <- c("Robustness checks based on modified specification (3) from Table (1), including relevant sample. ",
                '"Non-linear Age" regressions additionally control for squared age, ',
                'while the "Spillover" regression uses a sample in which direct interactions ",
                "between non-grandfathered and grandfathered boilers are limited.')
tbl_footer <- c("\\hline\\hline",
                paste0("\\multicolumn{", ncol(tbl_colnames)-1, "}{p{10cm}}{\\textit{Notes:} ", 
                       paste(txt_footer, collapse=""), " Boiler-level clustered standard errors used, with ",
                       "*** p$<$0.01,  ** p$<$0.05,  * p$<$0.10 and ",
                       "\\textit{t}-statistics in parentheses.} \\\\"),
                "\\\\",
                "\\end{longtable}\n",
                "\\end{scriptsize}",
                "\\end{singlespace}",
                "\\end{center}")

## Output TeX table file
f_header %>% write_lines(l.file$tbl8, append=FALSE)

tbl_header1 %>% write_lines(l.file$tbl8, append=TRUE)
tbl_colnames %>% write_tsv(l.file$tbl8, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header2 %>% write_lines(l.file$tbl8, append=TRUE)
tbl_colnames %>% write_tsv(l.file$tbl8, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header3 %>% write_lines(l.file$tbl8, append=TRUE)

tbl_coef %>% write_tsv(l.file$tbl8, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(l.file$tbl8, append=TRUE)
tbl_summary  %>% write_tsv(l.file$tbl8, append=TRUE, col_names=FALSE, escape=NULL)

tbl_footer %>% write_lines(l.file$tbl8, append=TRUE)


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl|subtbl|txt)_"))


# GF vs GF TABLE ----------------------------------------------------------------------------------

## Build regression table -----------------------------------------------------

## Construct column names
tbl_colnames <- df.reg %>%
  filter(name %in% c("GF vs GF (1)","GF vs GF (2)")) %>%
  distinct(name, lhs) %>%
  mutate(lhs = factor(lhs, levels=c("DURATION","survive","SO2"), labels=c("Utilization","Survival","Emissions"))) %>%
  arrange(lhs, name) %>%
  rowid_to_column(var="model_id") %>%
  relocate(model_id, .after=lhs) %>%
  mutate(lhs = ifelse(name=="GF vs GF (2)", paste0("\\multicolumn{2}{c}{\\textit{", lhs, "}}"), ""),
         model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}")) %>%
  select(-name) %>%
  mutate_all(~ifelse(.!="", paste0(., " &"), "")) %>%
  add_row(lhs=" &", model_id=" &", .before=1) %>%
  add_row(lhs="\\\\ \\cmidrule{2-3}\\cmidrule{4-5}\\cmidrule{6-7}", model_id="\\\\\\\\[-1.8ex]") %>%
  t() %>%
  as_tibble() %>%
  mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))

## Construct table coefficients
l.var <- list("Gf",
              "capacity","capacity_gf",
              "so2_nonattain","so2_nonat_Gf",
              "applic_reg","applic_reg_Gf"
)
l.var_labs <- list("GF$^{F}$",
                   "Size","GF$^{F}$ $\\times$ size",
                   "NAAQS","GF$^{F}$ $\\times$ NAAQS",
                   "MMBTU","GF$^{F}$ $\\times$ MMBTU"
)
tbl_coef <- df.reg %>%
  filter(name %in% c("GF vs GF (1)","GF vs GF (2)")) %>%
  mutate(var = case_when(var %in% c("GfA","AR") ~ "Gf",
                         str_detect(var, "capacity_(gfA|AR)") ~ "capacity_gf",
                         str_detect(var, "so2_nonat_(GfA|AR)") ~ "so2_nonat_Gf",
                         str_detect(var, "applic_reg_(GfA|AR)") ~ "applic_reg_Gf",
                         .default=var)) %>%
  filter(var %in% unlist(l.var)) %>%
  mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
  mutate(
    lhs = factor(lhs, levels=c("DURATION","survive","SO2"), labels=c("Utilization","Survival","Emissions")),
    coef = case_when(
      pval<0.01 ~ paste0(coef, "^{***}"),
      pval<0.05 ~ paste0(coef, "^{**}"),
      pval<0.10 ~ paste0(coef, "^{*}"),
      TRUE ~ as.character(coef)),
    tstat = paste0("(", tstat, ")")) %>%
  select(lhs, model_id, var, coef, tstat) %>%
  arrange(lhs, model_id) %>%
  pivot_longer(cols=c(coef, tstat), names_to="stat", values_to="val") %>%
  pivot_wider(names_from=c(lhs, model_id), names_sep="_", values_from=val, values_fill="") %>%
  mutate(var = factor(var, levels=unlist(l.var), labels=unlist(l.var_labs)),
         stat = factor(stat, levels=c("coef","tstat"))) %>%
  arrange(var, stat) %>%
  relocate(stat) %>%
  mutate(var = as.character(var), 
         var = case_when(stat=="coef" ~ var, TRUE ~ "")) %>%
  mutate_at(vars(last_col()), ~case_when(var=="GF" ~ paste0(., " \\rule{0pt}{2.6ex} \\\\"),
                                         stat=="coef" ~ paste0(., " \\\\"), 
                                         TRUE ~ paste0(., " \\\\ [1.0ex]"))) %>%
  mutate_at(vars(2:last_col(offset=1)), ~paste0(., " &")) %>%
  select(-stat)

## Construct table summary
l.sum <- list("ctrl_mkt","ctrl_so2","N","r2")
l.sum_labs <- list("Market Controls","Sulfur Controls","Observations","R$^{2}$")
tbl_summary <- df.reg %>%
  filter(name %in% c("GF vs GF (1)","GF vs GF (2)")) %>%
  distinct(lhs, model_id, type, fml, N, r2) %>%
  mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
  mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
  mutate(lhs = factor(lhs, levels=c("DURATION","survive","SO2"),labels=c("Utilization","Survival","Emissions"))) %>%
  select(lhs, model_id, starts_with("fe"), starts_with("ctrl"), N, r2) %>%
  mutate_at(vars(starts_with("fe"), starts_with("ctrl")), 
            ~case_when(is.na(.) ~ "\\textit{}",
                       TRUE ~ paste0("\\textit{", ., "}"))) %>%
  mutate_at(vars(-lhs, -model_id), ~ paste0("\\multicolumn{1}{c}{", ., "}")) %>%
  arrange(lhs, model_id) %>%
  pivot_longer(cols=c(-lhs, -model_id), names_to="var", values_to="val") %>%
  pivot_wider(names_from=c(lhs,model_id), names_sep="_", values_from=val) %>%
  mutate(var = factor(var, levels=unlist(l.sum), labels=unlist(l.sum_labs))) %>%
  arrange(var) %>%
  mutate_at(vars(1:last_col(offset=1)), ~paste0(., " &")) %>%
  mutate_at(vars(last_col()), ~paste0(., " \\\\"))

## Prepare file header, table header and footer, and subtable headers
f_header <- c("%% Grandfathering Project",
              "%% Fully versus partially grandfathered regression table",
              glue::glue("%% Compiled on {base::Sys.time()}"),
              "%% Written by Jack Gregory\n")
tbl_header1 <- c("\\begin{center}",
                 "\\begin{singlespace}",
                 "\\begin{scriptsize}\n",
                 paste0("\\begin{longtable}[c]{@{\\extracolsep{2.5ex}}l*{", 
                        ncol(tbl_colnames)-2, "}{D{.}{.}{-2}}@{\\extracolsep{2.5ex}}}"),
                 "\t\\caption{Fully versus partially grandfathered regression results}",
                 "\t\\label{tbl:reg_gfvsgf}\n",
                 "\\\\",
                 "\\hline\\hline \\\\ [-1.2ex]")
tbl_header2 <- c("\\midrule \\\\",
                 "\\endfirsthead",
                 "\\hline\\hline \\\\ [-1.2ex]")
tbl_header3 <- c("\\midrule \\\\",
                 "\\endhead")
tbl_midder <- c("\\hline \\\\ [-1.8ex]")
txt_footer <- c("OLS regressions comparing fully grandfathered boilers (GF$^{F}$) with partially grandfathered boilers. ",
                "The sample used and non-environmental controls (i.e., boiler-level and market-level controls as well as ",
                "year, state, utility type and manufacturer fixed effects) as in specification (3) in Table (1).")
tbl_footer <- c("\\hline\\hline",
                paste0("\\multicolumn{", ncol(tbl_colnames)-1, "}{p{16cm}}{\\textit{Notes:} ", 
                       paste(txt_footer, collapse=""), " Boiler-level clustered standard errors used, with ",
                       "*** p$<$0.01,  ** p$<$0.05,  * p$<$0.10 and ",
                       "\\textit{t}-statistics in parentheses.} \\\\"),
                "\\\\",
                "\\end{longtable}\n",
                "\\end{scriptsize}",
                "\\end{singlespace}",
                "\\end{center}")

## Output TeX table file
f_header %>% write_lines(l.file$tbl3, append=FALSE)

tbl_header1 %>% write_lines(l.file$tbl3, append=TRUE)
tbl_colnames %>% write_tsv(l.file$tbl3, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header2 %>% write_lines(l.file$tbl3, append=TRUE)
tbl_colnames %>% write_tsv(l.file$tbl3, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header3 %>% write_lines(l.file$tbl3, append=TRUE)

tbl_coef %>% write_tsv(l.file$tbl3, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(l.file$tbl3, append=TRUE)
tbl_summary  %>% write_tsv(l.file$tbl3, append=TRUE, col_names=FALSE, escape=NULL)

tbl_footer %>% write_lines(l.file$tbl3, append=TRUE)


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl|subtbl|txt)_"))


# ANTICIPATION TABLE ------------------------------------------------------------------------------

## Build regression table -----------------------------------------------------

## Construct column names
tbl_colnames <- df.reg %>%
  filter(name %in% c("Anticipation")) %>%
  distinct(name, lhs) %>%
  mutate(name = factor(name, levels=c("Anticipation","Placebo"), labels=c("Anticipation (1963)","Anticipation Placebo (1957)")),
         lhs = factor(lhs, levels=c("DURATION","survive","SO2"), labels=c("Utilization","Survival","Emissions"))) %>%
  arrange(name, lhs) %>%
  rowid_to_column(var="model_id") %>%
  relocate(model_id, .after=lhs) %>%
  mutate(name = ifelse(lhs=="Emissions", paste0("\\multicolumn{3}{c}{\\textit{", name, "}}"), ""),
         lhs = paste0("\\multicolumn{1}{c}{\\textit{", lhs, "}}"),
         model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}")) %>%
  mutate_all(~ifelse(.!="", paste0(., " &"), "")) %>%
  add_row(name=" &", lhs=" &", model_id=" &", .before=1) %>%
  add_row(name="\\\\\\\\[-1.8ex] \\cmidrule{2-4}", lhs="\\\\", model_id="\\\\\\\\[-1.8ex]") %>%
  t() %>%
  as_tibble() %>%
  mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))

## Construct table coefficients
l.var <- list("pre_1963","post_1963_Gf","pre_Gf","post_Gf",
              "capacity","capacity_pre_1963","capacity_post_1963_GF","capacity_pre_Gf","capacity_post_Gf",
              "so2_nonattain","pre1963_in_nonnat_GF","post1963_GF_in_nonnat_const","so2_nonat_pre_Gf","so2_nonat_post_Gf",
              "applic_reg","applic_reg_pre1963","applic_reg_post1963GF","applic_reg_pre_Gf","applic_reg_post_Gf"
)
l.var_labs <- list("Pre-period (GF)","Post-period (GF)","Pre-period (GF)","Post-period (GF)",
                   "Size","Size $\\times$ pre-period (GF)","Size $\\times$ post-period (GF)",
                   "Size $\\times$ pre-period (GF)","Size $\\times$ post-period (GF)",
                   "NAAQS","NAAQS $\\times$ pre-period (GF)","NAAQS $\\times$ post-period (GF)",
                   "NAAQS $\\times$ pre-period (GF)","NAAQS $\\times$ post-period (GF)",
                   "MMBTU","MMBTU $\\times$ pre-period (GF)","MMBTU $\\times$ post-period (GF)",
                   "MMBTU $\\times$ pre-period (GF)","MMBTU $\\times$ post-period (GF)"
)
tbl_coef <- df.reg %>%
  filter(name %in% c("Anticipation"#,"Placebo"
                     )) %>%
  mutate(var = case_match(var,
                          "pre_1963" ~ "pre_Gf",
                          "post_1963_Gf" ~ "post_Gf",
                          "capacity_pre_1963" ~ "capacity_pre_Gf",
                          "capacity_post_1963_GF" ~ "capacity_post_Gf",
                          "pre1963_in_nonnat_GF" ~ "so2_nonat_pre_Gf",
                          "post1963_GF_in_nonnat_const" ~ "so2_nonat_post_Gf",
                          "applic_reg_pre1963" ~ "applic_reg_pre_Gf",
                          "applic_reg_post1963GF" ~ "applic_reg_post_Gf",
                         .default=var)) %>%
  filter(var %in% unlist(l.var)) %>%
  mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
  mutate(
    name = factor(name, levels=c("Anticipation","Placebo")),
    lhs = factor(lhs, levels=c("DURATION","survive","SO2"), labels=c("Utilization","Survival","Emissions")),
    coef = case_when(
      pval<0.01 ~ paste0(coef, "^{***}"),
      pval<0.05 ~ paste0(coef, "^{**}"),
      pval<0.10 ~ paste0(coef, "^{*}"),
      TRUE ~ as.character(coef)),
    tstat = paste0("(", tstat, ")")) %>%
  arrange(name, lhs) %>%
  select(lhs, model_id, var, coef, tstat) %>%
  pivot_longer(cols=c(coef, tstat), names_to="stat", values_to="val") %>%
  pivot_wider(names_from=c(lhs,model_id), names_sep="_", values_from=val, values_fill="") %>%
  mutate(var = factor(var, levels=unlist(l.var), labels=unlist(l.var_labs)),
         stat = factor(stat, levels=c("coef","tstat"))) %>%
  arrange(var, stat) %>%
  relocate(stat) %>%
  mutate(var = as.character(var), 
         var = case_when(stat=="coef" ~ var, TRUE ~ "")) %>%
  mutate_at(vars(last_col()), ~case_when(var=="GF" ~ paste0(., " \\rule{0pt}{2.6ex} \\\\"),
                                         stat=="coef" ~ paste0(., " \\\\"), 
                                         TRUE ~ paste0(., " \\\\ [1.0ex]"))) %>%
  mutate_at(vars(2:last_col(offset=1)), ~paste0(., " &")) %>%
  select(-stat)

## Construct table summary
l.sum <- list("N","r2")
l.sum_labs <- list("Observations","R$^{2}$")
tbl_summary <- df.reg %>%
  filter(name %in% c("Anticipation")) %>%
  distinct(name, lhs, model_id, type, fml, N, r2) %>%
  mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
  mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
  mutate(name = factor(name, levels=c("Anticipation","Placebo")),
         lhs = factor(lhs, levels=c("DURATION","survive","SO2"),labels=c("Utilization","Survival","Emissions"))) %>%
  arrange(name, lhs) %>%
  select(lhs, model_id, starts_with("fe"), starts_with("ctrl"), N, r2) %>%
  mutate_at(vars(starts_with("fe"), starts_with("ctrl")), 
            ~case_when(is.na(.) ~ "\\textit{}",
                       TRUE ~ paste0("\\textit{", ., "}"))) %>%
  mutate_at(vars(-lhs, -model_id), ~ paste0("\\multicolumn{1}{c}{", ., "}")) %>%
  pivot_longer(cols=c(-lhs, -model_id), names_to="var", values_to="val") %>%
  pivot_wider(names_from=c(lhs,model_id), names_sep="_", values_from=val) %>%
  mutate(var = factor(var, levels=unlist(l.sum), labels=unlist(l.sum_labs))) %>%
  arrange(var) %>%
  mutate_at(vars(1:last_col(offset=1)), ~paste0(., " &")) %>%
  mutate_at(vars(last_col()), ~paste0(., " \\\\"))

## Prepare file header, table header and footer, and subtable headers
f_header <- c("%% Grandfathering Project",
              "%% Anticipation regression table",
              glue::glue("%% Compiled on {base::Sys.time()}"),
              "%% Written by Jack Gregory\n")
tbl_header1 <- c("\\begin{center}",
                 "\\begin{singlespace}",
                 "\\begin{scriptsize}\n",
                 paste0("\\begin{longtable}[c]{@{\\extracolsep{0.0ex}}l*{", 
                        ncol(tbl_colnames)-2, "}{D{.}{.}{-2}}@{\\extracolsep{0.0ex}}}"),
                 "\t\\caption{Anticipation regression results}",
                 "\t\\label{tbl:reg_anticipation}\n",
                 "\\\\",
                 "\\hline\\hline \\\\ [-1.2ex]")
tbl_header2 <- c("\\midrule \\\\",
                 "\\endfirsthead",
                 "\\hline\\hline \\\\ [-1.2ex]")
tbl_header3 <- c("\\midrule \\\\",
                 "\\endhead")
tbl_midder <- c("\\hline \\\\ [-1.8ex]")
txt_footer <- c("Robustness checks based on modified specification (3) from Table (1), including relevant sample. ",
                '"Anticipation" regressions allow the grandfathering effects to differ between boilers ',
                "with an inservice year before and after the 1963 cutoff year. ")
tbl_footer <- c("\\hline\\hline",
                paste0("\\multicolumn{", ncol(tbl_colnames)-1, "}{p{10cm}}{\\textit{Notes:} ", 
                       paste(txt_footer, collapse=""), " Boiler-level clustered standard errors used, with ",
                       "*** p$<$0.01,  ** p$<$0.05,  * p$<$0.10 and ",
                       "\\textit{t}-statistics in parentheses.} \\\\"),
                "\\\\",
                "\\end{longtable}\n",
                "\\end{scriptsize}",
                "\\end{singlespace}",
                "\\end{center}")

## Output TeX table file
f_header %>% write_lines(l.file$tbl9, append=FALSE)

tbl_header1 %>% write_lines(l.file$tbl9, append=TRUE)
tbl_colnames %>% write_tsv(l.file$tbl9, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header2 %>% write_lines(l.file$tbl9, append=TRUE)
tbl_colnames %>% write_tsv(l.file$tbl9, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header3 %>% write_lines(l.file$tbl9, append=TRUE)

tbl_coef %>% write_tsv(l.file$tbl9, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(l.file$tbl9, append=TRUE)
tbl_summary  %>% write_tsv(l.file$tbl9, append=TRUE, col_names=FALSE, escape=NULL)

tbl_footer %>% write_lines(l.file$tbl9, append=TRUE)


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl|subtbl|txt)_"))
rm(list=ls(pattern="^ctrl_"), fes)
rm(l.lhs, l.name, l.rhs, l.ctrl, l.cond, cond_survive, l.type, l.var, l.var_labs, l.sum, l.sum_labs)


### END CODE ###

