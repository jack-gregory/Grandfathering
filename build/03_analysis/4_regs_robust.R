## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Regs -- Robust
## Jack Gregory
## 25 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# PREAMBLE ----------------------------------------------------------------------------------------

## Initiate regression functions
source(here("src/stata_regs.R"))


## Create output folder
date <- format(Sys.Date(), "%Y%m%d")
# date <- "20240629"
dir_create(path(l.path$out, date))


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
  # "Alternative",
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

## ... Non-linear Age
ctrl_gf_int <- "capacity_gf so2_nonat_Gf applic_reg_Gf"
ctrl_sq <- "age_sq"

## ... Alternative
ctrl_gfa_int <- "capacity_gfA so2_nonat_GfA applic_reg_GfA"

## ... GF vs GF
ctrl_ar_int <- "capacity_AR so2_nonat_AR"

## ... Build
l.rhs <- list(
  glue("{ctrl_boiler} {ctrl_env} {ctrl_ant} {fes}"),
  glue("Gf {ctrl_boiler} {ctrl_env} {ctrl_gf_int} {ctrl_sq} {fes}"),
  # glue("GfA {ctrl_boiler} {ctrl_env} {ctrl_gf_int} {ctrl_sq} {fes}"),
  glue("AR {ctrl_boiler} so2_nonattain {fes}"),
  glue("AR {ctrl_boiler} so2_nonattain {ctrl_ar_int} {fes}")
)

## Market controls
l.ctrl <- list(
  1,1,1,1#,1
)

## Regression conditions
l.cond <- list(
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  # "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)"
)
cond_survive <- "& ((capacity>0.075 & ut_type==4) | ((ut_type==5|ut_type==2) & year>=1990)) & year<=2017"

## Regression type
l.type <- list(
  "reg","reg","reg","reg"#,"reg"
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


# COMBINED TABLE ----------------------------------------------------------------------------------

## Build regression table -----------------------------------------------------
# 
# ## Construct column names
# tbl_colnames <- df.reg %>%
#   distinct(model_id, name, type) %>%
#   mutate(model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}"),
#          name = paste0("\\multicolumn{1}{c}{\\textit{", name, "}}"),
#          type = paste0("\\multicolumn{1}{c}{\\textit{", type, "}}")) %>%
#   mutate_all(~paste0(., " &")) %>%
#   add_row(model_id=" &", name=" &", type=" &", .before=1) %>%
#   add_row(model_id="\\\\\\\\[-1.8ex]", name="\\\\\\\\[-1.8ex]", type="\\\\") %>%
#   t() %>%
#   as_tibble() %>%
#   mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))
# 
# ## Construct table coefficients
# l.var <- list("Gf","pre_1963","post_1963_Gf",#"GfA","AR",
#               "capacity","capacity_gf","capacity_pre_1963","capacity_post_1963_GF",#"capacity_gfA","capacity_AR",
#               "so2_nonattain","so2_nonat_Gf","pre1963_in_nonnat_GF","post1963_GF_in_nonnat_const",#"so2_nonat_GfA","so2_nonat_AR",
#               "applic_reg","applic_reg_Gf","applic_reg_pre1963","applic_reg_post1963GF"#,"applic_reg_GfA"
#               )
# l.var_labs <- list("GF","Pre-1963","GF $\\times$ post-1962",#"GFA","GFE",
#                    "Size","GF $\\times$ size","Size $\\times$ pre-1963","GF $\\times$ size $\\times$ post-1962",
#                    #"GFA $\\times$ size","GFE $\\times$ size",
#                    "NAAQS","GF $\\times$ NAAQS","NAAQS $\\times$ pre-1963","GF $\\times$ NAAQS $\\times$ post-1962",
#                    #"GFA $\\times$ NAAQS","GFE $\\times$ NAAQS",
#                    "MMBTU","GF $\\times$ MMBTU","MMBTU $\\times$ pre-1963","GF $\\times$ MMBTU $\\times$ post-1962"#,
#                    #"GFA $\\times$ MMBTU"
#                    )
# 
# tbl_coef <- df.reg %>%
#   mutate(var = case_when(var %in% c("GfA","AR") ~ "Gf",
#                          str_detect(var, "capacity_(gfA|AR)") ~ "capacity_gf",
#                          str_detect(var, "so2_nonat_(GfA|AR)") ~ "so2_nonat_Gf",
#                          str_detect(var, "applic_reg_(GfA|AR)") ~ "applic_reg_Gf",
#                          .default=var)) %>%
#   filter(var %in% unlist(l.var)) %>%
#   mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
#   mutate(coef = case_when(
#     pval<0.01 ~ paste0(coef, "^{***}"),
#     pval<0.05 ~ paste0(coef, "^{**}"),
#     pval<0.10 ~ paste0(coef, "^{*}"),
#     TRUE ~ as.character(coef)),
#     tstat = paste0("(", tstat, ")")) %>%
#   select(lhs, model_id, var, coef, tstat) %>%
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
#   mutate_at(vars(last_col()), ~case_when(var=="GF" ~ paste0(., " \\rule{0pt}{2.6ex} \\\\"),
#                                          stat=="coef" ~ paste0(., " \\\\"), 
#                                          TRUE ~ paste0(., " \\\\ [1.0ex]"))) %>%
#   mutate_at(vars(3:last_col(offset=1)), ~paste0(., " &")) %>%
#   select(-stat)
# 
# ## Construct table summary
# l.sum <- list(#"fe_yr","fe_st","fe_ut","fe_man",
#               "ctrl_mkt","ctrl_so2","N","r2")
# l.sum_labs <- list(#"Year FE","State FE","Utility FE","Manufacture FE",
#                    "Market Controls","Sulfur Controls","Observations","R$^{2}$")
# tbl_summary <- df.reg %>%
#   distinct(lhs, model_id, type, fml, N, r2) %>%
#   mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
#   mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
#   mutate(#fe_yr = ifelse(str_detect(fml, "i.year"), "X", ""),
#          #fe_st = ifelse(str_detect(fml, "i.states"), "X", ""),
#          #fe_ut = ifelse(str_detect(fml, "i.ut_type"), "X", ""),
#          #fe_man = ifelse(str_detect(fml, "i.manufact"), "X", ""),
#          ctrl_mkt = ifelse(str_detect(fml, ctrl_mkt), "X", ""),
#          ctrl_so2 = ifelse((type=="iv"|str_detect(fml, "sulfur_content_tot")), "X", "")) %>%
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
# ## Prepare file header, table header and footer, and subtable headers
# f_header <- c("%% Grandfathering Project",
#               "%% Robustness regression table",
#               glue::glue("%% Compiled on {base::Sys.time()} using <5_regs_robust.R>"),
#               "%% Written by Jack Gregory\n")
# tbl_header1 <- c("\\begin{center}",
#                  "\\begin{singlespace}",
#                  "\\begin{scriptsize}\n",
#                  paste0("\\begin{longtable}[c]{@{\\extracolsep{2.5ex}}l*{", 
#                         ncol(tbl_colnames)-2, "}{D{.}{.}{-2}}@{\\extracolsep{2.5ex}}}"),
#                  "\t\\caption{Robustness regression results}",
#                  "\t\\label{tbl:reg_robust}\n",
#                  "\\\\",
#                  "\\hline\\hline \\\\ [-1.2ex]")
# tbl_header2 <- c("\\midrule \\\\",
#                  "\\endfirsthead",
#                  "\\hline\\hline \\\\ [-1.2ex]")
# tbl_header3 <- c("\\midrule \\\\",
#                  "\\endhead")
# subtbl_headerA <- c(paste0("\\multicolumn{", ncol(tbl_colnames)-1, 
#                            "}{l}{\\textbf{Panel A:  Utilization} [hours per year]} \\\\ [1.0ex]"),
#                     "\\hline")
# subtbl_headerB <- c("\\hline \\\\",
#                     paste0("\\multicolumn{", ncol(tbl_colnames)-1, 
#                            "}{l}{\\textbf{Panel B:  Survival} [percentage points]} \\\\ [1.0ex]"),
#                     "\\hline")
# subtbl_headerC <- c("\\hline \\\\",
#                     # "\\pagebreak",
#                     paste0("\\multicolumn{", ncol(tbl_colnames)-1, 
#                            "}{l}{\\textbf{Panel C:  Emissions} [SO$_2$ lbs per MW of capacity per hour run]} \\\\ [1.0ex]"),
#                     "\\hline")
# tbl_midder <- c("\\hline \\\\ [-1.8ex]")
# txt_footer <- c("This table presents our main regression results for our three channels, namely: ",
#             "utilization, survival and emissions. ",
#             "Specifications (1)-(3) are estimated using OLS, while specifications (4)-(6) use ",
#             "2SLS and leverage sulfur content of the available coal weighted by network distance as an instrument ",
#             "for sulfur content of the combusted coal. The unit of observation is boiler-year. ",
#             "The sample is restricted to those boilers with inservice years between 1950 and 2006, inclusive. ",
#             "The $IOU$ column restricts the sample to boilers belonging to IOUs; ",
#             "the $IOU+$ columns expand to include commercial, industrial and IOU boilers; while, ",
#             "the $All$ column uses all types of available boilers. ",
#             "All regressions include boiler controls as well as year, state, utility type and manufacture type fixed effects. ",
#             "Utilization and emissions estimations use data from 1995-2018, ",
#             "while survival estimations use 1985-2017.")
# tbl_footer <- c("\\hline\\hline",
#                 paste0("\\multicolumn{", ncol(tbl_colnames)-1, "}{p{16cm}}{\\textit{Notes:} ", 
#                        paste(txt_footer, collapse=""), " Boiler-level clustered standard errors used, with ",
#                        "*** p$<$0.01,  ** p$<$0.05,  * p$<$0.10 and ",
#                        "\\textit{t}-statistics in parentheses.} \\\\"),
#                 "\\\\",
#                 "\\end{longtable}\n",
#                 "\\end{scriptsize}",
#                 "\\end{singlespace}",
#                 "\\end{center}")
# 
# ## Output TeX table file
# file <- here::here(l.path$out, date, "tbl.reg_robust.tex")
# f_header %>% write_lines(file, append=FALSE)
# tbl_header1 %>% write_lines(file, append=TRUE)
# tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
# tbl_header2 %>% write_lines(file, append=TRUE)
# tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
# tbl_header3 %>% write_lines(file, append=TRUE)
# 
# subtbl_headerA %>% write_lines(file, append=TRUE)
# tbl_coef %>% 
#   filter(lhs=="Utilization") %>%
#   select(-lhs) %>%
#   write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
# tbl_midder %>% write_lines(file, append=TRUE)
# tbl_summary %>% 
#   filter(lhs=="Utilization") %>%
#   select(-lhs) %>%
#   write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
# 
# subtbl_headerB %>% write_lines(file, append=TRUE)
# tbl_coef %>% 
#   filter(lhs=="Survival") %>%
#   select(-lhs) %>%
#   write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
# tbl_midder %>% write_lines(file, append=TRUE)
# tbl_summary %>% 
#   filter(lhs=="Survival") %>%
#   select(-lhs) %>%
#   write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
# 
# subtbl_headerC %>% write_lines(file, append=TRUE)
# tbl_coef %>%
#   filter(lhs=="Emissions") %>%
#   select(-lhs) %>%
#   write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
# tbl_midder %>% write_lines(file, append=TRUE)
# tbl_summary %>%
#   filter(lhs=="Emissions") %>%
#   select(-lhs) %>%
#   write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
# 
# tbl_footer %>% write_lines(file, append=TRUE)


## Clean environment ------------------------------------------------------------------------------
# rm(list=ls(pattern="^(f|tbl|subtbl|txt)_"))
# rm(list=ls(pattern="^ctrl_"), fes, file)
# rm(l.lhs, l.name, l.rhs, l.ctrl, l.cond, cond_survive, l.type, l.var, l.var_labs, l.sum, l.sum_labs)


# FIRST TABLE -------------------------------------------------------------------------------------

## Build regression table -----------------------------------------------------

## Construct column names
tbl_colnames <- df.reg %>%
  filter(name %in% c("Anticipation","Non-linear Age")) %>%
  distinct(name, lhs) %>%
  mutate(name = factor(name, levels=c("Non-linear Age","Anticipation")),
         lhs = factor(lhs, levels=c("DURATION","survive","SO2"), labels=c("Utilization","Survival","Emissions"))) %>%
  arrange(name, lhs) %>%
  rowid_to_column(var="model_id") %>%
  relocate(model_id, .after=lhs) %>%
  mutate(name = ifelse(lhs=="Emissions", paste0("\\multicolumn{3}{c}{\\textit{", name, "}}"), ""),
         lhs = paste0("\\multicolumn{1}{c}{\\textit{", lhs, "}}"),
         model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}")) %>%
  mutate_all(~ifelse(.!="", paste0(., " &"), "")) %>%
  add_row(name=" &", lhs=" &", model_id=" &", .before=1) %>%
  add_row(name="\\\\\\\\[-1.8ex] \\cmidrule{2-4}\\cmidrule{5-7}", lhs="\\\\", model_id="\\\\\\\\[-1.8ex]") %>%
  t() %>%
  as_tibble() %>%
  mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))

## Construct table coefficients
l.var <- list("Gf","pre_1963","post_1963_Gf",#"GfA","AR",
              "capacity","capacity_gf","capacity_pre_1963","capacity_post_1963_GF",#"capacity_gfA","capacity_AR",
              "so2_nonattain","so2_nonat_Gf","pre1963_in_nonnat_GF","post1963_GF_in_nonnat_const",#"so2_nonat_GfA","so2_nonat_AR",
              "applic_reg","applic_reg_Gf","applic_reg_pre1963","applic_reg_post1963GF"#,"applic_reg_GfA"
)
l.var_labs <- list("GF","Pre-1963 (GF)","Post-1962 (GF)",#"GFA","GFE",
                   "Size","GF $\\times$ size","Size $\\times$ pre-1963 (GF)","Size $\\times$ post-1962 (GF)",
                   #"GFA $\\times$ size","GFE $\\times$ size",
                   "NAAQS","GF $\\times$ NAAQS","NAAQS $\\times$ pre-1963 (GF)","NAAQS $\\times$ post-1962 (GF)",
                   #"GFA $\\times$ NAAQS","GFE $\\times$ NAAQS",
                   "MMBTU","GF $\\times$ MMBTU","MMBTU $\\times$ pre-1963 (GF)","MMBTU $\\times$ post-1962 (GF)"#,
                   #"GFA $\\times$ MMBTU"
)
tbl_coef <- df.reg %>%
  filter(name %in% c("Anticipation","Non-linear Age")) %>%
  mutate(var = case_when(var %in% c("GfA","AR") ~ "Gf",
                         str_detect(var, "capacity_(gfA|AR)") ~ "capacity_gf",
                         str_detect(var, "so2_nonat_(GfA|AR)") ~ "so2_nonat_Gf",
                         str_detect(var, "applic_reg_(GfA|AR)") ~ "applic_reg_Gf",
                         .default=var)) %>%
  filter(var %in% unlist(l.var)) %>%
  mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
  mutate(
    name = factor(name, levels=c("Non-linear Age","Anticipation")),
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
l.sum <- list(#"fe_yr","fe_st","fe_ut","fe_man",
  "ctrl_mkt","ctrl_so2","N","r2")
l.sum_labs <- list(#"Year FE","State FE","Utility FE","Manufacture FE",
  "Market Controls","Sulfur Controls","Observations","R$^{2}$")
tbl_summary <- df.reg %>%
  filter(name %in% c("Anticipation","Non-linear Age")) %>%
  distinct(name, lhs, model_id, type, fml, N, r2) %>%
  mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
  mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
  mutate(name = factor(name, levels=c("Non-linear Age","Anticipation")),
         lhs = factor(lhs, levels=c("DURATION","survive","SO2"),labels=c("Utilization","Survival","Emissions"))) %>%
  # mutate(fe_yr = ifelse(str_detect(fml, "i.year"), "X", ""),
  #        fe_st = ifelse(str_detect(fml, "i.states"), "X", ""),
  #        fe_ut = ifelse(str_detect(fml, "i.ut_type"), "X", ""),
  #        fe_man = ifelse(str_detect(fml, "i.manufact"), "X", ""),
  #        ctrl_mkt = ifelse(str_detect(fml, ctrl_mkt), "X", ""),
  #        ctrl_so2 = ifelse((type=="iv"|str_detect(fml, "sulfur_content_tot")), "X", "")) %>%
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
              glue::glue("%% Compiled on {base::Sys.time()} using <5_regs_robust.R>"),
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
txt_footer <- c("Robustness checks based on specification (3) from Table (1), including relevant sample. ",
                '"Non-linear Age" regressions additionally control for squared age and squared capacity, ',
                'while "Anticipation" regressions allow the grandfathering effects to differ between boilers ',
                "with an inservice year before and after 1963. ")
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
file <- here::here(l.path$out, date, "tbl.reg_robust.tex")
f_header %>% write_lines(file, append=FALSE)

tbl_header1 %>% write_lines(file, append=TRUE)
tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header2 %>% write_lines(file, append=TRUE)
tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header3 %>% write_lines(file, append=TRUE)

tbl_coef %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_summary  %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)

tbl_footer %>% write_lines(file, append=TRUE)


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl|subtbl|txt)_"))
rm(file)
   

# SECOND TABLE ------------------------------------------------------------------------------------

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
l.sum <- list(#"fe_yr","fe_st","fe_ut","fe_man",
  "ctrl_mkt","ctrl_so2","N","r2")
l.sum_labs <- list(#"Year FE","State FE","Utility FE","Manufacture FE",
  "Market Controls","Sulfur Controls","Observations","R$^{2}$")
tbl_summary <- df.reg %>%
  filter(name %in% c("GF vs GF (1)","GF vs GF (2)")) %>%
  distinct(lhs, model_id, type, fml, N, r2) %>%
  mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
  mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
  mutate(lhs = factor(lhs, levels=c("DURATION","survive","SO2"),labels=c("Utilization","Survival","Emissions"))) %>%
  # mutate(fe_yr = ifelse(str_detect(fml, "i.year"), "X", ""),
  #        fe_st = ifelse(str_detect(fml, "i.states"), "X", ""),
  #        fe_ut = ifelse(str_detect(fml, "i.ut_type"), "X", ""),
  #        fe_man = ifelse(str_detect(fml, "i.manufact"), "X", ""),
  #        ctrl_mkt = ifelse(str_detect(fml, ctrl_mkt), "X", ""),
  #        ctrl_so2 = ifelse((type=="iv"|str_detect(fml, "sulfur_content_tot")), "X", "")) %>%
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
              glue::glue("%% Compiled on {base::Sys.time()} using <5_regs_robust.R>"),
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
file <- here::here(l.path$out, date, "tbl.reg_gfvsgf.tex")
f_header %>% write_lines(file, append=FALSE)

tbl_header1 %>% write_lines(file, append=TRUE)
tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header2 %>% write_lines(file, append=TRUE)
tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header3 %>% write_lines(file, append=TRUE)

tbl_coef %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_summary  %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)

tbl_footer %>% write_lines(file, append=TRUE)


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl|subtbl|txt)_"))
rm(list=ls(pattern="^ctrl_"), fes, file)
rm(l.lhs, l.name, l.rhs, l.ctrl, l.cond, cond_survive, l.type, l.var, l.var_labs, l.sum, l.sum_labs)


# ANTICIPATION PLACEBOS ---------------------------------------------------------------------------

## Add variables ----------------------------------------------------------------------------------

# yr <- 1957
yr <- 1963
# yr <- 1967

df.placebo <- df.gf |>
  mutate(pre_Gf = (inservice_y<yr) * Gf,
         post_Gf = (inservice_y>=yr) * Gf,
         capacity_pre_Gf = capacity * pre_Gf,
         capacity_post_Gf = capacity * post_Gf,
         so2_nonat_pre_Gf = so2_nonattain * pre_Gf,
         so2_nonat_post_Gf = so2_nonattain * post_Gf,
         applic_reg_pre_Gf = applic_reg * pre_Gf,
         applic_reg_post_Gf = applic_reg * post_Gf)


## Build specifications -------------------------------------------------------

## Left-hand side vars
l.lhs <- list(
  "DURATION",
  "survive",
  "SO2"
)

## Model names
l.name <- list(
  "Anticipation"
)

## Right-hand side vars
## ... General
ctrl_boiler <- "age capacity efficiency_100_pct_load"
ctrl_mkt <- "state_cap_growth coal2gas_price d_growth"
ctrl_env <- "so2_nonattain applic_reg"
fes <- "i.year i.states i.ut_type i.manufact"

## ... Anticipation
ctrl_ant <- paste("pre_Gf post_Gf",
                  "capacity_pre_Gf capacity_post_Gf",
                  "so2_nonat_pre_Gf so2_nonat_post_Gf",
                  "applic_reg_pre_Gf applic_reg_post_Gf",
                  sep=" ")

## ... Build
l.rhs <- list(glue("{ctrl_boiler} {ctrl_env} {ctrl_ant} {fes}"))

## Market controls
l.ctrl <- list(1)

## Regression conditions
l.cond <- list("if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)")
cond_survive <- "& ((capacity>0.075 & ut_type==4) | ((ut_type==5|ut_type==2) & year>=1990)) & year<=2017"

## Regression type
l.type <- list(
  "reg"
)


## Perform regressions --------------------------------------------------------

## Create bespoke regression function
stata_reg_robust <- function(fml, df, ...) {
  
  cat("\nstata::reg  ", fml, "\n", sep="")
  
  ## Create Stata call
  stata_do <- 
    glue('eststo: reg {fml}, vce(cluster ID)
          test pre_Gf=post_Gf
          test capacity_pre_Gf=capacity_post_Gf
          test so2_nonat_pre_Gf=so2_nonat_post_Gf
          test applic_reg_pre_Gf=applic_reg_post_Gf
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
                      ~stata_reg_robust(fml=..1, df=df.placebo)),
         model = map(model, 
                     ~select(.x, var, coef, stderr, tstat, pval, ci_lower, ci_upper, 
                             N, r2, r2_a))) %>%
  unnest(model)


### END CODE ###


# stata_do <- "eststo: imperfectiv DURATION Gf age capacity capacity_gf efficiency_100_pct_load so2_nonattain so2_nonat_Gf applic_reg applic_reg_Gf ARPprice year* states* ut_type* manufact* state_cap_growth coal2gas_price d_growth (slfr = sulfur_net_iv) if (inservice_y>=1950 & inservice_y<=2006), vce(cluster ID) exogvars(Gf capacity_gf so2_nonat_Gf applic_reg_Gf)
#              *regsave, tstat pval ci detail(scalars)"
# stata(stata_do,
#             data.in = df.gf |>
#               dplyr::filter(ut_type %in% c(2,4,5)) |>
#               dplyr::rename(slfr = sulfur_content_tot) |>
#               fastDummies::dummy_cols(select_columns = c("year","states","ut_type","manufact"),
#                                       remove_first_dummy = TRUE,
#                                       remove_selected_columns = TRUE)) |>
#   capture.output(file=here::here("out", date, "log.imperfectiv.txt"))

