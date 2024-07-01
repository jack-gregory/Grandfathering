## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Grandfathering
## Analysis -- Regs -- Main
## Jack Gregory
## 25 June 2024
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


### START CODE ###


# PREAMBLE ----------------------------------------------------------------------------------------

## Initiate regression functions
source(here("src/stata_regs.R"))


## Create output folder
date <- format(Sys.Date(), "%Y%m%d")
# date <- "20221117"
dir_create(path(l.path$out, date))


# MAIN REGRESSIONS --------------------------------------------------------------------------------

## Build specifications -------------------------------------------------------

## Left-hand side vars
l.lhs <- list(
  "DURATION",
  "survive",
  "SO2"
)

## Right-hand side vars
ctrl_boiler <- "age capacity capacity_gf efficiency_100_pct_load"
ctrl_env <- "so2_nonattain applic_reg ARPprice"
ctrl_env_gf <- "so2_nonattain so2_nonat_Gf applic_reg applic_reg_Gf ARPprice"
fes <- "i.year i.states i.ut_type i.manufact"
ctrl_mkt <- "state_cap_growth coal2gas_price d_growth"

l.rhs <- list(
  glue("Gf {ctrl_boiler} {fes}"),
  glue("Gf {ctrl_boiler} {ctrl_env} {fes}"),
  glue("Gf {ctrl_boiler} so2_nonattain so2_nonat_Gf applic_reg applic_reg_Gf {fes}"),
  glue("Gf {ctrl_boiler} {ctrl_env_gf} sulfur_content_tot c.sulfur_content_tot#c.ARPprice {fes}"),
  glue("Gf {ctrl_boiler} {ctrl_env_gf} {fes}"),
  glue("Gf {ctrl_boiler} so2_nonattain so2_nonat_Gf applic_reg applic_reg_Gf {fes}"),
  glue("Gf {ctrl_boiler} {ctrl_env_gf} {fes}")
)

## Market controls
l.ctrl <- list(
  1,1,1,1,1,1,1
)

## Regression conditions
l.cond <- list(
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)",
  "if ut_type!=. & (inservice_y>=1950 & inservice_y<=2006)",
  "if ut_type!=. & (inservice_y>=1950 & inservice_y<=2006)"
)
cond_survive <- "& ((capacity>0.075 & ut_type==4) | ((ut_type==5|ut_type==2) & year>=1990)) & year<=2017"

## Regression type
l.type <- list(
  "reg","reg","reg","reg","iv","reg","iv"
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


## Build regression table -----------------------------------------------------

## Construct column names
tbl_colnames <- df.reg %>%
  mutate(owner = case_when(str_detect(fml, "\\(ut_type==4 | ut_type==5 | ut_type==2\\)") ~ "IOU+",
                           str_detect(fml, "ut_type!=.") ~ "All",
                           TRUE ~ "IOU")) %>%
  distinct(model_id, type, owner) %>%
  mutate(model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}"),
         type = ifelse(type=="reg", "OLS", "IV"),
         type = paste0("\\multicolumn{1}{c}{\\textit{", type, "}}"),
         owner = paste0("\\multicolumn{1}{c}{\\textit{", owner, "}}")) %>%
  mutate_all(~paste0(., " &")) %>%
  add_row(model_id=" &", type=" &", owner=" &", .before=1) %>%
  add_row(model_id="\\\\\\\\[-1.8ex]", type="\\\\\\\\[-1.8ex]", owner="\\\\") %>%
  t() %>%
  as_tibble() %>%
  mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))

## Construct table coefficients
l.var <- list("Gf","capacity","capacity_gf","so2_nonattain","so2_nonat_Gf","applic_reg","applic_reg_Gf",
              "ARPprice","sulf_cont_iv","ARP_iv_sulf_cont")
l.var_labs <- list("GF","size","GF $\\times$ size","NAAQS","GF $\\times$ NAAQS","MMBTU","GF $\\times$ MMBTU",
                   "ARP price","Sulfur IV","ARP price $\\times$ sulfur IV")
tbl_coef <- df.reg %>%
  filter(var %in% unlist(l.var)) %>%
  mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
  mutate(coef = case_when(
    pval<0.01 ~ paste0(coef, "^{***}"),
    pval<0.05 ~ paste0(coef, "^{**}"),
    pval<0.10 ~ paste0(coef, "^{*}"),
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
l.sum <- list(#"fe_yr","fe_st","fe_ut","fe_man",
              "ctrl_mkt","ctrl_so2","N","r2")
l.sum_labs <- list(#"Year FE","State FE","Utility FE","Manufacture FE",
                   "Market Controls","Sulfur Controls","Observations","R$^{2}$")
tbl_summary <- df.reg %>%
  distinct(lhs, model_id, type, fml, N, r2) %>%
  mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
  mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
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
  pivot_longer(cols=c(-lhs, -model_id), names_to="var", values_to="val") %>%
  pivot_wider(names_from=model_id, names_prefix="model", values_from=val) %>%
  mutate(lhs = factor(lhs, levels=c("DURATION","survive","SO2"),
                      labels=c("Utilization","Survival","Emissions")),
         var = factor(var, levels=unlist(l.sum), labels=unlist(l.sum_labs))) %>%
  arrange(lhs, var) %>%
  mutate_at(vars(2:last_col(offset=1)), ~paste0(., " &")) %>%
  mutate_at(vars(last_col()), ~paste0(., " \\\\"))

## Prepare file header, table header and footer, and subtable headers
f_header <- c("%% Grandfathering Project",
              "%% Main regression table",
              glue::glue("%% Compiled on {base::Sys.time()} using <3_regs.R>"),
              "%% Written by Jack Gregory\n")
tbl_header1 <- c("\\begin{center}",
                 "\\begin{singlespace}",
                 "\\begin{scriptsize}\n",
                 paste0("\\begin{longtable}[c]{@{\\extracolsep{0ex}}l*{", 
                        ncol(tbl_colnames)-2, "}{D{.}{.}{-2}}@{\\extracolsep{-0.3ex}}}"),
                 "\t\\caption{Main regression results}",
                 "\t\\label{tbl:reg_main}\n",
                 "\\\\",
                 "\\hline\\hline \\\\ [-1.2ex]")
tbl_header2 <- c("\\midrule \\\\",
                 "\\endfirsthead",
                 "\\hline\\hline \\\\ [-1.2ex]")
tbl_header3 <- c("\\midrule \\\\",
                 "\\endhead")
subtbl_headerA <- c(paste0("\\multicolumn{", ncol(tbl_colnames)-1, 
                           "}{l}{\\textbf{Panel A:  Utilization} [hours per year]} \\\\ [1.0ex]"),
                    "\\hline")
subtbl_headerB <- c("\\hline \\\\",
                    paste0("\\multicolumn{", ncol(tbl_colnames)-1, 
                           "}{l}{\\textbf{Panel B:  Survival} [percentage points]} \\\\ [1.0ex]"),
                    "\\hline")
subtbl_headerC <- c("\\hline \\\\",
                    "\\pagebreak",
                    paste0("\\multicolumn{", ncol(tbl_colnames)-1, 
                           "}{l}{\\textbf{Panel C:  Emissions} [SO$_2$ lbs per MW of capacity per hour run]} \\\\ [1.0ex]"),
                    "\\hline")
tbl_midder <- c("\\hline \\\\ [-1.8ex]")
txt_footer <- c("This table presents our main regression results for our three channels, namely: ",
            "utilization, survival and emissions. ",
            "Specifications (1)-(4) and (6) are estimated using OLS, while specifications (5) and (7) use ",
            "2SLS and leverage sulfur content of the available coal weighted by network distance as an instrument ",
            "for sulfur content of the combusted coal. The unit of observation is boiler-year. ",
            "The sample is restricted to those boilers with inservice years between 1950 and 2006, inclusive. ",
            "The $IOU+$ columns include independently-owned utilities as well as commercial and industrial boilers; while, ",
            "the $All$ column uses all types of available boilers. ",
            "All regressions include boiler controls as well as year, state, utility type and manufacture type fixed effects. ",
            "Utilization and emissions regressions also include market controls and use data from 1995-2018, ",
            "while survival estimations use 1985-2017.")
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
file <- here::here(l.path$out, date, "tbl.reg_main.tex")
f_header %>% write_lines(file, append=FALSE)
tbl_header1 %>% write_lines(file, append=TRUE)
tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header2 %>% write_lines(file, append=TRUE)
tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_header3 %>% write_lines(file, append=TRUE)

subtbl_headerA %>% write_lines(file, append=TRUE)
tbl_coef %>% 
  filter(lhs=="Utilization") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_summary %>% 
  filter(lhs=="Utilization") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)

subtbl_headerB %>% write_lines(file, append=TRUE)
tbl_coef %>% 
  filter(lhs=="Survival") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_summary %>% 
  filter(lhs=="Survival") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)

subtbl_headerC %>% write_lines(file, append=TRUE)
tbl_coef %>%
  filter(lhs=="Emissions") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_summary %>%
  filter(lhs=="Emissions") %>%
  select(-lhs) %>%
  write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)

tbl_footer %>% write_lines(file, append=TRUE)


## Clean environment ----------------------------------------------------------
rm(list=ls(pattern="^(f|tbl|subtbl|txt)_"))
rm(l.lhs, l.var, l.var_labs, l.sum, l.sum_labs)


# MAIN REGRESSIONS (fixest) -----------------------------------------------------------------------
# 
# library(fixest)
# 
# ## Stata global definitions
# # global controls_env so2_nonattain applic_reg ARPprice_sp
# # global controls_env_interact grand_NSR_in_nonnat_const applic_reg_const
# # global controls_gen_common age max_boi_nameplate
# 
# ## fixest conversions of Stata commands
# # (1) reg `i' grand_NSR_const $controls_gen_common i.year i.states i.ut_type, vce(robust)
# feols(fml=DURATION ~ grand_NSR_const + age + max_boi_nameplate | year + states + ut_type,
#       data=df.gf,
#       se="hetero") %>%
#   summary()
# 
# # (2) reg `i' grand_NSR_const capacity_gf $controls_env $controls_env_interact $controls_gen_common \\\
# #       i.year i.states i.ut_type, vce(robust)
# feols(fml=DURATION ~ grand_NSR_const + age + max_boi_nameplate +
#         i(grand_NSR_const, max_boi_nameplate, 0) + so2_nonattain + applic_reg + ARPprice_sp +
#         grand_NSR_in_nonnat_const + applic_reg_const | year + states + ut_type,
#       data=df.gf,
#       se="hetero") %>%
#   summary()
# 
# feols(fml=DURATION ~ grand_NSR_const + age + max_boi_nameplate +
#         i(grand_NSR_const, max_boi_nameplate, 0) + so2_nonattain + applic_reg + ARPprice_sp +
#         i(grand_NSR_const, so2_nonattain, 0) + i(grand_NSR_const, applic_reg, 0) |
#         year + states + ut_type,
#       data=df.gf,
#       se="hetero") %>%
#   summary()
# 
# 
# # DURATION Gf age capacity capacity_gf efficiency_100_pct_load so2_nonattain so2_nonat_Gf applic_reg applic_reg_Gf ARPprice
# # i.year i.states i.ut_type i.manufact state_cap_growth coal2gas_price d_growth 
# #if (ut_type==4 | ut_type==5 | ut_type==2) & (inservice_y>=1950 & inservice_y<=2006)
# s1 <- feols(fml=sulfur_content_tot ~ sulfur_net_iv + Gf + age + capacity + capacity_gf + efficiency_100_pct_load +
#         so2_nonattain + so2_nonat_Gf + applic_reg + applic_reg_Gf + ARPprice +
#         state_cap_growth + coal2gas_price + d_growth |
#         year + states + ut_type + manufact,
#       data=df.gf |>
#         filter(ut_type %in% c(2,4,5)) |>
#         filter(inservice_y>=1950 & inservice_y<=2006),
#       vcov=cluster ~ ID)
# df.gf_test <- df.gf |>
#   filter(ut_type %in% c(2,4,5)) |>
#   filter(inservice_y>=1950 & inservice_y<=2006) |>
#   slice(s1[["obs_selection"]][["obsRemoved"]]) |>
#   bind_cols(fitted_values = fitted(s1)) |>
#   mutate(interaction_term = fitted_values * ARPprice)
# feols(fml=DURATION ~ fitted_values + Gf + age + capacity + capacity_gf + efficiency_100_pct_load +
#         so2_nonattain + so2_nonat_Gf + applic_reg + applic_reg_Gf + ARPprice + interaction_term +
#         state_cap_growth + coal2gas_price + d_growth |
#         year + states + ut_type + manufact,
#       data=df.gf_test,
#       vcov=cluster ~ ID) %>%
#   summary()


# FIRST STAGE REGRESSIONS--------------------------------------------------------------------------

## Build specifications -------------------------------------------------------

## Right-hand side vars
l.rhs <- l.rhs[l.type=="iv"]

## Market controls
l.ctrl <- l.ctrl[l.type=="iv"]

## Regression conditions
l.cond <- l.cond[l.type=="iv"]

## Regression type
l.type <- list(
  "reg","reg"
)


## Perform regressions --------------------------------------------------------

## Build regression dataframe
df.reg <- tibble(l.type, l.rhs, l.cond) %>%
  rowid_to_column(var="model_id") %>%
  rename_with(.fn=~str_replace(., "l\\.", "")) %>%
  mutate(ctrl = ctrl_mkt,
         cond = as.character(cond),
         fml = paste("sulfur_content_tot sulfur_net_iv", rhs, ctrl, cond, sep=" ")) %>%
  mutate_at(vars(-model_id), as.character) %>%
  mutate(model = map2(fml, type,
                      ~stata_reg(fml=.x, df=df.gf, type=.y, bootstrap=FALSE)),
         model = map(model, 
                     ~select(.x, var, coef, stderr, tstat, pval, ci_lower, ci_upper, 
                             N, r2, r2_a))) %>%
  unnest(model)


## Build regression table -----------------------------------------------------

## Construct column names
tbl_colnames <- df.reg %>%
  mutate(owner = case_when(str_detect(fml, "\\(ut_type==4 | ut_type==5 | ut_type==2\\)") ~ "IOU+",
                           str_detect(fml, "ut_type!=.") ~ "All",
                           TRUE ~ "IOU")) %>%
  distinct(model_id, type, owner) %>%
  mutate(model_id = ifelse(model_id==1, 5, 7),
         model_id = paste0("\\multicolumn{1}{c}{(", model_id, ")}"),
         type = ifelse(type=="reg", "OLS", "IV"),
         type = paste0("\\multicolumn{1}{c}{\\textit{", type, "}}"),
         owner = paste0("\\multicolumn{1}{c}{\\textit{", owner, "}}")) %>%
  mutate_all(~paste0(., " &")) %>%
  add_row(model_id=" &", type=" &", owner=" &", .before=1) %>%
  add_row(model_id="\\\\\\\\ [-1.8ex]", type="\\\\\\\\ [-1.8ex]", owner="\\\\") %>%
  t() %>%
  as_tibble() %>%
  mutate_at(vars(last_col(offset=1)), ~str_replace(., " &$", ""))

## Construct table coefficients
l.var <- list("sulfur_net_iv","Gf","capacity","capacity_gf","so2_nonattain","so2_nonat_Gf",
              "applic_reg","applic_reg_Gf")
l.var_labs <- list("Sulfur distance","GF","size","GF $\\times$ size","NAAQS","GF $\\times$ NAAQS",
                   "MMBTU","GF $\\times$ MMBTU")
tbl_coef <- df.reg %>%
  filter(var %in% unlist(l.var)) %>%
  mutate_at(vars(coef, tstat), ~formatC(round(., digits=2), format="f", digits=2)) %>%
  mutate(coef = case_when(pval<0.01 ~ paste0(coef, "^{***}"),
                          pval<0.05 ~ paste0(coef, "^{**}"),
                          pval<0.10 ~ paste0(coef, "^{*}"),
                          TRUE ~ as.character(coef)),
         tstat = paste0("(", tstat, ")")) %>%
  select(model_id, var, coef, tstat) %>%
  pivot_longer(cols=c(coef, tstat), names_to="stat", values_to="val") %>%
  pivot_wider(names_from=c(model_id), names_sep="_", values_from=val) %>%
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
l.sum <- list(#"fe_yr","fe_st","fe_ut","fe_man",
              "ctrl_mkt","N","r2")
l.sum_labs <- list(#"Year FE","State FE","Utility FE","Manufacture FE",
                   "Market Controls","Observations","R$^{2}$")
tbl_summary <- df.reg %>%
  distinct(model_id, type, fml, N, r2) %>%
  mutate_at(vars(N), ~formatC(., format="d", digits=0, big.mark=",")) %>%
  mutate_at(vars(r2), ~formatC(round(., digits=3), format="f", digits=3)) %>%
  # mutate(fe_yr = ifelse(str_detect(fml, "i.year"), "X", ""),
  #        fe_st = ifelse(str_detect(fml, "i.states"), "X", ""),
  #        fe_ut = ifelse(str_detect(fml, "i.ut_type"), "X", ""),
  #        fe_man = ifelse(str_detect(fml, "i.manufact"), "X", ""),
  #        ctrl_mkt = ifelse(str_detect(fml, ctrl_mkt), "X", "")) %>%
  select(model_id, starts_with("fe"), starts_with("ctrl"), N, r2) %>%
  mutate_at(vars(starts_with("fe"), starts_with("ctrl")),
            ~case_when(is.na(.) ~ "\\textit{}",
                       TRUE ~ paste0("\\textit{", ., "}"))) %>%
  mutate_at(vars(-model_id), ~ paste0("\\multicolumn{1}{c}{", ., "}")) %>%
  pivot_longer(cols=c(-model_id), names_to="var", values_to="val") %>%
  pivot_wider(names_from=c(model_id), names_sep="_", values_from=val) %>%
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
                "\t\\caption{Main regression first-stage results}",
                "\t\\label{tbl:reg_main_fs}\n",
                "\\scriptsize\n",
                paste0("\\begin{tabular}{@{\\extracolsep{2.5ex}}l*{", ncol(tbl_colnames)-2,
                       "}{D{.}{.}{-2}}@{\\extracolsep{2.5ex}}}"),
                "\\\\[-1.8ex] \\hline",
                "\\hline \\\\[-1.8ex]")
tbl_midder <- c("\\hline \\\\[-1.8ex]")
txt_footer <- c("This table presents the first-stage results for our main regressions, corresponding to ",
                "specifications (5) and (7) in Table 1. ",
                "The three channels each produce similar results, so we report those for utilization only. ",
                "The dependent variable is the SO$_2$ content of combusted coal. ",
                "All specifications are estimated using OLS and data from 1995-2018. The unit of observation is boiler-year. ",
                "The sample is restricted to those boilers with inservice years between 1950 and 2006, inclusive. ",
                "The $IOU+$ column includes independently-owned utilities as well as commercial and industrial boilers; while, ",
                "the $All$ column uses all types of available boilers. ",
                "All regressions above include boiler and market controls as well as year, state, utility type and ",
                "manufacture type fixed effects. ")
tbl_footer <- c("\\hline",
                "\\hline \\\\[-1.8ex]",
                paste0("\\multicolumn{", ncol(tbl_colnames)-1, "}{p{9cm}}{\\textit{Notes:} ", 
                       paste(txt_footer, collapse=""), " Boiler-level clustered standard errors used, with ",
                       "*** p$<$0.01,  ** p$<$0.05,  * p$<$0.10 and ",
                       "\\textit{t}-statistics in parentheses.} \\\\"),
                "\\end{tabular}",
                "\\end{table}")

## Output TeX table file
file <- here::here(l.path$out, date, paste0("tbl.reg_main_fs.tex"))
f_header %>% write_lines(file, append=FALSE)
tbl_header %>% write_lines(file, append=TRUE)
tbl_colnames %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_coef %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_midder %>% write_lines(file, append=TRUE)
tbl_summary %>% write_tsv(file, append=TRUE, col_names=FALSE, escape=NULL)
tbl_footer %>% write_lines(file, append=TRUE)


## Clean environment ------------------------------------------------------------------------------
rm(list=ls(pattern="^(f|tbl|txt)_"))
rm(list=ls(pattern="^ctrl_"), fes, file)
rm(l.lhs, l.rhs, l.cond, l.type, l.var, l.var_labs, l.sum, l.sum_labs)


### END CODE ###

