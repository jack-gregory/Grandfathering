# Grandfathering

This R project and GitHub repository contributes to the NSR grandfathering project.  It is authored by:

- Sylwia Bialek, PhD
- Jack Gregory, PhD
- Bridget Plas
- Prof Richard Revesz

The remainder of this readme file is organized as follows.  First, we discuss the research objective.  We then discuss the structure of the repository.  Finally, we describe the code and its operation.


## Objective

We are interested in the consequences of the NSR grandfathering provisions included in the Clean Air Act Amendments 1977.  We study them empirically in the context of coal-fired boilers, where we examine how grandfathering clauses affected their utilization, survival and emissions rates.

Further elaborate on the following:

- Research question;
- Empirical strategy;
- Data; and,
- Relevance/importance.


## Structure

The following table lists and describes the structure of the repository.

| **Folder** | **Description** |
|------------|-----------------|
| <../build> | Contains all build code. |
| <../build/01_data> | Contains all code related to data collection, ingestion, and cleaning. |
| <../build/02_preanalysis> | Contains all code related to project preanalysis and setup. |
| <../build/03_analysis> | Contains all code related to project analysis. |
| <../data> | Contains cleaned data related to the GitHub portion of the project.  Its subfolders contain raw data, including but not limited to: EIA, EPA, and IPUMS. |
| <../lit> | Contains relevant literature, as well as a database and a companion script for generating an associated BibTex file. |
| <../out> | Contains all project outputs arranged in subfolders by date of generation, including plots and regression tables. |
| <../report> | Contains project documents in LaTeX and pdf formats. |
| <../src> | Contains all source code and functions. |


## Code

The following table lists and describes the scripts within the build folder.

| **Script** | **Description** |
|------------|-----------------|
| <../01_data/CEMS_1_scrape.R> | This program is the first in a series related to EPA CEMS data; it scrapes CEMS data from the EPA FTP website. |
| <../01_data/CEMS_2_ingestion.R> | This program is the second in a series related to EPA CEMS data; it synthesizes all available CEMS data and transfers it to the local MySQL epa database. | 
| <../01_data/CEMS_3_query.R> | This program is the third in a series related to EPA CEMS data; it queries CEMS data from the MySQL epa database, builds a GF-EPA crosswalk by boiler id, and returns aggregated CEMS data by hour and year.|
| <../01_data/EIA_861.Rmd> | This workbook is part of the data cleaning process.  It downloads and cleans annual sales by utility from the EIA.  Specifically, it utilizes the "Sales to Ultimate Customers" table reported in form EIA-861.  It returns <../data/eia_sales.csv> which contains a utility-by-year panel of electricity sales. |
| <../01_data/EIA_423923.Rmd> | This workbook is a part of the data cleaning process.  It downloads and cleans data from EIA-923 and its predecessor, EIA-423.  The forms contain monthly fuel receipts for plants with a fossil-fueled nameplate generating capacity of 50 or more MW.  It returns <../data/eia_fuel.xlsx> and <../data/eia_sulfur.csv>, which contain plant- and state-by-month panels of fuel prices and mine and county cross-section of sulfur contents, respectively. |
| <../01_data/EIA_906923.Rmd> | This workbook is a part of the data cleaning process.  It downloads and cleans data from EIA-923 and its predecessors, EIA-906 and EIA-920.  The forms provide monthly and annual data on generation and fuel consumption at the power plant and prime mover level.  A subset of plants -- steam-electric plants 10 MW and above -- also provide boiler level and generator level data.  It returns <../data/eia_netgen.csv> and <../data/eia_netgen_unit.csv>, which contain plant- and boiler-by-month panels of net generation, respectively. |
| <../01_data/EIA_capacity.Rmd> | This workbook is a part of the data cleaning process.  It downloads and cleans annual capacity by state and fuel source from the EIA.  Specifically, it utilizes the "Existing Nameplate and Net Summer Capacity by Energy Source, Producer Type and State" table based on annual data reported in form EIA-860.  It returns <../data/eia_capacity.csv>, which contains a state-by-fuel-by-year panel of electricity capacities. |
| <../01_data/NHGIS.Rmd> | This workbook is pat of the data cleaning process.  It downloads and cleans IPUMS NHGIS data comprising county level socio-demographics.  The data is originally sourced from the Decennial Census and the American Community Survey.  It returns <../data/NHGIS.csv>, which is a county-by-year panel of income and population share data. |
| <../02_analysis/balance.Rmd> | This workbook is a part of the analysis.  It prepares a data balance table using cleaned grandfathering data, EPA-EIA crosswalk, and EPA facility attributes.  It returns <../out/../tbl.balance.tex>.  It also returns <../out/../gf_share_map.png>, which contains a map associating boiler NSR grandfathering share with their plant coordinates. |
| <../02_analysis/local_reg.R> | This program is a part of the analysis.  It prepares a local regulations plot by state and year.  It returns <../out/../local_reg.pdf>. |
| <../02_analysis/netgen.R> | This program is a part of the analysis.  It calculates the the net-to-gross generation ratio (GR) for as many grandfathered boilers/power plants as possible.  It utilizes hourly gross generation data from EPA CEMS and monthly net generation data from EIA-923 and its predecessors.  It performs a set of naive regressions and returns <../out/../tbl.reg_gr.tex>, which contains the associated regression table. |
| <../02_analysis/retire.Rmd> | This workbook is a part of the analysis.  It prepares a boiler retirement map.  It returns <../out/../retire_map.png>, which contains a map associating boiler retirements with their plant coordinates. |
| <../02_analysis/sulfur.Rmd> | This workbook is a part of the data cleaning and the analysis.  It prepares a set of sulfur IVs based on plant distance to low sulfur coal.  It utilizes cleaned data from EIA-923 and its predecessors prepared in <../build/01_data/EIA_423923.Rmd>.  It returns <../data/sulfur_iv.csv>, which contains a plant cross-section of sulfur IVs.  It also returns <../out/../sulfur_map.png> and <../out/../sulfur_iv_map.png>, which are maps associating the median county sulfur contents and sulfur IVs with their plant coordinates, respectively. |
| <../02_analysis/survival_share.R> | This program is a part of the analysis.  It prepares a survival share bar plot.  It returns <../out/../survival_share_10yr.pdf>. |

