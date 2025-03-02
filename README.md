Grandfathering
================
26 February 2025

- [Overview](#overview)
- [Abstract](#abstract)
- [Repository Structure](#repository-structure)
- [Data Availability](#data-availability)
- [Computational requirements](#computational-requirements)
- [Code](#code)
- [Instructions](#instructions)
- [Citation](#citation)
- [References](#references)

------------------------------------------------------------------------

## Overview

This GitHub repository contains the replication package for:

> Bialek-Gregory, Sylwia, Jack Gregory, Bridget Pals, and Richard
> Revesz. Forthcoming. “[Still your grandfather’s boiler: Estimating the
> effects of the Clean Air Act’s grandfathering
> provisions](https://www.journals.uchicago.edu/doi/10.1086/734214).”
> *Journal of the Association of Environmental and Resource Economists*.

It is a ‘first principles’ alternative to the official replication
package available [here](https://doi.org/10.7910/DVN/YOMSIL). Both
packages are fully self-contained; however, this repository endeavors
wherever possible to provide code in lieu of data. For example, a major
departure from the official replication package is the inclusion of the
SQL code necessary to setup a MySQL CEMS[^1] database.

The code in this replication package reproduces data collection,
cleaning, and analysis for the paper using R, SQL, and Stata. The
scripts in the `build/05_analysis` subfolder produce all tables and
figures in the paper and appendices. The replicator should expect the
code to run for about a week on a personal computer running Windows 10
Home on an Intel four-core CPU at 2.60GHz with 20GB of RAM.

*!!!Add data and code license summary!!!*

The remainder of this readme file is organized as follows. First, we
reproduce the paper’s [abstract](#abstract) and provide the [repository
structure](#repository-structure). Next, we discuss [data
availability](#data-availability), through the main data sources and all
data files, and we list all [computational
requirements](#computational-requirements). We then provide a
description of the [code](#code) and [instructions](#instructions) for
how to reproduce our analysis. We suggest sample [citations](#citation)
for the paper and repository. Finally, we list our
[references](#references).

## Abstract

While vintage differentiation is a highly prominent feature of various
regulations, it can induce significant biases. We study these biases in
the context of New Source Review—a program within the U.S. Clean Air Act
imposing costly sulfur dioxide (SO$_2$) abatement requirements on new
boilers but not existing ones. Leveraging a novel dataset covering
state-level SO$_2$ regulations, we show that the regulatory
differentiation decreased the probability of a grandfathered boiler
retiring in a given year by 2.4 percentage points (i.e., 60 percent
smaller) and increased their operations by around 1,150 hours annually.
Conservative back-of-the-envelope calculations suggest annual societal
damages of up to \$2.5 billion associated with additional SO$_2$
emissions caused by delayed retirements, higher utilization, and higher
emission rates in the years considered. The results imply that long
after their passage and in presence of many subsequent, more stringent
environmental policies, grandfathering provisions continued to have
strong perverse impacts.

## Repository Structure

The following table lists and describes the structure of the repository.

| Folder | Description | Included |
|:---|:---|:---|
| `build` | Contains all collection, cleaning, preprocessing, and analysis code. | Yes |
| `build/01_eia` | Contains all code related to EIA data collection and cleaning. Includes an automation script `01_eia/00_master.R`, though the constituent scripts can be run independently and in any order. | Yes |
| `build/02_boilers` | Contains all code related to EIA boiler cleaning, with an automation script `02_boilers/00_master.R` where order is important. | Yes |
| `build/03_cems` | Contains all code related to EPA CEMS data collection, ingestion, and cleaning. Note that order of the scripts is important. | Yes |
| `build/04_preprocessing` | Contains all code related to data preprocessing, with an automation script `04_preprocessing/00_master.R` though the constituent scripts can be run independently and in any order. | Yes |
| `build/05_analysis` | Contains all code related to project analysis and produce all tables and figures within the paper. Includes an automation script `05_analysis/00_master.R` where order is important. | Yes |
| `data` | Contains both raw and cleaned data. The latter is located in the root directory, while the former is stored by source among its subfolders. | Yes |
| `lit` | Contains a database and a companion script for generating an associated BibTex file. | Yes |
| `out` | Contains all project outputs arranged in subfolders by generation date. Folder is generated through the build code. | No |
| `src` | Contains all source code and functions. | Yes |

## Data Availability

All data used in the paper is from secondary sources. That is, it is
either collected from publicly available sources or assembled manually.

See [LICENSE.txt](LICENSE.txt) for details.

The following datasets are licensed under the [Creative Commons
Attribution-NonCommercial 4.0 International License (CC BY-NC
4.0)](https://creativecommons.org/licenses/by-nc/4.0/):

- State emission regulations $\longrightarrow$
  `so2_regulations_by_state_1976_to_2019.xlsx` and
  `data/regs/indiana.xlsx`
- New Source Review grandfathering status $\longrightarrow$
  `data/regs/gf_status_born_around_1978.xlsx`
- County attainment status 1978–1990 $\longrightarrow$
  `data/epa/phistory_1978_1990.csv`

You may use, share, and adapt this data for **non-commercial** purposes
with appropriate credit.

All other data are sourced from the U.S. federal government under the
[public domain](https://www.usa.gov/government-copyright). As such,
these data are licensed under the [Creative Commons Zero (CC0)
License](https://creativecommons.org/publicdomain/zero/1.0/). You may
use, share, and adapt this data without restriction. See
[LICENSE.md](LICENSE.md) for further details.

Below we provide tables summarizing the [data
sources](#main-data-sources) for the analysis and the [data
files](#data-files) within the repository.

### Main data sources

The table below summarizes main data references necessary for the
analysis.

| Source | Dataset | Via | Description | Citation |
|:---|:---|:---|:---|:---|
| [Bureau of Transportation Statistics (BTS)](https://www.bts.gov/) | [National Transportation Atlas Database (NTAD)](https://www.bts.gov/ntad) | Download | The NTAD is a set of nationwide geographic databases of transportation facilities, transportation networks, and associated infrastructure. These datasets include spatial information for transportation modal networks and intermodal terminals, as well as the related attribute information for these features. The current project utilizes rail and marine geospatial datasets. | US DOT (2022) |
| [Census Bureau (CB)](https://www.census.gov/) | [Geospatial data](NA) | Download | CB geospatial data including state shapefiles and geocodes. | N/A |
| [Energy Information Administration (EIA)](https://www.eia.gov/survey) | [EIA-423](https://www.eia.gov/electricity/data/eia423/) | Code | Historical survey Form EIA-423 collected monthly fuel receipts for plants with a fossil-fueled nameplate generating capacity of 50+ MW. It was discontinued in 2011; beginning in 2008, its information was transferred to Form EIA 923. | US EIA (2023) |
|  | [EIA-767](https://www.eia.gov/electricity/data/eia767/) | Code | Historical survey Form EIA-767 collected data from steam-electric plants with a generator nameplate rating of 10+ MW. It contains data on plant operations and equipment design, including boilers, generators, flue gas desulfurizations, flue gas particulate collectors, and stacks. EIA Form 767 was discontinued in 2005; beginning in 2007, its information was transferred to Forms EIA 860 and EIA 923. | US EIA (2005) |
|  | [EIA-860](https://www.eia.gov/electricity/data/eia860/) | Code | The survey Form EIA-860 collects generator-level data on existing and planned electric power plants with 1+ MW of combined nameplate capacity. The EIA-860 superseded the EIA-767. | US EIA (2019a) |
|  | [EIA-861](https://www.eia.gov/electricity/data/eia861/) | Code | The survey Form EIA-861 collects data from distribution utilities and power marketers of electricity. | US EIA (2021a) |
|  | [EIA-923](https://www.eia.gov/electricity/data/eia923/) | Code | The survey Form EIA-923 collects detailed electric power data on electricity generation, fuel consumption, and receipts at the power plant and prime mover level. The EIA-923 superseded the EIA-423, EIA-759, EIA-767, EIA-867, EIA-906, and EIA-920. | US EIA (2019b) |
|  | [Geospatial data](https://atlas.eia.gov/datasets) | Download | EIA geospatial data including state and county shapefiles. | US EIA (2021b) |
|  | [State data](https://www.eia.gov/electricity/data/state/) | Code | EIA state-level summary data, primarily with respect to Form EIA-860. | US EIA (2019c) |
| [Environmental Protection Agency (EPA)](https://campd.epa.gov/) | [Continuous Emission Monitoring System (CEMS)](https://www.epa.gov/power-sector/power-sector-data) | Code | EPA CEMS data is available from the Clean Air Markets Program Data (CAMPD) website. It contains hourly resolution of plant utilization and emissions beginning in 1995 and containing nost coal plants. | US EPA (2021b) |
| [Environmental Protection Agency (EPA)](https://www.epa.gov/green-book) | [EPA Green Book](https://www.epa.gov/green-book/green-book-data-download) | Download | EPA nonattainment areas from 1992 to the present. | US EPA (2021a) |
| [Environmental Protection Agency (EPA)](https://campd.epa.gov/) | [EPA-EIA Crosswalk](https://www.epa.gov/power-sector/power-sector-data-crosswalk) | Code | A data crosswalk to integrate US power sector emission and operation data from the EPA and EIA. | Huetteman et al. (2021) |
|  | [Facility Attributes](https://19january2021snapshot.epa.gov/airmarkets/facility-attributes_.html) | Download | The EPA collects power plant data through its various programs. It synthesizes this information through an annual “facility attributes” dataset. | US EPA (2021b) |

### Data files

This repository represents a ‘first principles’ replication package,
which means that it focuses on supplying all necessary code in lieu of
data. Essentially, we aim to provide all collection and cleaning code
for publicly available data instead of a preprocessed dataset ready for
analysis.

If you would prefer a replication package which focuses on data
provision, much of the data listed below is available through the
[official replication package](https://doi.org/10.7910/DVN/YOMSIL).

There are two datasets of particular note which cover our universe of
state regulations.

- \<so2_regulations_by_state_1976_to_2019.xlxs\> $\longrightarrow$ All
  state regulations except Indiana site-specific ones.
- \<indiana.xlsx\> $\longrightarrow$ Indiana site-specific regulations
  based on the boilers in our sample and as such is non-exhaustive.

These were manually compiled by examining EPA historical documents,
state administrative codes, entries in the Federal Register, and other
resources. Further details are included in the paper. Each file contains
a data dictionary, while they also contain references to the relevant
data sources used to identify each regulation. Moreover, the [official
replication package](https://doi.org/10.7910/DVN/YOMSIL) contains a zip
folder with references that are more challenging to locate.

Additional documentation—including data dictionaries—are typically
included directly in the code.

| File | Location | Code | Source | Description | Included |
|:---|:---|:---|:---|:---|:---|
| 1989_1998_Nonutility_Power_Producer.zip | `data/eia` | `NA` | [EIA](https://www.eia.gov/electricity/data/eia923/) | data from EIA-867 available for years 1989–1998 on independent power producers | Yes |
| ARP_power_plants.csv | `data/regs` | `NA` | [Federal Register](https://www.federalregister.gov/citation/58-FR-3687) | Plants participating in Phase I of the Acid Rain Program (ARP) based on Table 1 in Subpart B 58 FR 3687. | Yes |
| ARP_prices.xlsx | `data/regs` | `NA` | [EPA](https://www.epa.gov/power-sector/so2-allowance-auctions) | Data on annual ARP auction permit prices. | Yes |
| EIA electricity demand data.zip | `data/eia` | `NA` | [EIA](https://www.eia.gov/electricity/data/state/) | State-by-year panel of electricity demand. | Yes |
| Facility_Attributes.zip | `data/epa` | `NA` | [EPA](https://campd.epa.gov/data/bulk-data-files) | EPA power plant data from 2021 synthesized through its various programs. | Yes |
| Marine_Highways.zip | `data/bts` | `NA` | [BTS](https://geodata.bts.gov/maps/marine-highways) | The Marine Highways dataset was updated on October 31, 2023 and is part of the NTAD. The dataset contains the locations of all 28 maritime routes that have been designated as Marine Highways by the US Department of Transportation. | Yes |
| North_American_Rail_Network_Lines.zip | `data/bts` | `NA` | [BTS](https://geodata.bts.gov/maps/north-american-rail-network-lines) | The North American Rail Network (NARN) Rail Lines dataset was updated on October 12, 2023 and is part of the NTAD. Among other variables, the dataset provides rail ownership, trackage rights, and type. | Yes |
| USA_Counties\_(Generalized).zip | `data/eia/shp` | `NA` | [EIA](https://atlas.eia.gov/datasets/esri::usa-counties-generalized) | EIA geospatial data for the 2020 US Census County boundaries including 2020 US Census codes. | Yes |
| USA_States\_(Generalized).zip | `data/eia/shp` | `NA` | [EIA](https://atlas.eia.gov/datasets/esri::usa-states-generalized) | EIA geospatial data for the 2020 US Census State boundaries including 2020 US Census codes. | Yes |
| all-geocodes-v2020.xlsx | `data/cb` | `NA` | [CB](https://www2.census.gov/programs-surveys/popest/geographies/2020/) | The CB provides a list of all geocodes from the 2020 US Census. | Yes |
| all_fuel_1970_2018.csv | `data` | `build/02_boilers/05_fuel_and_fuel_comp_1970_2018.R` | Internal | Plant-by-year panel of EIA-759 and EIA-923 coal data between 1970 and 2018. | No |
| all_years_all_plants_and_features.csv | `data` | `build/02_boilers/08_feature_generation.R` | Internal | Draft synthesized boiler data. | No |
| aqcrs_to_cty_xwalk.csv | `data/xwalk` | `NA` | Internal | Crosswalk from Air Quality Control Regions (ACQRs) to counties. | Yes |
| boilers_1985_2005.csv | `data` | `build/02_boilers/02_eia767_1985-2005.R` | Internal | Boiler-by-year panel of EIA-767 data between 1985 and 2005. | No |
| boilers_1985_2018.csv | `data` | `build/02_boilers/04_merge_eia767_and_eia860.R` | Internal | Boiler-by-year panel of EIA-767 and EIA-860 data between 1985 and 2018. | No |
| boilers_2007_2018.csv | `data` | `build/02_boilers/03_eia860_2007-2018.R` | Internal | Boiler-by-year panel of EIA-860 data between 2007 and 2018. | No |
| boilers_fuel_comp_1985_2018.csv | `data` | `build/02_boilers/05_fuel_and_fuel_comp_1970_2018.R` | Internal | Boiler-by-year panel of EIA-767 and EIA-923 coal data between 1985 and 2018. | No |
| cb_2018_us_county_500k.zip | `data/cb` | `NA` | [CB](https://census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html) | CB geospatial data of US counties. | Yes |
| cems_YYYY.rds | `data/epa` | `build/03_cems/03_cems_query.R` | Internal | Yearly CEMS data restricted to the grandfathering sample consisting of gross load, SO$_2$ emissions, etc. by plant and unit. | No |
| cems_hrHH.rds | `data/epa` | `build/03_cems/03_cems_query.R` | Internal | Hourly CEMS data restricted to the grandfathering sample consisting of gross load, SO$_2$ emissions, etc. by plant and unit. | No |
| cems_yr.dta | `data` | `build/03_cems/03_cems_query.R` | Internal | Yearly CEMS data restricted to the grandfathering sample consisting of gross load, SO$_2$ emissions, etc. by plant and unit. | No |
| coal_char_data_by_plant_year.csv | `data` | `build/02_boilers/06_coal_mine_data_eia923.R` | Internal | Plant-by-year panel of EIA-423 and EIA-923 coal data. | No |
| ctrpoints_counties.csv | `data` | `build/02_boilers/01_county_level_data.R` | Internal | Cross-section of county centerpoints. | No |
| eia860_file_structure.xlsx | `data/xwalk` | `NA` | Internal | Description of the Form EIA-860 file structure, which changes in minor ways from year to year, to make the files easier to import. | Yes |
| eia923_fuel_dictionary.csv | `data/xwalk` | `NA` | Internal | Fuel definitions and codes across EIA-923 and its predecessors sourced from the included dataset notes. | Yes |
| eia_capacity.csv | `data` | `build/01_eia/EIA_capacity.Rmd` | Internal | State-by-fuel-by-year panel of electricity capacities. | No |
| eia_fuel.xlsx | `data` | `build/01_eia/EIA_423923.Rmd` | Internal | Plant- and state-by-month panels of fuel prices. | No |
| eia_netgen.csv | `data` | `build/01_eia/EIA_906923.Rmd` | Internal | Plant-by-month panel of net generation. | No |
| eia_netgen_unit.csv | `data` | `build/01_eia/EIA_906923.Rmd` | Internal | Boiler-by-month panel of net generation. | No |
| eia_plant.csv | `data` | `NA` | Internal | Plant-by-year panel of Form EIA-923 reporting frequency data from sheet “Page 6 Plant Frame”. | Yes |
| eia_sales.csv | `data` | `build/01_eia/EIA_861.Rmd` | Internal | Utility-by-year panel of electricity sales. | No |
| eia_sulfur.csv | `data` | `build/01_eia/EIA_423923.Rmd` | Internal | Mine, county, and state cross-section of sulfur contents. | No |
| epa.cems | MySQL | `build/03_cems/02_cems_ingestion.R` | [EPA](https://campd.epa.gov/) | Table populated with cleaned EPA CEMS data. | No |
| epa.filelog | MySQL | `build/03_cems/02_cems_ingestion.R` | Internal | Metadata table populated with EPA CEMS raw data files which have been ingested successfully into the MySQL database. | No |
| epa.xwalk | MySQL | `build/03_cems/02_cems_ingestion.R` | [EPA](https://github.com/USEPA/camd-eia-crosswalk/raw/master/epa_eia_crosswalk.csv) | Table populated with EPA and EIA power plant crosswalk data synthesized from the EPA-EIA crosswalk and the CEMS database. | No |
| epa_eia_crosswalk.csv | `data/epa` | `build/03_cems/02_cems_ingestion.R` | [EPA](https://github.com/USEPA/camd-eia-crosswalk/raw/master/epa_eia_crosswalk.csv) | Crosswalk mapping EPA and EIA power sector emission and operation data, authored by the EPA’s Clean Air Markets Division (CAMD). | Yes |
| existcapacity_annual.xlsx | `data/eia` | `build/01_eia/EIA_capacity.Rmd` | [EIA](https://www.eia.gov/electricity/data/state/existcapacity_annual.xlsx) | “Existing Nameplate and Net Summer Capacity by Energy Source, Producer Type and State” table based on annual data reported in form EIA-860. | No |
| f423_YYYY.zip | `data/eia/f923` | `build/01_eia/EIA_423923.Rmd` | [EIA](https://www.eia.gov/electricity/data/eia423/) | Form EIA-423 collected monthly fuel receipts for plants with a fossil-fueled nameplate generating capacity of 50+ MW, where the data are available from 1972 to 2011. | No |
| f759_YYYY.zip | `data/eia/f923` | `build/01_eia/EIA_759.Rmd` | [EIA](https://www.eia.gov/electricity/data/eia923/eia906u.php) | Historical form EIA-759 “Monthly power plant report”, where the data are available from 1970 to 2000. | No |
| f767_YYYY.zip | `data/eia/f767` | `build/01_eia/EIA_767.Rmd` | [EIA](https://www.eia.gov/electricity/data/eia767/) | Historical form EIA-767 “Steam-electric plant operation and design report”, where the data are available from 1985 to 2005. | No |
| f860_YYYY.zip | `data/eia/f860` | `build/01_eia/EIA_860.Rmd` | [EIA](https://www.eia.gov/electricity/data/eia860/) | Form EIA-860 “Electric generators”, where the data are available from 1990 to the present. | No |
| f861_YYYY.zip | `data/eia/f861` | `build/01_eia/EIA_861.Rmd` | [EIA](https://www.eia.gov/electricity/data/eia861/) | Form EIA-861 a census of all electric utilities, where the data are available from 1990 to the present. | No |
| f906920_YYYY.zip | `data/eia/f923` | `build/01_eia/EIA_906923.Rmd` | [EIA](https://www.eia.gov/electricity/data/eia923/) | Predecessor to Form EIA-923, where the data are available from 1998 to 2007. | No |
| f906_YYYY.zip | `data/eia/f923` | `build/01_eia/EIA_906923.Rmd` | [EIA](https://www.eia.gov/electricity/data/eia923/eia906u.php) | Predecessor to Form EIA-923, where the data are available from 1998 to 2007. | No |
| f923_YYYY.zip | `data/eia/f923` | `build/01_eia/EIA_423923.Rmd` | [EIA](https://www.eia.gov/electricity/data/eia923/) | Form EIA-923 collects electric power data on electricity generation, fuel consumption, fossil fuel stocks, and receipts at the power plant and prime mover level, where the data are available from 1989 to the present through historical forms, including EIA-867, EIA-906, and EIA-920. | No |
| fgds_1985_2005.csv | `data` | `build/02_boilers/02_eia767_1985-2005.R` | Internal | Flue-gas-desulfurization-by-year panel of EIA-767 data between 1985 and 2005. | No |
| fgds_1985_2018.csv | `data` | `build/02_boilers/04_merge_eia767_and_eia860.R` | Internal | Flue-gas-desulfurization-by-year panel of EIA-767 and EIA-860 data between 1985 and 2018. | No |
| fgds_2007_2018.csv | `data` | `build/02_boilers/03_eia860_2007-2018.R` | Internal | Flue-gas-desulfurization-by-year panel of EIA-860 data between 2007 and 2018. | No |
| generators_1985_2005.csv | `data` | `build/02_boilers/02_eia767_1985-2005.R` | Internal | Generator-by-year panel of EIA-767 data between 1985 and 2005. | No |
| generators_1985_2018.csv | `data` | `build/02_boilers/04_merge_eia767_and_eia860.R` | Internal | Generator-by-year panel of EIA-767 and EIA-860 data between 1985 and 2018. | No |
| generators_2007_2018.csv | `data` | `build/02_boilers/03_eia860_2007-2018.R` | Internal | Generator-by-year panel of EIA-860 data between 2007 and 2018. | No |
| gf_cems_xwalk.dta | `data/xwalk` | `build/03_cems/03_cems_query.R` | Internal | Final crosswalk on ORISPL and boiler codes between the boiler dataset `regression_vars.csv` and CEMS data. | Yes |
| gf_matches_full.csv | `data` | `build/03_cems/03_cems_query.R` | Internal | Draft crosswalk on ORISPL and boiler codes between the boiler dataset `regression_vars.csv`, the EPA-EIA crosswalk, and CEMS data, used to generate manual matches for the `gf_cems_xwalk.dta`. | No |
| gf_status_born_around_1978.xlsx | `data/regs` | `NA` | Internal | Boiler NSR-grandfathering status for those that commenced operations soon after 1978. The data are based on calls, news search, etc. | Yes |
| handmatched_pa_maine_nm_mass.csv | `data/xwalk` | `build/02_boilers/07_merge_boilers_to_regulations.R` | Internal | Crosswalk matching boilers in states with irregular geographical boundaries for air quality regions/basins (e.g., not defined by county) to the correct geographical location. It was compiled by comparing plant location with a map of air basins available [here](https://files.dep.state.pa.us/air/AirQuality/AQPortalFiles/Regulations%20and%20Clean%20Air%20Plans/attain/PM25Des/FigureB17-PA%20Air%20Basins.pdf). | Yes |
| indiana.xlsx | `data/regs` | `NA` | Internal | Indiana site-specific regulations limited to boilers in our sample. | Yes |
| merged_regs_and_plants.csv | `data` | `build/02_boilers/07_merge_boilers_to_regulations.R` | Internal | Plant-by-year panel of regulations. | No |
| mine_data_long.csv | `data` | `build/02_boilers/06_coal_mine_data_eia923.R` | Internal | Coal mine-by-plant-by-year panel of EIA-423 and EIA-923 coal mine data. | No |
| mine_var_names.csv | `data/xwalk` | `build/02_boilers/06_coal_mine_data_eia923.R` | Internal | Crosswalk to account for small changes in field names over time in EIA-423 and EIA-923 coal mine data. | Yes |
| nonattainment_by_cty_year.csv | `data` | `build/02_boilers/01_county_level_data.R` | Internal | County-by-year panel of nonattainment status. | No |
| phistory.csv | `data/epa` | `NA` | [EPA](https://www3.epa.gov/airquality/greenbook/downld/phistory.xls) | County attainment status from 1992 onwards sourced from the EPA Green Book. Note that coverage between 1978 to 1990 is included in \<phistory_1978_1990.csv\>. | Yes |
| phistory_1978_1990.csv | `data/epa` | `NA` | [EPA](NA) | County attainment status from 1978 to 1990 based upon work by Randy A. Becker, PhD, at the Center for Economic Studies, US Census Bureau and synthesized at the State and Local Programs Group Air Quality Policy Division Office of the EPA. | Yes |
| plants_1985_2005.csv | `data` | `build/02_boilers/02_eia767_1985-2005.R` | Internal | Plant-by-year panel of EIA-767 data between 1985 and 2005. | No |
| plants_1985_2018.csv | `data` | `build/02_boilers/04_merge_eia767_and_eia860.R` | Internal | Plant-by-year panel of EIA-767 and EIA-860 data between 1985 and 2018. | No |
| plants_2007_2018.csv | `data` | `build/02_boilers/03_eia860_2007-2018.R` | Internal | Plant-by-year panel of EIA-860 data between 2007 and 2018. | No |
| plants_fuel_1970_2000.csv | `data` | `build/02_boilers/05_fuel_and_fuel_comp_1970_2018.R` | Internal | Plant-by-year panel of EIA-759 coal data between 1970 and 2000. | No |
| readme.xlsx | `data` | `NA` | Internal | Tabular data for the README.md file. | Yes |
| regression_vars.csv | `data` | `build/02_boilers/09_generate_regression_vars.R` | Internal | Final synthesized boiler data. | No |
| regressions_ready_data.dta | `data` | `build/04_preprocessing/02_regression_data.do` | Internal | Final synthesized boiler data preprocessed for analysis. | No |
| so2_regulations_by_state_1976_to_2019.xlsx | `data/regs` | `NA` | Internal | State-by-year panel of regulations except Indiana site-specific regulations, which are located in \<indiana.xlsx\>. These data were manually collected. | Yes |
| stackflues_1985_2005.csv | `data` | `build/02_boilers/02_eia767_1985-2005.R` | Internal | Stackflue-by-year panel of EIA-767 data between 1985 and 2005. | No |
| stackflues_1985_2018.csv | `data` | `build/02_boilers/04_merge_eia767_and_eia860.R` | Internal | Stackflue-by-year panel of EIA-767 and EIA-860 data between 1985 and 2018. | No |
| stackflues_2007_2018.csv | `data` | `build/02_boilers/03_eia860_2007-2018.R` | Internal | Stackflue-by-year panel of EIA-860 data between 2007 and 2018. | No |
| state_code_xwalk.csv | `data/xwalk` | `NA` | Internal | Crosswalk from state name to state two-letter code. | Yes |
| state_fips_xwalk.csv | `data/xwalk` | `NA` | Internal | Crosswalk from state name to FIPS code. | Yes |
| sulfur_iv.csv | `data` | `build/04_preprocessing/01_sulfur.Rmd` | Internal | Plant-by-year panel of sulfur instrumental variables based on EIA-423 and EIA-923 coal data, and NTAD network data. | No |
| utilities_in_1996-97.csv | `data` | `build/02_boilers/08_feature_generation.R` | Internal | Utility-by-year panel of EIA-860 data between 1996 and 1997. | No |
| utils_2007_2018.csv | `data` | `build/02_boilers/03_eia860_2007-2018.R` | Internal | Utility-by-year panel of EIA-860 data between 2007 and 2018. | No |
| varname_xwalks.xlsx | `data/xwalk` | `NA` | Internal | Crosswalk to account for small changes in field names over times in EIA form data. | Yes |

## Computational requirements

The analysis for the paper can be performed on a personal computer. The
[software](#software) and [hardware](#hardware) requirements are
summarised below.

### Software

We use R as the backbone of our workflow. Stata and MySQL serve specific
but limited roles within our project.

All utilized software and packages are summarized in the table below.

| Software | Purpose | Packages |
|:---|:---|:---|
| R 4.3.1 | Data collection, cleaning, and analysis | assertr \[2.8\] |
|  |  | data.table \[1.15.4\] |
|  |  | foreign \[0.8-82\] |
|  |  | fs \[1.6.2\] |
|  |  | geofacet \[0.2.1\] |
|  |  | ggrepel \[0.9.1\] |
|  |  | glue \[1.6.2\] |
|  |  | haven \[2.5.2\] |
|  |  | here \[1.0.1\] |
|  |  | Hmisc \[5.2-0\] |
|  |  | httr \[1.4.6\] |
|  |  | knitr \[1.48\] |
|  |  | lmtest \[0.9-40\] |
|  |  | plotly \[4.10.4\] |
|  |  | readxl \[1.4.2\] |
|  |  | reshape2 \[1.4.4\] |
|  |  | rmarkdown \[2.29\] |
|  |  | RStata \[1.1.1\] |
|  |  | rvest \[1.0.3\] |
|  |  | sandwich \[3.0-2\] |
|  |  | scales \[1.2.1\] |
|  |  | sf \[1.0-15\] |
|  |  | sfnetworks \[0.6.3\] |
|  |  | stargazer \[5.2.3\] |
|  |  | tidygraph \[1.3.0\] |
|  |  | tidyverse \[2.0.0\] |
|  |  | writexl \[1.4.0\] |
|  |  | zip \[2.2.0\] |
|  |  | zoo \[1.8-11\] |
| Stata SE 16.1 | Regressions | grc1leg |
|  |  | regsave |
| MySQL 8.0 | CEMS data storage | NA |

### Hardware

The most recent run took about a week on a personal laptop running
Windows 10 Home with an Intel four-core CPU at 2.60GHz, 20GB of RAM, and
50GB of free space.

Approximate storage space required for the repository is about 3GB once
all public data has been collected. The CEMS SQL database requires
approximately 30GB of storage, due to the hourly resolution of the data.
In the [official replication
package](https://doi.org/10.7910/DVN/YOMSIL), we instead provide
synthesized CEMS datasets.

While most scripts run in a matter of minutes to hours, there are a few
which require days to complete. These time-intensive scripts include:

- `build/03_cems`;
- `build/04_preprocessing/01_sulfur.Rmd`; and,
- `build/05_analysis/08_plot_coefyr.R`.

## Code

Below we provide a high-level overview of the code along with a
file-by-file description of their purpose.

The code in this repository is licensed under the [GNU General Public
License v3.0 (GPL-3.0)](https://www.gnu.org/licenses/gpl-3.0.html). You
may use, share, and adapt all code with appropriate credit and with
derivations under the same license. See [LICENSE.md](LICENSE.md) for
details.

The main code folders can be summarized as follows:

- Programs in `build/01_eia` collect and clean EIA data, including but
  not limited to Forms EIA-767, EIA-860, EIA-861, and EIA-923. The
  automation script `01_eia/00_master.R` will run them all, where order
  is not important. Alternatively, all constituent programs can be run
  independently
- Programs in `build/02_boilers` synthesize the boiler dataset,
  primarily based on EIA data. The automation script
  `02_boilers/00_master.R` will run them all where order is important.
- Programs in `build/03_cems` collect, ingest, and clean EPA CEMS data,
  where order is important.
- Programs in `build/04_preprocessing` prepare the sulfur instrumental
  variables as well as the final regression dataset. The automation
  script `04_preprocessing/00_master.R` will run them both, where order
  is not important. Alternatively, all constituent programs can be run
  independently.
- Programs in `build/05_analysis` perform the analysis and output all
  tables and figures. The automation script `05_analysis/00_master.R`
  will run them all. Order is important for programs `04_regs_data.R`
  through `08_plot_coefyr.R`; otherwise, the remaining programs are
  independent and order is not essential.
- Functions in `src` assist the `build` programs to complete their
  tasks.

The following table describes the code within the `build` and `src`
folders. Further details are provided within the scripts themselves.

| File | Location | Description |
|:---|:---|:---|
| EIA_423923.Rmd | `build/01_eia` | This workbook is a part of the data cleaning process. It downloads and cleans data from EIA-923 and its predecessor, EIA-423. The forms contain monthly fuel receipts for plants with a fossil-fueled nameplate generating capacity of 50+ MW. It returns \<eia_fuel.xlsx\> and \<eia_sulfur.csv\>, which contain plant- and state-by-month panels of fuel prices and mine and county cross-section of sulfur contents, respectively. |
| EIA_759.Rmd | `build/01_eia` | This workbook is part of the data cleaning process. It downloads data from EIA-759, a predecessor to EIA-923. The forms contain monthly power plant data. It forms part of the \<regression_vars.csv\> dataset which contains a boiler-by-year panel. |
| EIA_767.Rmd | `build/01_eia` | This workbook is part of the data cleaning process. It downloads data from EIA-767, a predecessor to EIA-923. The forms contain plant operations and equipment design data. It forms part of the \<regression_vars.csv\> dataset which contains a boiler-by-year panel. |
| EIA_860.Rmd | `build/01_eia` | This workbook is part of the data cleaning process. It downloads data from EIA-860. The forms contain electric generator data. It forms part of the \<regression_vars.csv\> dataset which contains a boiler-by-year panel. |
| EIA_861.Rmd | `build/01_eia` | This workbook is part of the data cleaning process. It downloads and cleans annual sales by utility from the EIA. Specifically, it utilizes the “Sales to Ultimate Customers” table reported in form EIA-861. It returns \<eia_sales.csv\> which contains a utility-by-year panel of electricity sales. |
| EIA_906923.Rmd | `build/01_eia` | This workbook is a part of the data cleaning process. It downloads and cleans data from EIA-923 and its predecessors, EIA-906 and EIA-920. The forms provide monthly and annual data on generation and fuel consumption at the power plant and prime mover level. A subset of plants – steam-electric plants 10 MW and above – also provide boiler level and generator level data. It returns \<eia_netgen.csv\> and \<eia_netgen_unit.csv\>, which contain plant- and boiler-by-month panels of net generation, respectively. |
| EIA_capacity.Rmd | `build/01_eia` | This workbook is a part of the data cleaning process. It downloads and cleans annual capacity by state and fuel source from the EIA. Specifically, it utilizes the “Existing Nameplate and Net Summer Capacity by Energy Source, Producer Type and State” table based on annual data reported in form EIA-860. It returns \<eia_capacity.csv\>, which contains a state-by-fuel-by-year panel of electricity capacities. |
| 00_master.R | `build/02_boilers` | This program provides a preamble and automates the other scripts in the `build/02_boilers` subfolder with the aim of performing boiler data cleaning for the Grandfathering project. |
| 01_county_level_data.R | `build/02_boilers` | This program This script synthesizes county centerpoints and county non-attainment by year, returning \<ctrpoints_counties.csv\> and \<nonattainment_by_cty_year.csv\>, respectively. |
| 02_eia767_1985-2005.R | `build/02_boilers` | This program creates the boiler sample from 1985 to 2005. It combines data from Form EIA-767 on coal plants, in particular location, heat rate, nameplate, and scrubbers. It returns \<boilers_1985_2005.csv\>, \<fgds_1985_2005.csv\>, \<plants_1985_2005.csv\>, \<generators_1985_2005.csv\>, and \<stackflues_1985_2005.csv\>. |
| 03_eia860_2007-2018.R | `build/02_boilers` | This program creates the boiler sample from 2007 to 2018, where it synthesizes data from Form EIA-860. It returns \<boilers_2007_2018.csv\>, \<fgds_2007_2018.csv\>, \<generators_2007_2018.csv\>, \<plants_2007_2018.csv\>, \<stackflues_2007_2018.csv\>, and \<utils_2007_2018.csv\>. |
| 04_merge_eia767_and_eia860.R | `build/02_boilers` | This program merges Forms EIA-767 and EIA-860, which contain most of the relevant boiler data from different time periods. It returns \<boilers_1985_2018.csv\>, \<fgds_1985_2018.csv\>, \<plants_1985_2018.csv\>, \<generators_1985_2018.csv\>, and \<stackflues_1985_2018.csv\>. |
| 05_fuel_and_fuel_comp_1970_2018.R | `build/02_boilers` | This program merges fuel data from Forms EIA-767, EIA-923, and EIA-867 between 1970 and 2018. It synthesizes coal plant location, heat rate, nameplate, and scrubber information. It returns \<plants_fuel_1970_2000.csv\>, \<all_fuel_1970_2018.csv\>, and \<boilers_fuel_comp_1985_2018.csv\>. |
| 06_coal_mine_data_eia923.R | `build/02_boilers` | This program merges coal mine data from Forms EIA-423 and EIA-923 between 1970 and 2018. It returns \<coal_char_data_by_plant_year.csv\> and \<mine_data_long.csv\>. |
| 07_merge_boilers_to_regulations.R | `build/02_boilers` | This program merges boiler and regulation data. It returns \<merged_regs_and_plants.csv\>. |
| 08_feature_generation.R | `build/02_boilers` | This program merges all data, generates additional useful features, eliminates unnecessary variables, and conducts additional data cleaning. It returns \<utilities_in_1996-97.csv\> and \<all_years_all_plants_and_features.csv\>. |
| 09_generate_regression_vars.R | `build/02_boilers` | This program among other things generates a variable to indicate boiler modification, creates rolling variables for generation and sulfur content, and performs additional data cleaning. It returns \<regression_vars.csv\>. |
| 01_cems_scrape.R | `build/03_cems` | This program is the first in a series related to EPA CEMS data; it scrapes CEMS data from the EPA FTP website. |
| 02_cems_ingestion.R | `build/03_cems` | This program is the second in a series related to EPA CEMS data; it synthesizes all available CEMS data and transfers it to the local MySQL epa database. |
| 03_cems_query.R | `build/03_cems` | This program is the third in a series related to EPA CEMS data; it queries CEMS data from the MySQL epa database, builds a GF-EPA crosswalk by boiler id, and returns aggregated CEMS data by hour and year. |
| 00_master.R | `build/04_preprocessing` | This program provides a preamble and automates the other scripts in the `build/04_preprocessing` subfolder with the aim of preparing the data for analysis. |
| 01_sulfur.Rmd | `build/04_preprocessing` | This workbook forms a part of the data cleaning process. It prepares a sulfur instrumental variable (IV) based on plant distance to low sulfur coal. It utilizes cleaned data from EIA-923 as well as its predecessors and network data from the NTAD. It returns \<sulfur_iv.csv\>. |
| 02_regression_data.do | `build/04_preprocessing` | This program preprocesses the data in advance of the analysis for the Grandfathering project. It returns … |
| 00_master.R | `build/05_analysis` | This program provides a preamble and automates the other scripts in the `build/05_analysis` subfolder with the aim of executing the analysis for the Grandfathering project. |
| 01_plot_survival.R | `build/05_analysis` | This program is a part of the analysis. It prepares a survival share bar plot returning \<fig1.pdf\>. |
| 02_tbl_balance.Rmd | `build/05_analysis` | This workbook is a part of the analysis. It prepares data balance tables using boiler data, EPA-EIA crosswalk, and EPA facility attributes. It returns \<tbl5.tex\> and \<tbl6.tex\>. It also returns \<fig2.pdf\>, which contains a map associating boiler NSR grandfathering share with their plant coordinates. |
| 03_plot_local_reg.R | `build/05_analysis` | This program is a part of the analysis. It prepares a local regulations plot by state and year. It returns \<fig3.pdf\>. |
| 04_regs_data.R | `build/05_analysis` | This program is part of the analysis. It prepares the cleaned boiler data for the regression analysis that follows in \<05_regs_main.R\>, \<06_regs_robust.R\>, and \<07_regs_netgen.R\>. |
| 05_regs_main.R | `build/05_analysis` | This program is part of the analysis. It performs the main regressions for utilization, survival, and emissions along with their first stages. It returns \<tbl1.tex\> and \<tbl2.tex\>, respectively. |
| 06_regs_robust.R | `build/05_analysis` | This program is part of the analysis. It performs the robustness regressions based on ‘fully vs partially grandfathered’, ‘non-linear age’, ‘spillover’, and ‘anticipation’. It returns \<tbl3.tex\>, \<tbl8.tex\>, and \<tbl9.tex\>. |
| 07_regs_netgen.R | `build/05_analysis` | This program is a part of the analysis. It calculates the the net-to-gross generation ratio (GR) for as many grandfathered boilers/power plants as possible. It utilizes hourly gross generation data from EPA CEMS and monthly net generation data from EIA-923 and its predecessors. It performs a set of naive regressions and returns \<tbl2.tex\>. |
| 08_plot_coefyr.R | `build/05_analysis` | This program is part of the analysis. It prepares a series of plots on the time-dependent effects of grandfathering and returns \<fig4a.pdf\>, \<fig4b.pdf\>, and \<fig4c.pdf\>. |
| 09_plot_coef_cutoff.do | `build/05_analysis` | This program is part of the analysis. It prepares a coefficient cutoff plot, including utilization, survival, and emissions. It returns \<fig5.png\>. |
| 10_plot_reg_dist.do | `build/05_analysis` | This program is part of the analysis. It prepares a distribution of sulfur dioxide regulations plot and returns \<fig6.png\>. |
| boilers.R | `src` | This script provides helper functions related to the boiler dataset prepared in `build/02_boilers`. |
| def_paths.R | `src` | This script provides directory paths within the Grandfathering repository. It is legacy code primarily used in `build/01_eia`, and otherwise, replaced with the here package. |
| dir_class.R | `src` | This script provides helper functions for \<def_paths.R\>, namely to extract a path’s base folder name and add that name as a class type. |
| find_newfiles.R | `src` | This script identifies new online data files for EPA CEMS compared to the local storage of these files. It is called in \<02_cems_ingestion.R\> only. |
| networks.R | `src` | This script provides helper functions related to the sf_networks package and network cleaning. It is called in \<01_sulfur.Rmd\> only. |
| plots.R | `src` | This script provides a set of ggplot2 theme functions. |
| preamble.R | `src` | This program provides some of the necessary packages for the Grandfathering repository. |
| sql_data_transfer.R | `src` | This script provides the functions necessary to transfer EPA CEMS raw files into the MySQL epa database. It is only used in the `build/01_eia` subfolder. |
| sql_db_epa.R | `src` | This script creates the epa schema within the local MySQL database. The schema is populated with EPA CEMS data. It is only used in the `build/01_eia` subfolder. |
| sql_tbl_cems.R | `src` | This script creates the cems table within the MySQL epa database. This table is populated with EPA CEMS cleaned data. It is only used in the `build/01_eia` subfolder. |
| sql_tbl_filelog.R | `src` | This script creates the filelog table within the MySQL epa database. This table is populated with EPA CEMS) raw data files which have been ingested successfully into the MySQL database. It is only used in the `build/01_eia` subfolder. |
| sql_tbl_xwalk.R | `src` | This script creates the xwalk table within the MySQL epa database. This table is populated with EPA-EIA power plant crosswalk data. It is only used in the `build/01_eia` subfolder. |
| stata_regs.R | `src` | This script provides functions necessary to perform analysis regressions in Stata. It is only used in the `build/05_analysis` subfolder. |
| text_colour.R | `src` | This script provides a text colouring function for workbooks within the Grandfathering project. |

## Instructions

The previous sections describe the data and software necessary to
conduct a replication. This section provides human-readable instruction
for performing a replication immediately below, along with a table
mapping outputs-to-code.

1.  Run all Rmd files in `build/01_eia` in any order. Use of the
    automation script `bulid/01_eia/00_master.R` is optional. Produced
    files include:
    - eia_capacity.csv
    - eia_fuel.xlsx
    - eia_netgen_unit.csv
    - eia_netgen.csv
    - eia_sales.csv
    - eia_sulfur.csv
2.  Run all R files in `build/02_boilers` in the order provided. Use of
    the automation script `bulid/02_boilers/00_master.R` is required.
    Note that `build/02_boilers/07_merge_boilers_to_regulations.R`
    requires manual intervention to prepare
    `handmatched_pa_maine_nm_mass.csv`. Produced files include:
    - regression_vars.csv
    - coal_char_data_by_plant_year.csv
3.  Run all R files in `build/03_cems` in the order provided. Note that
    `build/03_cems/03_cems_query.R` requires manual intervention to
    prepare `data/xwalk/gf_cems_xwalk.dta`. Produced files include:
    - cems_yr.dta
4.  Run all R and do files in `build/04_preprocessing` in any order. Use
    of the automation script `bulid/04_preprocessing/00_master.R` is
    optional. Produced files include:
    - sulfur_iv.csv
    - regressions_ready_data.dta
5.  Run all R, Rmd, and do files in `build/05_analysis` in the order
    provided. Use of the automation script
    `bulid/05_analysis/00_master.R` is required. Produced files are
    summarized in the output-code table below.

| Reference | File | Code | Title |
|:---|:---|:---|:---|
| Figure 1 | fig1.pdf | `build/05_analysis/01_plot_survival.R` | Propensity to survive until 2014 for boilers from different vintages |
| Figure 2 | fig2.pdf | `build/05_analysis/02_tbl_balance.Rmd` | Coal-fired boilers by initial NSR grandfathering status |
| Figure 3 | fig3.pdf | `build/05_analysis/03_plot_local_reg.R` | Mean inverse local regulations by state and year |
| Figure 4 | fig4a.pdf | `build/05_analysis/08_plot_coefyr.R` | Time-dependent effects of NSR grandfathering: Hourly utilization |
| Figure 4 | fig4b.pdf | `build/05_analysis/08_plot_coefyr.R` | Time-dependent effects of NSR grandfathering: Survival |
| Figure 4 | fig4c.pdf | `build/05_analysis/08_plot_coefyr.R` | Time-dependent effects of NSR grandfathering: Sulfur emissions rate |
| Figure 5 | fig5.png | `build/05_analysis/09_plot_coef_cutoff.do` | Grandfathering coefficient estimates depending on the set of considered boilers |
| Figure 6 | fig6.png | `build/05_analysis/10_plot_reg_dist.do` | Distribution of sulfur dioxide regulations in the data |
| Table 1 | tbl1.tex | `build/05_analysis/05_regs_main.R` | Main regression results |
| Table 2 | tbl2.tex | `build/05_analysis/07_regs_netgen.R` | Net-to-gross generation ratio regressions |
| Table 3 | tbl3.tex | `build/05_analysis/06_regs_robust.R` | Fully versus partially grandfathered regression results |
| Table 4 | N/A | `N/A` | Variables summary |
| Table 5 | tbl5.tex | `build/05_analysis/02_tbl_balance.Rmd` | Average characteristics of boilers by NSR grandfathering status |
| Table 6 | tbl6.tex | `build/05_analysis/02_tbl_balance.Rmd` | Average characteristics of boilers by local regulation |
| Table 7 | tbl7.tex | `build/05_analysis/05_regs_main.R` | Main regression first-stage results |
| Table 8 | tbl8.tex | `build/05_analysis/06_regs_robust.R` | Robustness regression results |
| Table 9 | tbl9.tex | `build/05_analysis/06_regs_robust.R` | Anticipation regression results |

## Citation

Please cite the paper as:

> Bialek-Gregory, Sylwia, Jack Gregory, Bridget Pals, and Richard
> Revesz. Forthcoming.  
> “[Still your grandfather’s boiler: Estimating the effects of the Clean
> Air Act’s grandfathering
> provisions](https://www.journals.uchicago.edu/doi/10.1086/734214).”  
> *Journal of the Association of Environmental and Resource Economists*.

Please cite the repository as:

> Bialek-Gregory, Sylwia, Jack Gregory, Bridget Pals, and Richard
> Revesz. Forthcoming.  
> *Grandfathering*. Version 4.0. GitHub repository. Available at
> <https://github.com/jack-gregory/Grandfathering>.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-Huetteman_etal2021" class="csl-entry">

Huetteman, J, J Tafoya, T Johnson, and J Schreifels. 2021. “EPA-EIA
Power Sector Data Crosswalk.” United States Environmental Protection
Agency (EPA).
<https://www.epa.gov/airmarkets/power-sector-data-crosswalk>.

</div>

<div id="ref-DOT2022" class="csl-entry">

US DOT. 2022. “National Transportation Atlas Database.” Bureau of
Transportation Statistics. <https://www.bts.gov/ntad>.

</div>

<div id="ref-EIA2005_767" class="csl-entry">

US EIA. 2005. “Form EIA-767: Steam-Electric Plant Operation and Design
Report.” U.S. Department of Energy.
<https://www.eia.gov/electricity/data/eia767/>.

</div>

<div id="ref-EIA2019_860" class="csl-entry">

———. 2019a. “Form EIA-860: Annual Electric Generator Report.” U.S.
Department of Energy. <https://www.eia.gov/electricity/data/eia860/>.

</div>

<div id="ref-EIA2019_923" class="csl-entry">

———. 2019b. “Form EIA-923: Power Plant Operations Report.”
<https://www.eia.gov/electricity/data/eia923/>.

</div>

<div id="ref-EIA2019_State" class="csl-entry">

———. 2019c. “State Electricity Data.” U.S. Department of Energy.
<https://www.eia.gov/electricity/data/state/>.

</div>

<div id="ref-EIA2019_861" class="csl-entry">

———. 2021a. “Form EIA-861: Annual Electric Power Industry Report.” U.S.
Department of Energy. <https://www.eia.gov/electricity/data/eia861/>.

</div>

<div id="ref-EIA2021_Atlas" class="csl-entry">

———. 2021b. “US Energy Atlas.” <https://atlas.eia.gov/>.

</div>

<div id="ref-EIA2011_423" class="csl-entry">

———. 2023. “Form EIA-423: Monthly Cost and Quality of Fuels for Electric
Plants Report.” U.S. Department of Energy.
<https://www.eia.gov/electricity/data/eia423/>.

</div>

<div id="ref-EPA2021_Nonattainment" class="csl-entry">

US EPA. 2021a. “County Nonattainment Data.”
<https://www3.epa.gov/airquality/greenbook/downld/phistory.xls>.

</div>

<div id="ref-EPA2021_Power" class="csl-entry">

———. 2021b. “Power Sector Data.” Office of Atmospheric Protection, Clean
Air; Power Division. <https://campd.epa.gov>.

</div>

</div>

[^1]: Continuous emission monitoring system (CEMS) data published by the
    US Environmental Protection Agency (EPA).
