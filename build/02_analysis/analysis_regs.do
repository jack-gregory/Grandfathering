

** (1) Convert to panel???
*egen id = group(plant_code boiler_id)
*xtset id year

** (2) Outcome vars
* +survive
* +hr_load
* sulfur_content_tot

** (3) Grandfathering var
* +With switching (grand_NSR)
* +Without switching (grand_NSR_const)

** (4) Owner
* +Include categorical (ut_type)

** (5) Fixed effects
* GF##Year (i.e. year fes + GF#year fes)
* GF#Year#Attain (i.e. GF? + GF#year#attain fes)
* techn#Year


est clear

** Definitions
global wd "C:\Users\Jack\Dropbox\Grandfathering"
global date "20210719"

** Import grandfathering data
use "$wd\data\gf_original\regressions_ready_data.dta", clear
drop if plant_code==.
drop if year<1985
drop if year==2006

** Remove duplicates across all variables
unab vlist : _all
bysort `vlist':  gen dup = cond(_N==1,0,_n)
order plant_code boiler_id year dup
tab dup
drop if dup>1
drop dup

** Remove duplicates across plant_code/boiler_id/year
** NB - These are due to differences in race_blk
bysort plant_code boiler_id year: gen dup = cond(_N==1,0,_n)
order plant_code boiler_id year dup
tab dup
drop if dup>1
drop dup

/*
** Merge with yearly CEMS data
preserve
	import delimited "$wd\data\cems_yr.csv", clear
	drop n
	rename orispl plant_code
	rename unit boiler_id
	rename duration hr_cems
	destring gload-nox_mass, ignore(NA) replace
	tempfile cems
	save `cems'
restore

*drop if year<1997
merge 1:1 plant_code boiler_id year using `cems', nogen //keep(match)
*/

/*
bysort plant_code boiler_id grand_NSR_const: gen id = _n
keep if id==1
drop id
bysort plant_code: gen id = _n
keep if id==1
drop id
*/

*rename survive sur
*rename hr_load ops
*rename sulfur_content_tot ems

foreach out of varlist survive /*hr_load*/ {
*foreach out of varlist survive hr_load hr_cems gload co2_mass nox_mass so2_mass {
	foreach gf in "" "_const" {

		est clear
		
		** Base
		eststo: reg `out' i.grand_NSR`gf'##i.so2_nonattain, vce(robust)
		estadd local boiler ""
		estadd local econ ""
		estadd local year ""
		estadd local state ""
		estadd local owner ""

		** Boiler Characteristics
		eststo: reg `out' i.grand_NSR`gf'##i.so2_nonattain ///
			age i.grand_NSR`gf'##c.max_boi_nameplate, vce(robust)
		estadd local boiler "X"
		estadd local econ ""
		estadd local year ""
		estadd local state ""
		estadd local owner ""

		** Federal + Local Regulation
		eststo: reg `out' i.grand_NSR`gf'##i.so2_nonattain ///											** Base
			applic_reg`gf' /// 																												** Local
			i.ARP_subject c.sulfur_content_tot c.ARPprice_sp#c.sulfur_content_tot ///	** ARP
			age i.grand_NSR`gf'##c.max_boi_nameplate, vce(robust)
		estadd local boiler "X"
		estadd local econ ""
		estadd local year ""
		estadd local state ""
		estadd local owner ""

		** Economic Characteristics
		eststo: reg `out' i.grand_NSR`gf'##i.so2_nonattain applic_reg`gf' ///
			i.ARP_subject c.sulfur_content_tot c.ARPprice_sp#c.sulfur_content_tot ///
			age i.grand_NSR`gf'##c.max_boi_nameplate d_growth, vce(robust)
		estadd local boiler "X"
		estadd local econ "X"
		estadd local year ""
		estadd local state ""
		estadd local owner ""

		** Owner Type
		eststo: reg `out' i.grand_NSR`gf'##i.so2_nonattain applic_reg`gf' ///
			i.ARP_subject c.sulfur_content_tot c.ARPprice_sp#c.sulfur_content_tot ///
			age i.grand_NSR`gf'##c.max_boi_nameplate d_growth i.ut_type, vce(robust)
		estadd local boiler "X"
		estadd local econ "X"
		estadd local owner "X"
		estadd local year ""
		estadd local state ""
		
		** Year FEs
		eststo: reg `out' i.grand_NSR`gf'##i.so2_nonattain applic_reg`gf' ///
			i.ARP_subject c.sulfur_content_tot c.ARPprice_sp#c.sulfur_content_tot ///
			age i.grand_NSR`gf'##c.max_boi_nameplate d_growth i.ut_type i.states, vce(robust)
		estadd local boiler "X"
		estadd local econ "X"
		estadd local owner "X"
		estadd local state "X"
		estadd local year ""

		** State FEs
		eststo: reg `out' i.grand_NSR`gf'##i.so2_nonattain applic_reg`gf' ///
			i.ARP_subject c.sulfur_content_tot c.ARPprice_sp#c.sulfur_content_tot ///
			age i.grand_NSR`gf'##c.max_boi_nameplate d_growth i.ut_type i.year i.states, ///
			vce(robust)
		estadd local boiler "X"
		estadd local econ "X"
		estadd local owner "X"
		estadd local state "X"
		estadd local year "X"
		
		** Reg table
		/*#delim ;
		esttab using "$wd/out/$date/tbl.`out'_grand_NSR`gf'.tex", replace
			nodep noomit nobase
			keep(1.grand_NSR`gf' 1.so2_nonattain 1.grand_NSR`gf'#1.so2_nonattain 
				applic_reg`gf' 1.ARP_subject sulfur_content_tot 
				c.ARPprice_sp#c.sulfur_content_tot age max_boi_nameplate 
				1.grand_NSR`gf'#c.max_boi_nameplate d_growth)
			coef(1.grand_NSR`gf' "GF" 1.so2_nonattain "NA" 
				1.grand_NSR`gf'#1.so2_nonattain "GF x NA" applic_reg`gf' "GF x Local" 
				1.ARP_subject "ARP" sulfur_content_tot "SO\$_2\$"
				c.ARPprice_sp#c.sulfur_content_tot "ARP Price x SO\$_2\$" age "Age"
				max_boi_nameplate "Size" 1.grand_NSR`gf'#c.max_boi_nameplate "GF x Size"
				d_growth "Demand")
			t scalars(year state owner) r2 ar2  
			/*compress*/
		;
		#delim cr*/
		#delim ;
		esttab using "$wd/out/$date/tbl.`out'_grand_NSR`gf'_short.tex", replace
			nodep noomit nobase
			keep(1.grand_NSR`gf' 1.so2_nonattain 1.grand_NSR`gf'#1.so2_nonattain 
				applic_reg`gf' 1.ARP_subject sulfur_content_tot 
				c.ARPprice_sp#c.sulfur_content_tot)
			coef(1.grand_NSR`gf' "GF" 1.so2_nonattain "NA" 
				1.grand_NSR`gf'#1.so2_nonattain "GF x NA" applic_reg`gf' "GF x Local" 
				1.ARP_subject "ARP" sulfur_content_tot "SO\$_2\$"
				c.ARPprice_sp#c.sulfur_content_tot "ARP Price x SO\$_2\$")
			t scalars(boiler econ year state owner) r2 ar2  
			/*compress*/
		;
		#delim cr
	}
}



** -----------------------------------------------------------------------------

foreach out of varlist survive /*hr_load*/ {
	foreach gf in "" "_const" {

		est clear
		
		** Base
		eststo: reg `out' i.grand_NSR`gf', vce(robust)
		estadd local boiler ""
		estadd local econ ""
		estadd local year ""
		estadd local state ""
		estadd local owner ""

		** Boiler Characteristics
		eststo: reg `out' i.grand_NSR`gf' ///
			age i.grand_NSR`gf'##c.max_boi_nameplate, vce(robust)
		estadd local boiler "X"
		estadd local econ ""
		estadd local year ""
		estadd local state ""
		estadd local owner ""

		** Economic Characteristics
		eststo: reg `out' i.grand_NSR`gf' ///
			age i.grand_NSR`gf'##c.max_boi_nameplate d_growth, vce(robust)
		estadd local boiler "X"
		estadd local econ "X"
		estadd local year ""
		estadd local state ""
		estadd local owner ""

		** Owner Type
		eststo: reg `out' i.grand_NSR`gf' ///
			age i.grand_NSR`gf'##c.max_boi_nameplate d_growth i.ut_type, vce(robust)
		estadd local boiler "X"
		estadd local econ "X"
		estadd local owner "X"
		estadd local state ""
		estadd local year ""
		
		** State FEs
		eststo: reg `out' i.grand_NSR`gf' ///
			age i.grand_NSR`gf'##c.max_boi_nameplate d_growth i.ut_type i.states, ///
			vce(robust)
		estadd local boiler "X"
		estadd local econ "X"
		estadd local owner "X"
		estadd local state "X"
		estadd local year ""
		
		** Year FEs
		eststo: reg `out' i.grand_NSR`gf' ///
			age i.grand_NSR`gf'##c.max_boi_nameplate d_growth i.ut_type i.states ///
			i.year, vce(robust)
		estadd local boiler "X"
		estadd local econ "X"
		estadd local owner "X"
		estadd local state "X"
		estadd local year "X"

		** Reg table
		#delim ;
		esttab using "$wd/out/$date/tbl.`out'_grand_NSR`gf'_noreg.tex", replace
			nodep noomit nobase
			keep(1.grand_NSR`gf')
			coef(1.grand_NSR`gf' "GF")
			t scalars(boiler econ year state owner) r2 ar2  
			/*compress*/
		;
		#delim cr
	}
}



