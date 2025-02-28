** %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
** Grandfathering
** Analysis -- Plots -- Coef Cutoff
** Sylwia Bialek
** 08 December 2024
** %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


*** START CODE ***


** PREAMBLE ---------------------------------------------------------------------------------------

** Set 
** ... definitions
set matsize 900, permanently

** ... date
global date = string(year("$S_DATE")) + string(month("$S_DATE"), "%02.0f") ///
  + string(day("$S_DATE"), "%02.0f")

** ... paths
global path_data "your_path_here\data"
global path_output "your_path_here\out\$date"
cap mkdir $path_output

** ... regressions
global IOUplus_cond "(ut_type==4|ut_type==2|ut_type==5)"
global LHS survive SO2 DURATION

** ... years
** A cutoff of 1950 for the birth of boilers is considered in most regressions
** in the paper.  We want to mark it as a vertical line on the graph. 
global cutoff_year 1950
global windowCons=(1978-$cutoff_year)
** Set the years to be considered
global start_year=1922
global last_year=1975


** Import data
use "$path_data\regressions_ready_data.dta", clear


** COEFFICIENT CUTOFF PLOTS -----------------------------------------------------------------------
** (Figure 5)

local lhs = 0
foreach i of global LHS {
	  
	  ** Increase iteration counter
		local ++lhs
		putexcel set "$path_data\Estimates.xlsx", sheet("Results_`i'", replace) modify
		local row=1
	
		quietly {	
			putexcel B1 = "GF_eff"
			putexcel C1 = "capacity_eff"
			putexcel D1 = "nonattain_eff"
			putexcel E1 = "applic_reg_eff"
			putexcel F1 = "GF-SE"
			putexcel G1 = "capacity_eff-SE"
			putexcel H1 = "nonattain_eff-SE"
			putexcel I1 = "applic_reg_eff-SE"
			putexcel J1 = "GF- p-value"
			putexcel K1 = "capacity_eff p-value"
			putexcel L1 = "nonattain_eff p-value"
			putexcel M1 = "applic_reg_eff p-value"
			putexcel N1 = "GF_lowCI"
			putexcel P1 = "capacity_lowCI"
			putexcel S1 = "nonattain_lowCI"
			putexcel U1 = "applic_reg_lowCI"
			putexcel O1 = "GF_highCI"
			putexcel R1 = "capacity_highCI"
			putexcel T1 = "nonattain_highCI"
			putexcel V1 = "applic_reg_highCI"
			putexcel A1 = "Window_width"
		}
		
		forvalues j= $start_year(1)$last_year{ 
			local row=`row'+1
			local width=1978-`j'
			putexcel set "$path_data\Estimates.xlsx", sheet("Results_`i'") modify
			putexcel A`row' =`width'
			preserve
	
			*We construct a symmetric window 
			drop if inservice_y<`j'| inservice_y>1978+`width'

  		if `i'==survive {
				drop if capacity<74 & ut_type==4
				drop if ut_type==5 & year<1990
				drop if year==2018
				di "above 73 MW"
			}

			qui reg `i' Gf ${full_`i'} if $IOUplus_cond, vce(cluster ID)

			quietly {
				putexcel B`row' = _b[Gf]
				putexcel C`row' = _b[capacity_gf]
				putexcel D`row' = _b[so2_nonat_Gf]
				putexcel E`row' = _b[applic_reg_Gf]
				putexcel F`row' = _se[Gf]
				putexcel G`row' = _se[capacity_gf]
				putexcel H`row' = _se[so2_nonat_Gf]
				putexcel I`row' = _se[applic_reg_Gf]
				local t = _b[Gf]/_se[Gf]
				local p=2*ttail(e(df_r),abs(`t'))
				putexcel J`row'=`p'
				local t = _b[capacity_gf]/_se[capacity_gf]
				local p=2*ttail(e(df_r),abs(`t'))
				putexcel K`row'=`p'
				local t = _b[so2_nonat_Gf]/_se[so2_nonat_Gf]
				local p=2*ttail(e(df_r),abs(`t'))
				putexcel L`row'=`p'
				local t = _b[applic_reg_Gf]/_se[applic_reg_Gf]
				local p=2*ttail(e(df_r),abs(`t'))
				putexcel M`row'=`p'
				local p=_b[Gf] - invttail(e(df_r),0.025)*_se[Gf]
				putexcel N`row'=`p'			
				local p=_b[Gf] + invttail(e(df_r),0.025)*_se[Gf]
				putexcel O`row'=`p'	
				local p=_b[so2_nonat_Gf] - invttail(e(df_r),0.025)*_se[so2_nonat_Gf]
				putexcel S`row'=`p'			
				local p=_b[so2_nonat_Gf] + invttail(e(df_r),0.025)*_se[so2_nonat_Gf]
				putexcel T`row'=`p'	
				local p=_b[applic_reg_Gf] - invttail(e(df_r),0.025)*_se[applic_reg_Gf]
				putexcel U`row'=`p'			
				local p=_b[applic_reg_Gf] + invttail(e(df_r),0.025)*_se[applic_reg_Gf]
				putexcel V`row'=`p'	
			}
			restore
	}
}

** Go to the generated Excel file and manually change 

foreach i of global LHS {
	
	if "`i'"=="DURATION" {
		local j="Utilization"
		global legend dd
	}
	else if "`i'"=="survive" {
		local j="Survival"
		global legend leg(off) 
	}
	else {
		local j="Emissions"
		global legend leg(off) 
	}
	
	import excel "$path_data\Estimates.xlsx", clear sheet(Results_`i') firstrow

	twoway (rarea GF_highCI GF_lowCI Window_width, color(gs13%55)) ///
	  (rarea nonattain_highCI nonattain_lowCI Window_width, color(gs13%55)) /// 
		(rarea applic_reg_highCI applic_reg_lowCI Window_width, color(gs13%45)) ///
		(line  GF_eff  Window_width, lpattern(shortdash_dot) lwidth(thick)) ///
		(line nonattain_eff Window_width) (line  applic_reg_eff Window_width, lpattern(dash)), ///
		xtitle("Max. distance in years to 1978",size(vsmall)) ///
		ytitle("Estimated coefficient", size(vsmall)) ///
		title("`j'", position(16) ring(1) size(small)) saving(`i', replace) ///
		legend(order(4 "Grandfathering (Gf)" 5 "Nonattainment # Gf" ///
		6 "Applicable regulations # Gf" 1 "95% Confidence interval") size(small)) ///
		xlabel(, labsize(vsmall)) ylabel(, labsize(small)) ///
		xline($windowCons) graphregion(color(white)) bgcolor(white)	   
}

grc1leg DURATION.gph SO2.gph survive.gph, col(3) iscale(1) graphregion(color(white))
graph export "$path_output\fig5.png", as(png) name("Graph") replace


*** END CODE ***

