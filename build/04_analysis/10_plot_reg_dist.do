** %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
** Grandfathering
** Analysis -- Plots -- Regulation Dist
** Sylwia Bialek
** 08 December 2024
** %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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


** Import data
use "$path_data\regressions_ready_data.dta", clear


** REGULATION DISTRIBUTION PLOT -------------------------------------------------------------------
** (Figure 6)

drop if (year<1985| year==2006 )
gen applic_reg_notInverted=1/applic_reg

twoway (histogram applic_reg if $IOUplus_cond & Gf==1, start(0) width(0.25) color(red%30)) ///
  (histogram applic_reg if $IOUplus_cond & Gf==0, start(0) width(0.25) fcolor(none) lcolor(black)), ///
  legend(order(1 "Grandfathered" 2 "Subject to NSR" ) size(vsmall)) ///
  xtitle("Local regulations (MMBtu/lbs)", size(vsmall)) ytitle("Density", size(vsmall)) ///
  title("Distribution of the inverse of local SO2 regulations", position(12) ring(0) size(small)) ///
  saving(Inverse, replace) graphregion(color(white)) bgcolor(white) 
	   
twoway (histogram applic_reg_notInverted if $IOUplus_cond & Gf==1, start(0) width(0.5) color(red%30)) ///
  (histogram applic_reg_notInverted if $IOUplus_cond & Gf==0, start(0) width(0.5) fcolor(none) lcolor(black)), ///
  leg(off) xtitle("Local regulations (MMBtu/lbs)", size(vsmall)) ytitle("Density", size(vsmall)) ///
  title("Distribution of the local SO2 regulations", position(12) ring(0) size(small)) ///
  saving(NotInverse, replace)  graphregion(color(white)) bgcolor(white)

gr combine NotInverse.gph Inverse.gph, col(1) iscale(1) graphregion(color(white)) 
graph export "$path_output\fig6.png", as(png) name("Graph") replace


*** END CODE ***

