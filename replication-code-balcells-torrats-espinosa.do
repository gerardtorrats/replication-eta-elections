*******************************************************************************************************************
*** Replication Code for "Using a Natural Experiment to Estimate the Electoral Consequences of Terrorist Attacks"
*** Authors: Laia Balcells & Gerard Torrats-Espinosa
*** Created: 09/26/2018

*** Input: File "replication-balcells-torrats-espinosa.dta" (set working directory accordingly)
*** Outputs: One .pdf file for each figure (set working directory accordingly) 
*******************************************************************************************************************

*** Import data
set more off
use "replication-balcells-torrats-espinosa.dta",clear

*** Macros for Covariates
local voteprior  votedPP votedPSOE votedIU votedBasque votedCatalan votedOther votedNone  votedWhite  
local ideology ideology
local gender female
local age  age1834 age3564 age6599
local education ed_none ed_secondary ed_college 
local employment emp_working emp_retired emp_unemployed emp_student emp_housewife
local sizetown  munsize_1 munsize_2 munsize_3 munsize_4 munsize_5 munsize_6 munsize_7



************** Regressions ******************************************************************************
************** Regressions ******************************************************************************
************** Regressions ******************************************************************************

*** Regressions of main effects on participation and incumbent support
foreach v in  incumbent participated  switch  {
	foreach d in 1 3 5 {
		* Without controls
		areg `v'  post   if abs(tpost) <=`d'  , cluster(prov_by_munsize ) a(attack_by_prov)
		estimates store `v'_`d'd_nc
		gen double r2_`v'_`d'd_nc = `e(r2_a)'
		gen double beta_`v'_`d'd_nc = _b[post]
		format r2_`v'_`d'd_nc beta_`v'_`d'd_nc %5.3f
		tostring r2_`v'_`d'd_nc beta_`v'_`d'd_nc, replace u force
		gen lab_b_r2_`v'_`d'd_nc = beta_`v'_`d'd_nc  + " (R-Sq="+ r2_`v'_`d'd_nc+")"

		* With all controls
		areg `v'  post `voteprior' `gender' `age' `education'  `employment'  `sizetown'  if abs(tpost) <=`d'  , cluster(prov_by_munsize ) a(attack_by_prov)
		estimates store `v'_`d'd
		gen double r2_`v'_`d'd = `e(r2_a)'
		gen double beta_`v'_`d'd = _b[post]
		format r2_`v'_`d'd beta_`v'_`d'd %5.3f
		tostring r2_`v'_`d'd beta_`v'_`d'd, replace u force
		gen lab_b_r2_`v'_`d'd = beta_`v'_`d'd  + " (R-Sq="+ r2_`v'_`d'd+")"

	}
}

*** Regressions of effect heterogeneity 
gen post_all = post
gen post_basque = post if ccaa_name =="Pais-Vasco"
gen post_absent = post if votedNone ==1
gen post_PP = post if votedPP ==1
gen post_PSOE = post if votedPSOE ==1
gen post_civpol = post if type_victim == "civpol"
gen post_polmil = post if type_victim == "polmil"
gen post_samep = post if same_prov ==1
gen post_diffp = post if same_prov ==0


foreach v in  incumbent participated   {
	foreach p in all basque absent PP PSOE civpol polmil  samep diffp {
		foreach d in 1 3 5 {
			areg `v'  post_`p' `voteprior' `gender' `age' `education' `employment'  `sizetown'  if abs(tpost) <=`d'  , cluster(prov_by_munsize ) a(attack_by_prov)
			estimates store `v'_`d'd_`p'
			gen double r2_`v'_`d'd_`p' = `e(r2_a)'
			gen double beta_`v'_`d'd_`p' = _b[post]
			format r2_`v'_`d'd_`p' beta_`v'_`d'd_`p' %5.3f
			tostring r2_`v'_`d'd_`p' beta_`v'_`d'd_`p', replace u force
			gen b_r2_`v'_`d'd_`p' = beta_`v'_`d'd_`p'  + " (R-Sq="+ r2_`v'_`d'd_`p'+")"

			areg `v'  post_`p'  if abs(tpost) <=`d'  , cluster(prov_by_munsize ) a(attack_by_prov)
			estimates store `v'_`d'd_`p'_nc
			gen double r2_`v'_`d'd_`p'_nc = `e(r2_a)'
			gen double beta_`v'_`d'd_`p'_nc = _b[post]
			format r2_`v'_`d'd_`p'_nc beta_`v'_`d'd_`p'_nc %5.3f
			tostring r2_`v'_`d'd_`p'_nc beta_`v'_`d'd_`p'_nc, replace u force
			gen b_r2_`v'_`d'd_`p'_nc = beta_`v'_`d'd_`p'_nc  + " (R-Sq="+ r2_`v'_`d'd_`p'_nc+")"
		}
	}
}


************** Main Article Figures ******************************************************************************
************** Main Article Figures ******************************************************************************
************** Main Article Figures ******************************************************************************
lab var post " "

*** Figure 1: Main Effects on Particpation
coefplot ///
	(participated_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(participated_5d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_participated_5d_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_5d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_participated_5d) mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(participated_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(participated_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(participated_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(participated_3d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_participated_3d_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_3d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_participated_3d) mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_1d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_participated_1d_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_1d , keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red )  ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_participated_1d) mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_1d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_1d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ), ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	format(%9.2f) ///
	xtitle("Change in Probability of Participation", size(large)) ///
	ytitle(	" 1 day           3 days          5 days   "     "    (N=3,810)   (N=9,135)   (N=11,848)     "  , size(large)) ///
	ylabel(, notick labsize(large)) ///
	xlabel(-.08(.04).08, labsize(large) )  ///
	legend(size(large ) order(4 "Without controls" 6 "With controls") rows(1) region(lcolor(none))) 
graph export figure1.pdf,replace



*** Figure 2: Heterogenity in Participation Effects (within 1 day)
foreach d in 1  {

sum post_all  if abs(tpost) <=`d' 
lab var post_all "(1) All (N=`r(N)')"

sum post_basque  if abs(tpost) <=`d' 
lab var post_basque "(2) Basque Country (N=`r(N)')"

sum post_absent  if abs(tpost) <=`d' 
lab var post_absent "(3) Previous Absentees (N=`r(N)')"

sum post_PP  if abs(tpost) <=`d' 
lab var post_PP "(4) PP Voters (N=`r(N)')"

sum post_PSOE  if abs(tpost) <=`d' 
lab var post_PSOE "(5) PSOE Voters (N=`r(N)')"

sum post_civpol  if abs(tpost) <=`d' 
lab var post_civpol "(6) Civilian-Politician (N=`r(N)')"

sum post_polmil  if abs(tpost) <=`d' 
lab var post_polmil "(7) Police-Military (N=`r(N)')"

sum post_samep  if abs(tpost) <=`d' 
lab var post_samep "(8) Same Province (N=`r(N)')"

sum post_diffp  if abs(tpost) <=`d' 
lab var post_diffp "(9) Different Province (N=`r(N)')"

coefplot ///
	(participated_`d'd_all, keep(post_all)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_all) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_all, keep(post_all)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_basque, keep(post_basque)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_basque) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_basque, keep(post_basque)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_absent, keep(post_absent)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_absent) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_absent, keep(post_absent)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_PP, keep(post_PP)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_PP) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_PP, keep(post_PP)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_PSOE, keep(post_PSOE)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_PSOE) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_PSOE, keep(post_PSOE)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_civpol, keep(post_civpol)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_civpol) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_civpol, keep(post_civpol)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_polmil, keep(post_polmil)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_polmil) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_polmil, keep(post_polmil)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_samep, keep(post_samep)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_samep) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_samep, keep(post_samep)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_diffp, keep(post_diffp)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_diffp) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_diffp, keep(post_diffp)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) , ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	offset(0) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	format(%9.2f) ///
	legend(size(large ) order(2 "With controls") rows(1) region(lcolor(none))) ///
	xtitle("Change Prob. Participation", size(large)) ///
	ylabel(, labsize(medlarge)) ///
	xlabel(-.2(.2).4, labsize(large) ) 
}
graph export figure2.pdf,replace



*** Figure 3: Main Effects on Incumbent Support
coefplot ///
	(incumbent_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1) mlabsize(4.2) mlabgap(*2) ) ///
	(incumbent_5d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_incumbent_5d_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_5d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_incumbent_5d) mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(incumbent_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(incumbent_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(incumbent_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(incumbent_3d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_incumbent_3d_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_3d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_incumbent_3d) mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_1d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_incumbent_1d_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_1d , keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red )  ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_incumbent_1d) mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_1d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_1d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ), ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	format(%9.2f) ///
	xtitle("Change in Probability of Incumbent Support", size(large)) ///
	ytitle(	" 1 day           3 days          5 days   "     "    (N=3,810)   (N=9,135)   (N=11,848)     "  , size(large)) ///
	ylabel(, notick labsize(large)) ///
	xlabel(-.08(.04).08, labsize(large) )  ///
	legend(size(large ) order(4 "Without controls" 6 "With controls") rows(1) region(lcolor(none))) 
graph export figure3.pdf,replace



*** Figure 5: Oster Selection test
local voteprior  votedPP votedPSOE votedIU votedBasque votedCatalan votedOther votedNone  votedWhite  
local ideology ideology
local gender female
local age  age1834 age3564 age6599
local education ed_none ed_secondary ed_college 
local employment emp_working emp_retired emp_unemployed emp_student emp_housewife
local sizetown  munsize_1 munsize_2 munsize_3 munsize_4 munsize_5 munsize_6 munsize_7

gen double n = _n/1000
gen double r2 = .
encode attack_by_prov,gen(attack_by_prov_num) 
xtset attack_by_prov_num
foreach d in 3 {
	gen double delta_par_beta000_d`d' = .
	gen double delta_inc_beta020_d`d' = .

	* Sensitivity in particiaption (true effect is zero)
	xtreg participated  post `voteprior' `gender' `age' `education' `employment'  `sizetown'  if abs(tpost) <=`d'  ,  fe

	foreach  r in  .35 .36 .37 .38 .39 .4 .41 .42 .43 .44 .45 .46 .47 .48 .49 .5 .51 .52 .53 .54 .55 .56 .57 .58 .59 .6 .61 .62 .63 .64 .65 .66 .67 .68 .69 .7  {
		psacalc delta post, beta(0) rmax(`r')
		replace delta_par_beta000_d`d' = `r(delta)' if n == `r'
		replace r2 = `r(rmax)' if n == `r'

	}

	* Sensitivity in incumbent (true effect is -.02)
	xtreg incumbent     post `voteprior' `gender' `age' `education' `employment'  `sizetown'  if abs(tpost) <=`d'  ,  fe
	foreach  r in  .35 .36 .37 .38 .39 .4 .41 .42 .43 .44 .45 .46 .47 .48 .49 .5 .51 .52 .53 .54 .55 .56 .57 .58 .59 .6 .61 .62 .63 .64 .65 .66 .67 .68 .69 .7  {

		psacalc delta post, beta(-.02) rmax(`r')
		replace delta_inc_beta020_d`d' = `r(delta)' if n == `r'

	}

}

twoway (line delta_inc_beta020_d3  r2,lpattern(dash) lcolor(black)) ///
|| (line delta_par_beta000_d3 r2,lpattern(solid) lcolor(black)) , ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	yline(1,lwidth(thin) lpattern(solid)) ///
	title("", size(medium)) ///
	xtitle("R-Squared Max." ,   size(large)) ///
	xscale(titlegap(*7)) ///
	l1title("Ratio Selection on Unobservables" "to Selection on Observables", size(large)) ///
	ylabel( 0 1 5(5)15, labsize(large) format(%9.0f)) ///
	xlabel(.35(.05).7, labsize(large) format(%9.2f)) ///
	legend(order(1 "True incumbent effect is -0.02" 2 "True participation effect is 0.00" ) size(large ) rows(2) region(lcolor(none))) 
graph export figure5.pdf,replace


************** SI Appendix Figures ******************************************************************************
************** SI Appendix Figures ******************************************************************************
************** SI Appendix Figures ******************************************************************************

*** Figure effect heterogenity participation 

foreach d in 1 3 5 {

sum post_all  if abs(tpost) <=`d' 
lab var post_all "(1) All (N=`r(N)')"

sum post_basque  if abs(tpost) <=`d' 
lab var post_basque "(2) Basque Country (N=`r(N)')"

sum post_absent  if abs(tpost) <=`d' 
lab var post_absent "(3) Previous Absentees (N=`r(N)')"

sum post_PP  if abs(tpost) <=`d' 
lab var post_PP "(4) PP Voters (N=`r(N)')"

sum post_PSOE  if abs(tpost) <=`d' 
lab var post_PSOE "(5) PSOE Voters (N=`r(N)')"

sum post_civpol  if abs(tpost) <=`d' 
lab var post_civpol "(6) Civilian-Politician (N=`r(N)')"

sum post_polmil  if abs(tpost) <=`d' 
lab var post_polmil "(7) Police-Military (N=`r(N)')"

sum post_samep  if abs(tpost) <=`d' 
lab var post_samep "(8) Same Province (N=`r(N)')"

sum post_diffp  if abs(tpost) <=`d' 
lab var post_diffp "(9) Different Province (N=`r(N)')"


coefplot ///
	(participated_`d'd_all, keep(post_all)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_all) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_all, keep(post_all)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_basque, keep(post_basque)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_basque) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_basque, keep(post_basque)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_absent, keep(post_absent)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_absent) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_absent, keep(post_absent)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_PP, keep(post_PP)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_PP) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_PP, keep(post_PP)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_PSOE, keep(post_PSOE)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_PSOE) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_PSOE, keep(post_PSOE)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_civpol, keep(post_civpol)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_civpol) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_civpol, keep(post_civpol)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_polmil, keep(post_polmil)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_polmil) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_polmil, keep(post_polmil)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_samep, keep(post_samep)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_samep) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_samep, keep(post_samep)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_diffp, keep(post_diffp)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_participated_`d'd_diffp) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_diffp, keep(post_diffp)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) , ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	offset(0) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	format(%9.2f) ///
	legend(size(large ) order(2 "With controls") rows(1) region(lcolor(none))) ///
	xtitle("Change Prob. Participation", size(large)) ///
	ylabel(, labsize(medlarge)) ///
	xlabel(-.2(.2).4, labsize(large) ) 
graph export participation-heterogenity-`d'd.pdf,replace

coefplot ///
	(participated_`d'd_all_nc, keep(post_all)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_participated_`d'd_all_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_all_nc, keep(post_all)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_basque_nc, keep(post_basque)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_participated_`d'd_basque_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_basque_nc, keep(post_basque)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_absent_nc, keep(post_absent)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_participated_`d'd_absent_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_absent_nc, keep(post_absent)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_PP_nc, keep(post_PP)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_participated_`d'd_PP_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_PP_nc, keep(post_PP)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_PSOE_nc, keep(post_PSOE)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_participated_`d'd_PSOE_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_PSOE_nc, keep(post_PSOE)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_civpol_nc, keep(post_civpol)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_participated_`d'd_civpol_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_civpol_nc, keep(post_civpol)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_polmil_nc, keep(post_polmil)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_participated_`d'd_polmil_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_polmil_nc, keep(post_polmil)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_samep_nc, keep(post_samep)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_participated_`d'd_samep_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_samep_nc, keep(post_samep)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_`d'd_diffp_nc, keep(post_diffp)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_participated_`d'd_diffp_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(participated_`d'd_diffp_nc, keep(post_diffp)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) , ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	offset(0) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	format(%9.2f) ///
	legend(size(large ) order(2 "Without controls") rows(1) region(lcolor(none))) ///
	xtitle("Change Prob. Participation", size(large)) ///
	ylabel(, labsize(medlarge)) ///
	xlabel(-.2(.2).4, labsize(large) ) 
graph export participation-heterogenity-`d'd-no-controls.pdf,replace

*** Figure effect heterogenity Incumbent 
coefplot ///
	(incumbent_`d'd_all, keep(post_all)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_incumbent_`d'd_all) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_all, keep(post_all)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_basque, keep(post_basque)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_incumbent_`d'd_basque) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_basque, keep(post_basque)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_absent, keep(post_absent)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_incumbent_`d'd_absent) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_absent, keep(post_absent)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_PP, keep(post_PP)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_incumbent_`d'd_PP) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_PP, keep(post_PP)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_PSOE, keep(post_PSOE)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_incumbent_`d'd_PSOE) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_PSOE, keep(post_PSOE)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_civpol, keep(post_civpol)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_incumbent_`d'd_civpol) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_civpol, keep(post_civpol)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_polmil, keep(post_polmil)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_incumbent_`d'd_polmil) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_polmil, keep(post_polmil)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_samep, keep(post_samep)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_incumbent_`d'd_samep) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_samep, keep(post_samep)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_diffp, keep(post_diffp)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(b_r2_incumbent_`d'd_diffp) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_diffp, keep(post_diffp)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) , ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	offset(0) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	format(%9.2f) ///
	legend(size(large ) order(2 "With controls") rows(1) region(lcolor(none))) ///
	xtitle("Change Incumbent Support", size(large)) ///
	ylabel(, labsize(medlarge)) ///
	xlabel(-.2(.2).4, labsize(large) ) 
graph export incumbent-heterogenity-`d'd.pdf,replace

coefplot ///
	(incumbent_`d'd_all_nc, keep(post_all)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_incumbent_`d'd_all_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_all_nc, keep(post_all)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_basque_nc, keep(post_basque)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_incumbent_`d'd_basque_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_basque_nc, keep(post_basque)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_absent_nc, keep(post_absent)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_incumbent_`d'd_absent_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_absent_nc, keep(post_absent)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_PP_nc, keep(post_PP)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_incumbent_`d'd_PP_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_PP_nc, keep(post_PP)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_PSOE_nc, keep(post_PSOE)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_incumbent_`d'd_PSOE_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_PSOE_nc, keep(post_PSOE)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_civpol_nc, keep(post_civpol)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_incumbent_`d'd_civpol_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_civpol_nc, keep(post_civpol)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_polmil_nc, keep(post_polmil)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_incumbent_`d'd_polmil_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_polmil_nc, keep(post_polmil)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_samep_nc, keep(post_samep)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_incumbent_`d'd_samep_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_samep_nc, keep(post_samep)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_`d'd_diffp_nc, keep(post_diffp)  lpatt(solid) lcol(blue) msym(square) msize(3) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(b_r2_incumbent_`d'd_diffp_nc) mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(incumbent_`d'd_diffp_nc, keep(post_diffp)  lpatt(solid) lcol(none) msym(square) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) , ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	offset(0) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	format(%9.2f) ///
	legend(size(large ) order(2 "Without controls") rows(1) region(lcolor(none))) ///
	xtitle("Change Incumbent Support", size(large)) ///
	ylabel(, labsize(medlarge)) ///
	xlabel(-.2(.2).4, labsize(large) ) 
graph export incumbent-heterogenity-`d'd-no-controls.pdf,replace
}



*** Robustness to controlling for unbalanced pre-treatment variables 
foreach v in  incumbent participated  {
	foreach d in 1 3 5 {
		* Controling for vote prior
		areg `v'  post `voteprior'   if abs(tpost) <=`d'  , cluster(prov_by_munsize ) a(attack_by_prov)
		estimates store `v'_`d'd_prior

		* Controling for education
		areg `v'  post `education'   if abs(tpost) <=`d'  , cluster(prov_by_munsize ) a(attack_by_prov)
		estimates store `v'_`d'd_educ

		* Controling for education and vote prior
		areg `v'  post `voteprior'  `education'   if abs(tpost) <=`d'  , cluster(prov_by_munsize ) a(attack_by_prov)
		estimates store `v'_`d'd_prior_educ
	}
}


coefplot ///
	(incumbent_5d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue ) ciopts(lpatt(solid) lcol(blue))    ) ///
	(incumbent_5d_prior, keep(post)  lpatt(solid) lcol(red) msym(triangle) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(incumbent_5d_educ, keep(post)  lpatt(solid) lcol(red) msym(diamond) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))  ) ///
	(incumbent_5d_prior_educ, keep(post)  lpatt(solid) lcol(red) msym(X) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(incumbent_5d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(incumbent_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(2) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_3d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue))   ) ///
	(incumbent_3d_prior, keep(post)  lpatt(solid) lcol(red) msym(triangle) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(incumbent_3d_educ, keep(post)  lpatt(solid) lcol(red) msym(diamond) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(incumbent_3d_prior_educ, keep(post)  lpatt(solid) lcol(red) msym(X) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(incumbent_3d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(incumbent_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(2) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(incumbent_1d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue))   ) ///
	(incumbent_1d_prior, keep(post)  lpatt(solid) lcol(red) msym(triangle) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))  ) ///
	(incumbent_1d_educ, keep(post)  lpatt(solid) lcol(red) msym(diamond) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))  ) ///
	(incumbent_1d_prior_educ, keep(post)  lpatt(solid) lcol(red) msym(X) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))  ) ///
	(incumbent_1d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(incumbent_1d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(2) mcol(none ) ciopts(lpatt(solid) lcol(none)) ), ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	vertical yline(0,lwidth(thin) lpattern(dash)) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	xtitle("5 Days                            3 Days                             1 Day         " , size(small)) ///
	l1title("Change in Probability of Incumbent Support", size(small)) ///
	ylabel(-.08(.02).08, labsize(small) format(%9.2f)) ///
	xlabel(, labsize(small) notick) ///
	format(%9.3f) ///
	legend(size(small ) order(2 "Without controls" 4 "Controling for vote in prior election" 6 "Controling for education" 8 "Controling for vote in prior election and education" 10 "All controls") rows(5) region(lcolor(none))) 
graph export incumbent-robustness.pdf,replace

coefplot ///
	(participated_5d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue ) ciopts(lpatt(solid) lcol(blue))    ) ///
	(participated_5d_prior, keep(post)  lpatt(solid) lcol(red) msym(triangle) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(participated_5d_educ, keep(post)  lpatt(solid) lcol(red) msym(diamond) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))  ) ///
	(participated_5d_prior_educ, keep(post)  lpatt(solid) lcol(red) msym(X) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(participated_5d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(participated_5d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(2) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_3d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue))   ) ///
	(participated_3d_prior, keep(post)  lpatt(solid) lcol(red) msym(triangle) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(participated_3d_educ, keep(post)  lpatt(solid) lcol(red) msym(diamond) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(participated_3d_prior_educ, keep(post)  lpatt(solid) lcol(red) msym(X) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(participated_3d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(participated_3d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(2) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(participated_1d_nc , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue))   ) ///
	(participated_1d_prior, keep(post)  lpatt(solid) lcol(red) msym(triangle) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))  ) ///
	(participated_1d_educ, keep(post)  lpatt(solid) lcol(red) msym(diamond) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))  ) ///
	(participated_1d_prior_educ, keep(post)  lpatt(solid) lcol(red) msym(X) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))  ) ///
	(participated_1d, keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red))   ) ///
	(participated_1d, keep(post)  lpatt(solid) lcol(none) msym(o) msize(2) mcol(none ) ciopts(lpatt(solid) lcol(none)) ), ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	vertical yline(0,lwidth(thin) lpattern(dash)) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	xtitle("5 Days                  3 Days                  1 Day   " , size(medlarge)) ///
	l1title("Change in Probability of Participating in Election", size(medlarge)) ///
	ylabel(-.08(.04).08, labsize(medium) format(%9.2f)) ///
	xlabel(, labsize(medlarge) notick) ///
	format(%9.3f) ///
	legend(size(medlarge ) order(2 "Without controls" 4 "Controling for vote in prior election" 6 "Controling for education" 8 "Controling for vote in prior election and education" 10 "All controls") rows(5) region(lcolor(none))) 
graph export participation-robustness.pdf,replace

*** Participation models using different clustering stratgeies
lab var post " "
gen pr = prov
gen att_pr = attack_by_prov
gen att = attack_num

foreach v in  participated    {
	foreach d in 1 3 5 {
		foreach c in pr att att_pr {
		* Without controls
		areg `v'  post   if abs(tpost) <=`d'  , cluster(`c') a(attack_by_prov_num)
		estimates store part_`d'd_nc_`c'
		gen double r2_part_`d'd_nc_`c' = `e(r2_a)'
		gen double beta_part_`d'd_nc_`c' = _b[post]
		format r2_part_`d'd_nc_`c' beta_part_`d'd_nc_`c' %5.3f
		tostring r2_part_`d'd_nc_`c' beta_part_`d'd_nc_`c', replace u force
		gen lab_b_r2_part_`d'd_nc_`c' = beta_part_`d'd_nc_`c'  + " (R-Sq="+ r2_part_`d'd_nc_`c'+")"

		* With all controls
		areg `v'  post `voteprior' `gender' `age' `education' `employment'  `sizetown'  if abs(tpost) <=`d'  , cluster(prov_by_munsize ) a(attack_by_prov_num)
		estimates store part_`d'd_`c'
		gen double r2_part_`d'd_`c' = `e(r2_a)'
		gen double beta_part_`d'd_`c' = _b[post]
		format r2_part_`d'd_`c' beta_part_`d'd_`c' %5.3f
		tostring r2_part_`d'd_`c' beta_part_`d'd_`c', replace u force
		gen lab_b_r2_part_`d'd_`c' = beta_part_`d'd_`c'  + " (R-Sq="+ r2_part_`d'd_`c'+")"

		}
	}
}

lab var post " "

foreach c in pr att att_pr {
coefplot ///
	(part_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(part_5d_nc_`c' , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_part_5d_nc_`c') mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(part_5d_`c', keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_part_5d_`c') mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(part_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(part_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(part_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(part_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(part_3d_nc_`c' , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_part_3d_nc_`c') mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(part_3d_`c', keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_part_3d_`c') mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(part_3d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(part_3d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(part_3d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(part_3d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(part_1d_nc_`c' , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_part_1d_nc_`c') mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(part_1d_`c' , keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red )  ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_part_1d_`c') mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(part_1d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(part_1d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ), ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	format(%9.2f) ///
	xtitle("Change in Probability of Participation", size(large)) ///
	ytitle(	" 1 day           3 days          5 days   "     "    (N=3,810)   (N=9,135)   (N=11,848)     "  , size(large)) ///
	ylabel(, notick labsize(large)) ///
	xlabel(-.08(.04).08, labsize(large) )  ///
	legend(size(large ) order(4 "Without controls" 6 "With controls") rows(1) region(lcolor(none))) 
graph export participation-main-cluster-`c'.pdf,replace
}


*** Incumbent models using different clustering stratgeies
foreach v in  incumbent    {
	foreach d in 1 3 5 {
		foreach c in pr att att_pr {
		
		*** Main models ***
		******************************	
		* Without controls
		areg `v'  post   if abs(tpost) <=`d'  , cluster(`c') a(attack_by_prov_num)
		estimates store inc_`d'd_nc_`c'
		gen double r2_inc_`d'd_nc_`c' = `e(r2_a)'
		gen double beta_inc_`d'd_nc_`c' = _b[post]
		format r2_inc_`d'd_nc_`c' beta_inc_`d'd_nc_`c' %5.3f
		tostring r2_inc_`d'd_nc_`c' beta_inc_`d'd_nc_`c', replace u force
		gen lab_b_r2_inc_`d'd_nc_`c' = beta_inc_`d'd_nc_`c'  + " (R-Sq="+ r2_inc_`d'd_nc_`c'+")"

		* With all controls
		areg `v'  post `voteprior' `gender' `age' `education' `employment'  `sizetown'  if abs(tpost) <=`d'  , cluster(prov_by_munsize ) a(attack_by_prov_num)
		estimates store inc_`d'd_`c'
		gen double r2_inc_`d'd_`c' = `e(r2_a)'
		gen double beta_inc_`d'd_`c' = _b[post]
		format r2_inc_`d'd_`c' beta_inc_`d'd_`c' %5.3f
		tostring r2_inc_`d'd_`c' beta_inc_`d'd_`c', replace u force
		gen lab_b_r2_inc_`d'd_`c' = beta_inc_`d'd_`c'  + " (R-Sq="+ r2_inc_`d'd_`c'+")"

		}
	}
}

foreach c in pr att att_pr {
coefplot ///
	(inc_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(inc_5d_nc_`c' , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue ) ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_inc_5d_nc_`c') mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(inc_5d_`c', keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_inc_5d_`c') mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(inc_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(inc_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(inc_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(inc_5d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none))  mlabposition(1)mlabsize(4.2) mlabgap(*2) ) ///
	(inc_3d_nc_`c' , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_inc_3d_nc_`c') mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(inc_3d_`c', keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red ) ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_inc_3d_`c') mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(inc_3d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(inc_3d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(inc_3d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(inc_3d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(inc_1d_nc_`c' , keep(post)  lpatt(solid) lcol(blue) msym(square) msize(2) mcol(blue )  ciopts(lpatt(solid) lcol(blue)) mlabel(lab_b_r2_inc_1d_nc_`c') mlabposition(12)mlabsize(4.2) mlabgap(*2)  ) ///
	(inc_1d_`c' , keep(post)  lpatt(solid) lcol(red) msym(o) msize(3) mcol(red )  ciopts(lpatt(solid) lcol(red)) mlabel(lab_b_r2_inc_1d_`c') mlabposition(6)mlabsize(4.2) mlabgap(*2)  ) ///
	(inc_1d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ) ///
	(inc_1d_`c', keep(post)  lpatt(solid) lcol(none) msym(o) msize(3) mcol(none ) ciopts(lpatt(solid) lcol(none)) ), ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	levels(95) ///
	grid(none) ///
	title("", size(medium)) ///
	format(%9.2f) ///
	xtitle("Change in Probability of Participation", size(large)) ///
	ytitle(	" 1 day           3 days          5 days   "     "    (N=3,810)   (N=9,135)   (N=11,848)     "  , size(large)) ///
	ylabel(, notick labsize(large)) ///
	xlabel(-.08(.04).08, labsize(large) )  ///
	legend(size(large ) order(4 "Without controls" 6 "With controls") rows(1) region(lcolor(none))) 
graph export incumbent-main-cluster-`c'.pdf,replace
}


*** Dropping attacks one by one
levelsof attack_num,local(attacks)
foreach a of local attacks {
gen post_a`a' = post
lab var post_a`a' "Excluding Attack #`a'"
areg participated  post_a`a' `voteprior' `gender' `age' `education' `employment'  `sizetown'    if abs(tpost) <=5  & attack_num !=`a',  cluster(prov_by_munsize) a(attack_by_prov)
estimates store participated_5d_drop`a'
}

lab var post "Including All 8 Attacks"
coefplot ///
	(participated_5d, keep(post)   lpatt(solid) lcol(red) msym(o) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red))    ) ///
	(participated_5d_drop1,  keep(post_a1)  lpatt(solid) lcol(red) msym(o) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(participated_5d_drop2, keep(post_a2)  lpatt(solid) lcol(red) msym(o) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(participated_5d_drop3, keep(post_a3)  lpatt(solid) lcol(red) msym(o) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(participated_5d_drop4, keep(post_a4)  lpatt(solid) lcol(red) msym(o) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(participated_5d_drop5, keep(post_a5)  lpatt(solid) lcol(red) msym(o) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(participated_5d_drop6, keep(post_a6)  lpatt(solid) lcol(red) msym(o) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(participated_5d_drop7, keep(post_a7)  lpatt(solid) lcol(red) msym(o) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) ///
	(participated_5d_drop8, keep(post_a8)  lpatt(solid) lcol(red) msym(o) msize(2) mcol(red ) ciopts(lpatt(solid) lcol(red)) ) , ///
	scheme(s1mono)  plotregion(lcolor(none))  ///
	horizontal xline(0,lwidth(thin) lpattern(dash)) ///
	grid(none) ///
	offset(0) ///
	levels(95) ///
	title("", size(medium)) ///
	xtitle("Change in Probability of Participating in Election" ,   size(medlarge)) ///
	ylabel(, labsize(medlarge) notick ) ///
	xlabel(-.08(.04).08, labsize(medlarge) format(%9.2f)) ///
	format(%9.3f) ///
	mlabel mlabposition(12)mlabsize(4.2) mlabgap(*1) ///
	legend(size(medlarge ) order(2 "With controls") rows(1) region(lcolor(none))) 
graph export participation-dropping-attacks.pdf,replace


