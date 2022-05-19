///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////////// This do-file conducts the main                		///////
////////// empirical analyses.                           		///////
//////////                                               		///////
////////// Table of contents:                            		///////
////////// ------------------                            		///////
//////////                                               		///////
////////// 1. Descriptive statistics (Table 3)           		///////
////////// 2. Analyses                                   		///////
////////// 	2.1 Baseline results (Table 4)               		///////
////////// 	2.2 Full spillover model (Table 5)           		///////
//////////	2.3 Plot effect by treatment fraction (Figure 2)	///////
////////// 	2.4 Tradeable vs non-tradeable (Table 6)     		///////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////

* set path (insert your path here; required for the code to run)
global 		path  "../Replication Package"

* additional functions required to run the code:
* (commented out, only need to be installed once)
*ssc 		install reghdfe
*ssc 		install estout

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////// *** 1. Descriptive statistics *** ///////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

* load master dataset (see do-file "01 Master Dataset.do")
use			"$path/Data/_Processed/Master dataset.dta", clear

* Table 3: Summary statistics
eststo		clear
eststo: 	qui: estpost summarize cb_dep cb_dep_d_50 cb_dep_ic bnk_num empl_07 age_07, d

#d ;
esttab, cells("mean(fmt(%9.2fc) label(Mean)) sd(fmt(%9.2fc) label(SD))
	p5(fmt(%9.2fc) label(p5)) p50(fmt(%9.2fc) label(p50))
	p95(fmt(%9.2fc) label(p95)) count(fmt(%9.0fc) label(N))")
	r nomtitle nonumber b(%9.3fc) noobs;
#d cr

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////// *** 2. Analyses *** /////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
//////// *** 2.1 Baseline results *** //////////////////////////
////////////////////////////////////////////////////////////////

* load master dataset (see do-file "01 Master Dataset.do")
use			"$path/Data/_Processed/Master dataset.dta", clear

* Table 4: Baseline results
eststo m1_1: qui: reg			empl_g cb_dep						 , 								cluster(region_fe)
estadd local age "\multicolumn{1}{c}{No}"
estadd local ind "\multicolumn{1}{c}{No}"
estadd local siz "\multicolumn{1}{c}{No}"
eststo m1_2: qui: reghdfe		empl_g cb_dep 			ln_age_07	 , absorb(ind_fe size_bin_07) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m1_3: qui: reghdfe		empl_g cb_dep cb_dep_ic ln_age_07	 , absorb(ind_fe size_bin_07) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"

esttab m1_1 m1_2 m1_3, nogap r se keep(cb_dep*) star(* 0.10 ** 0.05 *** 0.01) ///
stats(ind siz age N, layout("@" "@" "@" "@") fmt(%#s %#s %#s %9.0fc) ///
labels("Industry fixed effects" "Size bin fixed effects" "ln age" "Observations")) ///
nomtitle nodiscrete b(%9.3f)

////////////////////////////////////////////////////////////////
////////// *** 2.2 Full spillover model  *** ///////////////////
////////////////////////////////////////////////////////////////

* load master dataset (see do-file "01 Master Dataset.do")
use			"$path/Data/_Processed/Master dataset.dta", clear

* Table 5: Full spillover model
eststo m2_1: qui: reghdfe		empl_g cb_dep			 							ln_age_07 , absorb(ind_fe size_bin_07) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m2_2: qui: reghdfe		empl_g cb_dep cb_dep_ic 							ln_age_07 , absorb(ind_fe size_bin_07) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m2_3: qui: reghdfe		empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic	ln_age_07 , absorb(ind_fe size_bin_07) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
		
eststo m2_4: qui: reghdfe		empl_g cb_dep_d_50 				 							ln_age_07 , absorb(ind_fe size_bin_07) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m2_5: qui: reghdfe		empl_g cb_dep_d_50 cb_dep_d_50_ic 							ln_age_07 , absorb(ind_fe size_bin_07) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m2_6: qui: reghdfe		empl_g cb_dep_d_50 cb_dep_d_50Xcb_dep_d_50_ic inv_cb_dep_d_50Xcb_dep_d_50_ic	ln_age_07 , absorb(ind_fe size_bin_07) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"

esttab m2_1 m2_2 m2_3 m2_4 m2_5 m2_6, nogap r se keep(*cb_dep*) star(* 0.10 ** 0.05 *** 0.01) ///
stats(ind siz age N, layout("@" "@" "@" "@") fmt(%#s %#s %#s %9.0fc) ///
labels("Industry fixed effects" "Size bin fixed effects" "ln age" "Observations")) ///
nomtitle nodiscrete b(%9.3f)

////////////////////////////////////////////////////////////////
/////// *** 2.3 Plot effect by treatment fraction  *** /////////
////////////////////////////////////////////////////////////////

* load master dataset (see do-file "01 Master Dataset.do")
use			"$path/Data/_Processed/Master dataset.dta", clear

* save regression coefficients and average firm age
qui:		reghdfe		empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic	ln_age_07 , absorb(ind_fe size_bin_07) 	cluster(region_fe)
gen			beta_1 = _b[cb_dep]
gen			beta_T = _b[cb_depXcb_dep_ic]
gen			beta_C = _b[inv_cb_depXcb_dep_ic]
		
sum			ln_age_07
gen			age_mean = r(mean)
		
tabstat		cb_dep_ic, stats(p1 p99) // ~ranges from 0.03 - 0.3
		
keep		beta* age_mean
duplicates	drop
	
* predict standard errors
expand		2
gen			cb_dep = _n -1
expand		101
bys			cb_dep: gen cb_dep_ic = (_n-1)/(_N-1)
gen			cb_depXcb_dep_ic 		= cb_dep_ic*cb_dep
gen			inv_cb_depXcb_dep_ic 	= cb_dep_ic*(1-cb_dep)
ren			age_mean ln_age_07
predict		yhat_se, stdp
		
* effect on the control group (cf. eq. (11) in paper)
gen			E_yc = beta_C*cb_dep_ic
gen			E_yc_up		= E_yc + 1.645*yhat_se
gen			E_yc_down	= E_yc - 1.645*yhat_se
		
* effect on the treated group (cf. eq. (12) in paper)
gen			E_yt = beta_1 + beta_T*cb_dep_ic
gen			E_yt_up		= E_yt + 1.645*yhat_se
gen			E_yt_down	= E_yt - 1.645*yhat_se
		
* average (cf. eq. (13) in paper)
gen			E_y = (beta_1+beta_C)*cb_dep_ic + (beta_T-beta_C)*(cb_dep_ic^2)
		
* restrict to ~area where data is available
keep		if cb_dep_ic <= 0.51
		
* Figure 2: Commerzbank’s lending cut and spillover effects at the county level
twoway		(rarea E_yc_down E_yc_up cb_dep_ic if cb_dep == 0, color(black%10) lwidth(vthin))					///
			(rarea E_yt_down E_yt_up cb_dep_ic if cb_dep == 1, color(black%30)  lwidth(vthin)) 					///
			(line E_yt cb_dep_ic if cb_dep == 1, lcolor(black)	lwidth(medthick) lpatter(longdash))				///
			(line E_y  cb_dep_ic if cb_dep == 1, lcolor(black) 	lwidth(medthick)	lpatter(dash))				///
			(line E_yc cb_dep_ic if cb_dep == 0, lcolor(black) 	lwidth(medthick)	),							///
			graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))								///
			ylabel(-0.15(0.05)0.02, labsize(small) angle(horizontal) nogrid)									///
			xlabel(0.(0.1)0.5, labsize(small))																	///
			ytitle("employment growth (y)", height(-4)) xtitle("treatment fraction (CB dep{subscript:c})") 		///
			legend(order(3 5 4) pos(8) ring(0) col(1) label(3 "E[y{subscript:T} | CB dep{subscript:c}]") 		///
			label(5 "E[y{subscript:C} | CB dep{subscript:c}]") label(4 "E[y avg. | CB dep{subscript:c}]")) 
	
////////////////////////////////////////////////////////////////
///// *** 2.4 Tradeable vs non-tradeable (Table 6)  *** ////////
////////////////////////////////////////////////////////////////

* load master dataset (see do-file "01 Master Dataset.do")
use			"$path/Data/_Processed/Master dataset.dta", clear

* defined tradeable / non-tradeable sectors based on NACE codes
* based on Bertinelli et al (2016), Appendix A.1
	
	* tradeable sectors
	*******************
		
		*00-03: AGRICULTURE, FORESTRY AND FISHING
		*05-09: MININING AND QUARRYING
		*10-33: MANUFACTURING
		*49-53: TRANSPORTATION AND STORAGE
		*58-63: INFORMATION AND COMMUNICATION
		
	* non-tradeable sectors
	***********************
		
		*35:	ELECTRICITY, GAS, STEAM AND AIR CONDITIONING SUPPLY
		*36-39:	WATER SUPPLY; SEWERAGE, WASTE MANAGEMENTAND REMEDIATION ACTIVITIES
		*41-43:	CONSTRUCTION
		*45-47: WHOLESALE AND RETAIL TRADE; REPAIR OFMOTOR VEHICLES AND MOTORCYCLES
		*55-56:	ACCOMMODATION AND FOOD SERVICE ACTIVITIES
		*90-93:	ARTS, ENTERTAINMENT AND RECREATION
		*94-96:	OTHER SERVICE ACTIVITIES
		*97-98:	ACTIVITIES OF HOUSEHOLDS AS EMPLOYERS
			
	* not-classified (2,013 out of 23,436 obs)
	******************************************
		
		*69-75: PROFESSIONAL, SCIENTIFIC AND TECHNICAL ACTIVITIES
		*77-82: ADMINISTRATIVE AND SUPPORT SERVICE
	
gen			nt = . // non-tradeable indicator
	
replace		nt = 0 if inrange(floor(nace_prim_code/100),0,3)
replace		nt = 0 if inrange(floor(nace_prim_code/100),5,9)
replace		nt = 0 if inrange(floor(nace_prim_code/100),10,33)
replace		nt = 0 if inrange(floor(nace_prim_code/100),49,53)
replace		nt = 0 if inrange(floor(nace_prim_code/100),58,63)
	
replace		nt = 1 if inrange(floor(nace_prim_code/100),35,35)
replace		nt = 1 if inrange(floor(nace_prim_code/100),36,39)
replace		nt = 1 if inrange(floor(nace_prim_code/100),41,43)
replace		nt = 1 if inrange(floor(nace_prim_code/100),45,47)
replace		nt = 1 if inrange(floor(nace_prim_code/100),55,56)
replace		nt = 1 if inrange(floor(nace_prim_code/100),90,98)

* Table 6: Tradeable vs non-tradeable sectors
eststo m3_1: qui: reghdfe		empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic	ln_age_07 	if nt != .	, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m3_2: qui: reghdfe		empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic	ln_age_07  	if nt == 1	, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m3_3: qui: reghdfe		empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic	ln_age_07  	if nt == 0	, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"

eststo m3_4: qui: reghdfe		empl_g cb_dep_d_50 cb_dep_d_50Xcb_dep_d_50_ic inv_cb_dep_d_50Xcb_dep_d_50_ic	ln_age_07 	if nt != .	, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m3_5: qui: reghdfe		empl_g cb_dep_d_50 cb_dep_d_50Xcb_dep_d_50_ic inv_cb_dep_d_50Xcb_dep_d_50_ic	ln_age_07  	if nt == 1	, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m3_6: qui: reghdfe		empl_g cb_dep_d_50 cb_dep_d_50Xcb_dep_d_50_ic inv_cb_dep_d_50Xcb_dep_d_50_ic	ln_age_07  	if nt == 0	, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"

esttab m3_1 m3_2 m3_3 m3_4 m3_5 m3_6, nogap r se keep(*cb_dep*) star(* 0.10 ** 0.05 *** 0.01) ///
stats(ind siz age N, layout("@" "@" "@" "@") fmt(%#s %#s %#s %9.0fc) ///
labels("Industry fixed effects" "Size bin fixed effects" "ln age" "Observations")) ///
nomtitle nodiscrete b(%9.3f) 
					
	* difference in coefficients
	****************************
	
	* rescale industry FEs within tradeable and non-tradeable subsample
	* (to avoid "factor variable base category conflict" in suest command)
	gen		ind_fe_nt = ind_fe if nt == 1
	gen		ind_fe_t  = ind_fe if nt == 0
	bys		nt: egen x = min(ind_fe)
	replace	ind_fe_nt = ind_fe_nt - x
	replace	ind_fe_t  = ind_fe_t - x
	drop	x
	
	qui: reg	empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic	ln_age_07 i.ind_fe_nt i.size_bin_07 	if nt == 1
	est		store first
	qui: reg	empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic	ln_age_07 i.ind_fe_t  i.size_bin_07 	if nt == 0
	est		store last
	qui: 	suest last first, vce(cluster region_fe)
	lincom	_b[last_mean:inv_cb_depXcb_dep_ic]-_b[first_mean:inv_cb_depXcb_dep_ic]
	* p-value: 0.18
	
	qui: reg	empl_g cb_dep_d_50 cb_dep_d_50Xcb_dep_d_50_ic inv_cb_dep_d_50Xcb_dep_d_50_ic	ln_age_07  i.ind_fe_nt i.size_bin_07	if nt == 1
	est		store first
	qui: reg	empl_g cb_dep_d_50 cb_dep_d_50Xcb_dep_d_50_ic inv_cb_dep_d_50Xcb_dep_d_50_ic	ln_age_07  i.ind_fe_t  i.size_bin_07	if nt == 0
	est		store last
	qui: 	suest last first, vce(cluster region_fe)
	lincom	_b[last_mean:inv_cb_dep_d_50Xcb_dep_d_50_ic]-_b[first_mean:inv_cb_dep_d_50Xcb_dep_d_50_ic]
	* p-value: 0.11

////////////////////////////////////////////////////////////////
///// *** 2.5 Effect by cash holdings (untabulated)  *** ///////
////////////////////////////////////////////////////////////////
	
* load master dataset (see do-file "01 Master Dataset.do")
use			"$path/Data/_Processed/Master dataset.dta", clear

* cash/assets median split
gen			cash_at = cash_07/toas_07
xtile		xcash = cash_at, n(2)

eststo m4_1: qui: reghdfe		empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic 								ln_age_07 if xcash != ., absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m4_2: qui: reghdfe		empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic 								ln_age_07 if xcash == 1, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m4_3: qui: reghdfe		empl_g cb_dep cb_depXcb_dep_ic inv_cb_depXcb_dep_ic 								ln_age_07 if xcash == 2, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m4_4: qui: reghdfe		empl_g cb_dep_d_50 cb_dep_d_50Xcb_dep_d_50_ic inv_cb_dep_d_50Xcb_dep_d_50_ic 		ln_age_07 if xcash != ., absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m4_5: qui: reghdfe		empl_g cb_dep_d_50 cb_dep_d_50Xcb_dep_d_50_ic inv_cb_dep_d_50Xcb_dep_d_50_ic 		ln_age_07 if xcash == 1, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"
eststo m4_6: qui: reghdfe		empl_g cb_dep_d_50 cb_dep_d_50Xcb_dep_d_50_ic inv_cb_dep_d_50Xcb_dep_d_50_ic 		ln_age_07 if xcash == 2, absorb(size_bin_07 ind_fe) 	cluster(region_fe)
estadd local age "\multicolumn{1}{c}{Yes}"
estadd local ind "\multicolumn{1}{c}{Yes}"
estadd local siz "\multicolumn{1}{c}{Yes}"

esttab m4_1 m4_2 m4_3 m4_4 m4_5 m4_6, nogap r se keep(*cb_dep*) star(* 0.10 ** 0.05 *** 0.01) ///
stats(ind siz age N, layout("@" "@" "@" "@") fmt(%#s %#s %#s %9.0fc) ///
labels("Industry fixed effects" "Size bin fixed effects" "ln age" "Observations")) ///
nomtitle nodiscrete b(%9.3f) 

capture log close
* eof
