******************************************************************************************************************
***STATA DO-FILES FOR "DISENTANGLING THE EFFECTS OF A BANKING CRISIS: EVIDENCE FROM GERMAN FIRMS AND COUNTIES"****
******************************************************************************************************************

/*
The execution of the commands below requires the installation of a number of additional ado files,
such as ivreg2, xtivreg2, and coefplot. All additional files can be downloaded by using the command "findit", 
for example by typing "findit xtivreg2" into Stata.
The do-file was created using Stata 13.
*/



*************************************
***Firm panel variable definitions***
*************************************
use firm_panel, clear
label var cb_dep "Commerzbank dep"
label var cb_rel "Dummy for Commerzbank dep>0"
label var total_banks "No of bank relationships"
label var n "Employment"
label var wage "Average wage"
label var rcapital "Capital"
label var rdt "Liabilities"
label var exportquote "Export share"
label var importquote "Import share"
label var age "Age"
label var frbankdt_dt "Bank debt/debt"
label var frdt_sum "Liabilities/assets"
label var lrdt_bank "ln bank debt"
label var ln "ln employment"
label var lrcapital "ln capital"
label var lvaladd "ln value added"
label var lwage "ln wage"
label var irate "Interest paid over total liabilities"
label var d "Dummy for the years 2009-12"
label var cb_dep_09_12 "Firm CB dep*d"
label var importquote_09_12 "Import*d"
label var exportquote_09_12 "Export*d"
label var lage_09_12 "ln age*d"
label var county_cb_dep_09_12 "County CB dep*d"
label var cb_dep_09_12_fdt_bk_1 "Low bank debt dep*Firm CB dep*d"
label var cb_dep_09_12_fdt_bk_2 "High bank debt dep*Firm CB dep*d"
label var d_cb_cat_1 "CB dep = 0"
label var d_cb_cat_2 "Low firm CB dep*d"
label var d_cb_cat_3 "Medium firm CB dep*d"
label var d_cb_cat_4 "High firm CB dep*d"
label var size_c1 "4 firm size bins"
save firm_panel, replace



***************************************
***County panel variable definitions***
***************************************
use county_panel, clear
label var county_cb_dep_09_12 "County CB dep*d"
label var rgdp2000 "Year 2000 GDP"
label var pop2000 "Year 2000 population"
label var empl2000 "Year 2000 employment"
label var east "Dummy for former GDR"
label var lb "Dummy for Landesbank in crisis"
label var distance_instr "Distance instrument"
label var distance_hh "Distance to Hamburg"
label var distance_ffm "Distance to Frankfurt"
label var distance_dd "Distance to Duesseldorf"
label var distance_dr "Distance to Dresden"
label var distance_be "Distance to Berlin"
label var dlrgdp "Annual GDP Growth"
label var lrgdp_08_12 "GDP Growth 2008-12"
label var lrgdp_07_12 "GDP Growth 2007-12"
label var lempl_08_12 "Employment Growth 2008-12"
label var netmigr "Net migration rate"
label var lrgdp "Ln GDP"
label var lempl "Ln employment"
label var county_pop_density2000 "Population density 2000"
label var lcounty_gdppc2000 "Ln GDP per capita 2000"
label var lcounty_pop_tot2000_all "Ln population 2000"
label var hh_debt_index "Household debt index"
label var county_exportquote "Average export share"
label var county_importquote "Average import share"
label var q_ind_prod "Quantiles of the industrial production share"
label var d "Dummy for the years 2009-12"
save county_panel, replace



**************************************
***Patent data variable definitions***
**************************************
use firm_patents, clear
label variable gs_patent "Growth rate of patents"
label variable patents_d "Number of patents post lending cut"
label variable patents_d "Number of patents pre lending cut"
label variable cb_dep_no_patent_old "Non-patenting*Firm CB dep"
label variable cb_dep_patent_old "Patenting*Firm CB dep"
label variable lpatents_old "Ln Patents 1990-2004" 
save firm_patents, replace



*******************************************
******************Table I******************
***Summary statistics for the firm panel***
*******************************************
use firm_panel, clear

estpost summarize cb_dep total_banks n wage rcapital rdt exportquote importquote age frbankdt_dt frdt_sum if year==2006, listwise detail



***********************************************
********************Table II*******************
***Summary statistics for the county dataset***
***********************************************
use county_panel, clear

estpost summarize county_cb_dep rgdp2000 pop2000 empl2000 east lb distance_instr lrgdp_08_12 lempl_08_12 if year==2000, listwise detail



******************************************************
***********************Table III**********************
***Firm survey on banks’ willingness to grant loans***
******************************************************
use firm_survey, clear

foreach year in 2007 2008 2009 2010 2011 2012 {
reg bank_loan_supply_`year' cb_dep bank_loan_supply_2006 i.industry i.state i.size_c1 lage, cluster(county_fe)
}



************************************************
********************Table IV********************
***Firm bank loans and Commerzbank dependence***
************************************************
use firm_panel, clear

xtivreg2 lrdt_bank d_year_* ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 lrdt_bank d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 lrdt_bank d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)



******************************************************
************************Table V***********************
***Household debt and county Commerzbank dependence***
******************************************************
use firm_survey, clear

reg dsdebt0712 county_cb_dep ldebtallhouse02 ldebtoth02 debt02_d, cluster(county_fe)

reg dsdebt0712 county_cb_dep ldebtallhouse02 ldebtoth02 debt02_d ///
county_fract_ind* county_pop_density2000 lcounty_gdppc2000 lcounty_pop_tot2000_all hh_debt_index, cluster(county_fe)

reg dsdebt0712 county_cb_dep ldebtallhouse02 ldebtoth02 debt02_d ///
i.sex i.east i.hhchildcat i.hhadultcat i.hhedcat i.empstatus i.hhempstatus i.q_age i.q_hhincome ///
county_fract_ind* county_pop_density2000 lcounty_gdppc2000 lcounty_pop_tot2000_all hh_debt_index, cluster(county_fe)

foreach outcome in ///
debt08_d debt09_d debt10_d debt11_d debt12_d {
reg `outcome' county_cb_dep ldebtallhouse02 ldebtoth02 debt02_d ///
i.sex i.east i.hhchildcat i.hhadultcat i.hhedcat i.empstatus i.hhempstatus i.q_age i.q_hhincome ///
county_fract_ind* county_pop_density2000 lcounty_gdppc2000 lcounty_pop_tot2000_all hh_debt_index, cluster(county_fe)
}



************************************************
********************Table VI********************
***Firm employment and Commerzbank dependence***
************************************************
use firm_panel, clear

xtivreg2 ln d_year_* ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 ln d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 ln d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 ln d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
cb_dep_09_12_fdt_bk_1 cb_dep_09_12_fdt_bk_2, fe cluster(firm_county ind3)

xtivreg2 ln d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
d_cb_cat_2 d_cb_cat_3 d_cb_cat_4, fe cluster(firm_county ind3)



******************************************************
***********************Table VII**********************
***Further firm outcomes and Commerzbank dependence***
******************************************************
use firm_panel, clear

xtivreg2 lrcapital d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 lvaladd d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 lvaladd_k d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 lvaladd_n d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 lwage d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)

xtivreg2 irate d_year_* ///
dummy_firm_county_*_09_12 lage_09_12 dummy_size_c1_*_09_12 ///
dummy_ind3_*_09_12 importquote_09_12 exportquote_09_12 ///
cb_dep_09_12, fe cluster(firm_county ind3)



******************************************************
**********************Table VIII**********************
***County outcomes and Commerzbank dependence (OLS)***
******************************************************
use county_panel, clear

xtivreg2 lrgdp d_year_* ///
county_cb_dep_09_12 [weight=pop2000], fe cluster(q_ind_prod)

xtivreg2 lrgdp d_year_* ///
east_09_12 ///
county_fract_ind*_09_12 ///
county_exportquote_09_12 ///
county_importquote_09_12 ///
lb_09_12 ///
county_cb_dep_09_12 [weight=pop2000], fe cluster(q_ind_prod)

xtivreg2 lrgdp d_year_* ///
east_09_12 ///
county_fract_ind*_09_12 ///
county_exportquote_09_12 ///
county_importquote_09_12 ///
lb_09_12 ///
county_pop_density2000_09_12 ///
lcounty_gdppc2000_09_12 ///
lcounty_pop_tot2000_all_09_12 ///
hh_debt_index_09_12 ///
county_cb_dep_09_12 [weight=pop2000], fe cluster(q_ind_prod)

xtivreg2 lempl d_year_* ///
east_09_12 ///
county_exportquote_09_12 ///
county_importquote_09_12 ///
lb_09_12 ///
county_fract_ind*_09_12 ///
county_cb_dep_09_12 [weight=pop2000], fe cluster(q_ind_prod)

xtivreg2 netmigr d_year_* ///
east_09_12 ///
county_exportquote_09_12 ///
county_importquote_09_12 ///
lb_09_12 ///
county_fract_ind*_09_12 ///
county_cb_dep_09_12 [weight=pop2000], fe cluster(q_ind_prod)



*****************************************************
***********************Table IX**********************
***County outcomes and Commerzbank dependence (IV)***
*****************************************************
use county_panel, clear

ivreg2 county_cb_dep_09_12 ///
east_09_12 ///
distance_instr_09_12 [weight=pop2000], fe cluster(q_ind_prod)

ivreg2 county_cb_dep_09_12 ///
east_09_12 ///
distance_hh_09_12 distance_ffm_09_12 distance_dd_09_12 distance_dr_09_12 distance_be_09_12 ///
county_exportquote_09_12 ///
county_importquote_09_12 ///
lb_09_12 ///
county_fract_ind*_09_12 ///
county_pop_density2000_09_12 ///
lcounty_pop_tot2000_all_09_12 ///
lcounty_gdppc2000_09_12 ///
hh_debt_index_09_12 ///
distance_instr_09_12 [weight=pop2000], fe cluster(q_ind_prod)

xtivreg2 lrgdp d_year_* ///
east_09_12 ///
distance_hh_09_12 distance_ffm_09_12 distance_dd_09_12 distance_dr_09_12 distance_be_09_12 ///
(county_cb_dep_09_12 = distance_instr_09_12) [weight=pop2000], fe cluster(q_ind_prod)

xtivreg2 lrgdp d_year_* ///
east_09_12 ///
distance_hh_09_12 distance_ffm_09_12 distance_dd_09_12 distance_dr_09_12 distance_be_09_12 ///
county_exportquote_09_12 ///
county_importquote_09_12 ///
lb_09_12 ///
county_fract_ind*_09_12 ///
(county_cb_dep_09_12 = distance_instr_09_12) [weight=pop2000], fe cluster(q_ind_prod)

xtivreg2 lrgdp d_year_* ///
east_09_12 ///
distance_hh_09_12 distance_ffm_09_12 distance_dd_09_12 distance_dr_09_12 distance_be_09_12 ///
county_exportquote_09_12 ///
county_importquote_09_12 ///
lb_09_12 ///
county_fract_ind*_09_12 ///
county_pop_density2000_09_12 ///
lcounty_pop_tot2000_all_09_12 ///
lcounty_gdppc2000_09_12 ///
hh_debt_index_09_12 ///
(county_cb_dep_09_12 = distance_instr_09_12) [weight=pop2000], fe cluster(q_ind_prod)

xtivreg2 lempl d_year_* ///
east_09_12 ///
distance_hh_09_12 distance_ffm_09_12 distance_dd_09_12 distance_dr_09_12 distance_be_09_12 ///
county_exportquote_09_12 ///
county_importquote_09_12 ///
lb_09_12 ///
county_fract_ind*_09_12 ///
(county_cb_dep_09_12 = distance_instr_09_12) [weight=pop2000], fe cluster(q_ind_prod)

xtivreg2 netmigr d_year_* ///
east_09_12 ///
distance_hh_09_12 distance_ffm_09_12 distance_dd_09_12 distance_dr_09_12 distance_be_09_12 ///
county_exportquote_09_12 ///
county_importquote_09_12 ///
lb_09_12 ///
county_fract_ind*_09_12 ///
(county_cb_dep_09_12 = distance_instr_09_12) [weight=pop2000], fe cluster(q_ind_prod)



**************************************************************
****************************Table X***************************
***The direct and indirect effects on firm employment growth***
**************************************************************
use firm_crosssection, clear

ivreg2 empl_growth_0812 i.ind3 i.size_c1 lage importquote exportquote ///
cb_dep county_cb_dep_other_firms, cluster(q_ind_prod)

ivreg2 empl_growth_0812 i.ind3 i.size_c1 lage importquote exportquote ///
east ///
lb ///
county_fract_ind* ///
county_pop_density2000 ///
lcounty_gdppc2000 ///
lcounty_pop_tot2000_all ///
county_exportquote ///
county_importquote ///
hh_debt_index ///
cb_dep county_cb_dep_other_firms, cluster(q_ind_prod)



***********************************************************************
********************************Table XI*******************************
***The implied county employment change based on different estimates***
***********************************************************************

//This table uses  point estimates from Tables VI, VIII, IX, X, so no new results are required.



*********************************************
******************Table XII******************
***Firm patents and Commerzbank dependence***
*********************************************
use firm_patents, clear

ivreg2 gs_patent ///
dummy_size_c1_* dummy_ind3_* dummy_firm_county_* lage importquote exportquote ///
cb_dep_patent_old cb_dep_no_patent_old, cluster(firm_county ind3)

nbreg patents_d lpatents_old ///
dummy_size_c1_* dummy_ind3_* dummy_state_* lage importquote exportquote ///
cb_dep_patent_old, vce(cluster ind3)

nbreg patents_pre lpatents_old ///
dummy_size_c1_* dummy_ind3_* dummy_state_* lage importquote exportquote ///
cb_dep_patent_old, vce(cluster ind3)



*****************************
**********Figure IV**********
***Firm employment effects***
*****************************
use firm_panel, clear

collapse (mean) ln, by(cb_rel year)
gen ln2006=ln if year==2006
bysort cb_rel: egen ln_2006 = mean(ln2006)
gen ln_norm_2006 =ln /ln_2006

twoway (line ln_norm_2006 year if cb_rel==0, lwidth(thick) lpattern(longdash))  (line ln_norm_2006 year if cb_rel==1, lwidth(thick)), ///
ytitle("Mean ln employment (2006==1)") xtitle("") xlabel(#6) ///
legend(label(1 "Not dependent on CB") label(2 "Dependent on CB")) ///
graphregion(color(white))



****************************************************************************
**********************************Figure V**********************************
***County GDP growth, Commerzbank dependence, and the distance instrument***
****************************************************************************
use county_panel, clear

//OLS
twoway ///
(scatter lrgdp_07_12 county_cb_dep if year==2012) ///
(lfit lrgdp_07_12 county_cb_dep if year==2012, ///
lwidth(thick) graphregion(color(white)) ///
ytitle("County GDP growth 2007-12") xtitle("County CB dependence") legend(off))

//Residualized reduced form
reg distance_instr ///
east distance_hh distance_ffm distance_dd distance_dr distance_be if year==2012 [weight=pop2000], cluster(q_ind_prod)
predict distance_instr_res, resid
replace distance_instr_res = distance_instr_res + _b[_cons]

reg lrgdp_07_12 ///
east distance_hh distance_ffm distance_dd distance_dr distance_be if year==2012 [weight=pop2000], cluster(q_ind_prod)
predict lrgdp_07_12_res, resid
replace lrgdp_07_12_res = lrgdp_07_12_res + _b[_cons]

twoway ///
(scatter lrgdp_07_12_res distance_instr_res if year==2012) ///
(lfit lrgdp_07_12_res distance_instr_res if year==2012, ///
lwidth(thick) graphregion(color(white)) ///
ytitle("County GDP growth 2007-12 (residualized)") xtitle("Distance instrument (residualized)") legend(off))



*************************************************************************
********************************Figure VI********************************
***Reduced-form impact of the instrument on the county GDP growth rate***
*************************************************************************
use county_panel, clear

xtreg dlrgdp i.year ///
d#c.county_fract_ind* ///
d#lb ///
d#east ///
d#c.county_exportquote ///
d#c.county_importquote ///
d#c.lcounty_gdppc2000 ///
d#c.lcounty_pop_tot2000_all ///
d#c.county_pop_density2000 ///
d#c.hh_debt_index ///
d#c.cdist_hh_20457_100 ///
d#c.cdist_ffm_60311_100 ///
d#c.cdist_ddorf_40210_100 ///
d#c.cdist_dr_1067_100 ///
d#c.cdist_be_10117_100 ///
distance_instr_2002 ///
distance_instr_2003 ///
distance_instr_2004 ///
distance_instr_2005 ///
distance_instr_2006 ///
distance_instr_2007 ///
distance_instr_2008 ///
distance_instr_2009 ///
distance_instr_2010 ///
distance_instr_2011 ///
distance_instr_2012, fe cluster(q_ind_prod)

coefplot, keep(distance_instr_2002 ///
distance_instr_2003 ///
distance_instr_2004 ///
distance_instr_2005 ///
distance_instr_2006 ///
distance_instr_2007 ///
distance_instr_2008 ///
distance_instr_2009 ///
distance_instr_2010 ///
distance_instr_2011 ///
distance_instr_2012) ///
vertical ytitle("Coefficient on the distance instrument") xtitle("") ///
yline(0) ///
graphregion(color(white)) ///
levels(90)



******************************************************
**********************Figure VII**********************
***The size of the indirect effect by industry type***
******************************************************
use firm_crosssection, clear

ivreg2 empl_growth_0812 i.ind3 i.size_c1 lage importquote exportquote ///
east ///
lb ///
county_fract_ind* ///
county_pop_density2000 ///
lcounty_gdppc2000 ///
lcounty_pop_tot2000_all ///
county_exportquote ///
county_importquote ///
hh_debt_index ///
i.innovation_category i.tradability_category ///
cb_dep ///
county_cb_dep_other_firms_1 ///
county_cb_dep_other_firms_2 ///
county_cb_dep_other_firms_3 ///
county_cb_dep_other_firms_4 ///
county_cb_dep_other_firms_5 ///
county_cb_dep_other_firms_6 ///
county_cb_dep_other_firms_7, cluster(q_ind_prod)


//Add regression results to a matrix and then plot the contents of the matrix
matrix		G1 = J(7, 3, 0) 		
matrix 		coln G1 = beta ll95 ul95

lincom [county_cb_dep_other_firms_1]
	matrix	G1[1,1] 	= `r(estimate)'
	matrix	G1[1,2] 	= `r(estimate)' - 1.645*`r(se)'
	matrix	G1[1,3] 	= `r(estimate)' + 1.645*`r(se)'

lincom [county_cb_dep_other_firms_2]
	matrix	G1[2,1] 	= `r(estimate)'
	matrix	G1[2,2] 	= `r(estimate)' - 1.645*`r(se)'
	matrix	G1[2,3] 	= `r(estimate)' + 1.645*`r(se)'

lincom [county_cb_dep_other_firms_3]
	matrix	G1[3,1] 	= `r(estimate)'
	matrix	G1[3,2] 	= `r(estimate)' - 1.645*`r(se)'
	matrix	G1[3,3] 	= `r(estimate)' + 1.645*`r(se)'

lincom [county_cb_dep_other_firms_4]
	matrix	G1[4,1] 	= `r(estimate)'
	matrix	G1[4,2] 	= `r(estimate)' - 1.645*`r(se)'
	matrix	G1[4,3] 	= `r(estimate)' + 1.645*`r(se)'

lincom [county_cb_dep_other_firms_5]
	matrix	G1[5,1] 	= `r(estimate)'
	matrix	G1[5,2] 	= `r(estimate)' - 1.645*`r(se)'
	matrix	G1[5,3] 	= `r(estimate)' + 1.645*`r(se)'

lincom [county_cb_dep_other_firms_6]
	matrix	G1[6,1] 	= `r(estimate)'
	matrix	G1[6,2] 	= `r(estimate)' - 1.645*`r(se)'
	matrix	G1[6,3] 	= `r(estimate)' + 1.645*`r(se)'

lincom [county_cb_dep_other_firms_7]
	matrix	G1[7,1] 	= `r(estimate)'
	matrix	G1[7,2] 	= `r(estimate)' - 1.645*`r(se)'
	matrix	G1[7,3] 	= `r(estimate)' + 1.645*`r(se)'

matlist		G1

coefplot 	(matrix(G1[.,1]), ci((G1[.,2] G1[.,3]))), ///
						ytitle("Indirect effect on firm employment growth") yline(0) ///
						barwidth(0.9) fcolor(*.8) ///
						recast(bar) vertical ciopts(recast(rcap)) citop ///
						groups(r1 r2 r3 = `""{bf:Producers of}" "{bf:tradables}""' ///
						r4 r5 = `""{bf:Producers of}" "{bf:part-tradables}""'	///
						r6 r7 = `""{bf:Producers of}" "{bf:non-tradables}""') ///
						legend(off)	///
						coeflabel(r1=`"Low"' r2=`""Medium" "Innovation""' r3=`"High"' ///
						r4=`"Low"' r5=`""Medium" "Innovation""' ///
						r6=`"Low"' r7=`""Medium" "Innovation""') ///
						graphregion(color(white))
