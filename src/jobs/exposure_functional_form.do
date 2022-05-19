

qui {

********************************************************************************
* Stata Settings                                                               *
********************************************************************************

version 14.2
clear all
set more off
set varabbrev off
set linesize 255
set matsize  10800
set maxvar   32767

********************************************************************************
* Wildcards Definitions                                                        *
********************************************************************************

noi di "Wildcards Definitions ..."

* outcome variables
loc dep_vars       political_supporter populist_party

* time-invariant individual controls
loc controls_c_ind male age age_sq residence_gdr89 married migrant_status

* full controls without time-invariant individual controls
loc full_controls  curr_unempl self_employed in_education retired ///
                   educ_years hh_size hh_children house_owner hh_hasloans    ///
                   ln_hh_disp_inc ln_foreigner ln_pop ln_pop_d

********************************************************************************
* Open Dataset                                                                 *
********************************************************************************

noi di "Open Dataset and Binary Treatment ..."

u "${mydata}/final_data", clear

* exclude individuals younger than 16
soepdrop if age < 16

g dpost = (wave > 2008)

********************************************************************************
* Treatment Functional Form Plot                                              *
********************************************************************************

noi di "Treatment Functional Form Plot ... "

foreach out of varlist `dep_vars' {

    noi di "Iterating `out' ... "

    if ("`out'" == "political_supporter") loc estn ps
    else                                  loc estn pp

    reghdfe `out' `controls_c_ind' `full_controls' [pw=crosswgt], ///
        a(kkz_rek wave) vce(cl kkz_rek) res(`estn'_res_noife)

    preserve
    collapse (mean) `estn'_res_noife [pw=crosswgt], by(cbk_past_mean dpost)

    reshape wide `estn'_res_noife, i(cbk_past_mean) j(dpost)

    g `estn'_res_noife_diff = `estn'_res_noife1 - `estn'_res_noife0

    noi di ">>>data>>>`estn'_functional_form_cbk_past_mean_noife.csv"
    noi di "exposure,`estn'_before,`estn'_after,diff"
    forval n = 1/`=_N' {
        noi di string(cbk_past_mean[`n']) ","       ///
               string(`estn'_res_noife0[`n']) ","     ///
               string(`estn'_res_noife1[`n']) ","     ///
               string(`estn'_res_noife_diff[`n'])
    }
    noi di "<<<data<<<"
    restore

    reghdfe `out' `full_controls' [pw=crosswgt], ///
        a(persnr kkz_rek wave) vce(cl kkz_rek) res(`estn'_res_ife)

    preserve
    collapse (mean) `estn'_res_ife [pw=crosswgt], by(cbk_past_mean dpost)

    reshape wide `estn'_res_ife, i(cbk_past_mean) j(dpost)

    g `estn'_res_ife_diff = `estn'_res_ife1 - `estn'_res_ife0

    noi di ">>>data>>>`estn'_functional_form_cbk_past_mean_ife.csv"
    noi di "exposure,`estn'_before,`estn'_after,diff"
    forval n = 1/`=_N' {
        noi di string(cbk_past_mean[`n']) ","       ///
               string(`estn'_res_ife0[`n']) ","     ///
               string(`estn'_res_ife1[`n']) ","     ///
               string(`estn'_res_ife_diff[`n'])
    }
    noi di "<<<data<<<"
    restore

} // outcomes

********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***