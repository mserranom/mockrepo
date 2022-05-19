

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
loc dep_vars manifesto_bf              ///
             manifesto_pop_rood        ///
             manifesto_both_rood       ///
             protokolle_bf             ///
             protokolle_pop_rood       ///
             protokolle_both_rood      ///
             manifesto_bf_rood_slda    ///
             manifesto_pop_rood_slda   ///
             manifesto_both_rood_slda  ///
             protokolle_bf_rood_slda   ///
             protokolle_pop_rood_slda  ///
             protokolle_both_rood_slda

* time-invariant individual controls
loc controls_c_ind male age age_sq residence_gdr89 married migrant_status

* full controls without time-invariant individual controls
loc full_controls  curr_unempl self_employed in_education retired ///
                   educ_years hh_size hh_children house_owner hh_hasloans    ///
                   ln_hh_disp_inc ln_foreigner ln_pop

loc controls_stubs controls_c_ind full_controls
token "`controls_stubs'"
loc n: word count `controls_stubs'

forval c = 1/`n' {
    foreach var in ```c''' {
        loc missing_``c'' `missing_``c''' & !missing(`var')
    }
}

loc first_lag = (2009 - 2000 + 1) - 1

********************************************************************************
* Open Dataset and Treatment Interactions                                      *
********************************************************************************

noi di "Open Dataset and Treatment Interactions ..."

u "${mydata}/final_data", clear

* exclude individuals younger than 16
soepdrop if age < 16

* quadratic time trend
g wave_sq = wave^2

* Generate After-Shock Dummy Variable 2009 onwards
g dpost = (wave > 2008)
g dpre =  (wave < 2008)

bys persnr : g repl_ind = _n
bys persnr : egen total_repl = count(repl_ind)

tab wave, g(in)
unab totalvar : in? in??

su cbk_past_mean [aw=crosswgt] if !missing(cbk_past_mean), d
g  cbk_past_mean_std = (cbk_past_mean / `r(sd)')

g cbk_past_mean_iqr  = 0 ///
    if cbk_past_mean <= `r(p25)' & !missing(cbk_past_mean)
replace cbk_past_mean_iqr  = 1 ///
    if cbk_past_mean  > `r(p75)' & !missing(cbk_past_mean)
g cbk_past_mean_1090 = 0 ///
    if cbk_past_mean <= `r(p10)' & !missing(cbk_past_mean)
replace cbk_past_mean_1090 = 1 ///
    if cbk_past_mean  > `r(p90)' & !missing(cbk_past_mean)

g cbk_past_mean_std_pre   = cbk_past_mean_std * dpre
g cbk_past_mean_std_post  = cbk_past_mean_std * dpost
g cbk_past_mean_iqr_pre   = cbk_past_mean_iqr * dpre
g cbk_past_mean_iqr_post  = cbk_past_mean_iqr * dpost
g cbk_past_mean_1090_pre  = cbk_past_mean_1090 * dpre
g cbk_past_mean_1090_post = cbk_past_mean_1090 * dpost

foreach var of varlist `totalvar' {

    if ("`var'" != "in`first_lag'") {
        g `var'_cbk_past_mean_std  = `var' * cbk_past_mean_std
        g `var'_cbk_past_mean_iqr  = `var' * cbk_past_mean_iqr
        g `var'_cbk_past_mean_1090 = `var' * cbk_past_mean_1090
    }

} // interactions

foreach s in p50 p75 p90 {

    g cbk_past_mean_`s' = (cbk_past_mean > `r(`s')') if !missing(cbk_past_mean)
    g cbk_past_mean_`s'_pre  = cbk_past_mean_`s' * dpre
    g cbk_past_mean_`s'_post = cbk_past_mean_`s' * dpost

    foreach var of varlist `totalvar' {
        if ("`var'" != "in`first_lag'") {
            g `var'_cbk_past_mean_`s' = `var' * cbk_past_mean_`s'
        }
    } // interactions

} // stats

************************************************************
* Amplify Text Analysis Results                            *
************************************************************

foreach s in manifesto protokolle {
    * re-scale Rooduijn scores for baseline with text analysis
    replace `s'_bf_rood_slda   = `s'_bf_rood_slda * 100
    replace `s'_pop_rood_slda  = `s'_pop_rood_slda * 100
    egen    `s'_both_rood_slda = rowmean(`s'_bf_rood_slda `s'_pop_rood_slda)
    * re-scale Rooduijn scores for baseline with dictionary
    replace `s'_bf   = `s'_bf * 100
    replace `s'_pop_rood  = `s'_pop_rood * 100
    egen    `s'_both_rood = rowmean(`s'_bf `s'_pop_rood)
} // corpus



********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***