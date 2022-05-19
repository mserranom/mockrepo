version 14.2
clear all
set more off
set varabbrev off
set linesize 255
set matsize  10800
set maxvar   32767

* outcome variables
loc dep_vars       political_supporter populist_party
* time-varying individual controls
loc controls_t_ind curr_unempl self_employed in_education retired educ_years
* time-invariant individual controls
loc controls_c_ind male age age_sq residence_gdr89 married migrant_status
* time-varying household controls
loc controls_t_hh  hh_size hh_children house_owner hh_hasloans ln_hh_disp_inc
* time-invariant county controls
*loc controls_c_kk  east crisis_landesbank ln_foreigner_2006 ln_gdp_2006 ln_gdp_pc_2006 ln_pop_2006
loc controls_c_kk  crisis_landesbank ln_foreigner_2006 ln_gdp_2006 ln_gdp_pc_2006 ln_pop_2006
* specific time-varying county controls
loc controls_t_kk  ln_foreigner ln_gdp ln_gdp_pc ln_pop


u "${mydata}/final_data", clear

* exclude individuals younger than 16
soepdrop if age < 16
soepdrop if !east

* quadratic time trend
g wave_sq = wave^2

* Generate After-Shock Dummy Variable 2009 onwards
g dpost = (wave > 2008)

* county variables at 2006
preserve
collapse (mean) `controls_t_kk', by(kkz_rek wave)
soepkeep if wave == 2006
soepdrop wave
foreach var of varlist `controls_t_kk' {
    rename `var' `var'_2006
}
tempfile controls_2006
save `controls_2006'
restore

* previous firms exposure as baseline (standardized)
su cbk_past_mean [aw=crosswgt] if !missing(cbk_past_mean)
g  cbk_past_mean_post = (cbk_past_mean / `r(sd)') * dpost

g east_post = east * cbk_past_mean_post

noi merge m:1 kkz_rek using `controls_2006'
foreach var of varlist `controls_c_kk' {
    replace `var' = `var' * dpost
}

reghdfe populist_party cbk_past_mean_post `controls_t_ind' `controls_c_ind'  ///
    `controls_t_hh' `controls_c_kk' [pw=crosswgt],                           ///
    a( kkz_rek wave) vce(cl kkz_rek)