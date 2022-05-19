* mock example with the fake data
gl mydata = "~/Documents/Projects/CreditPopulism/tests/data/soep"

u "${mydata}/final_data", clear

* time-invariant individual controls
loc controls_c_ind male age age_sq residence_gdr89

* full controls without time-invariant individual controls
loc full_controls  curr_unempl self_employed in_education retired educ_years hh_size hh_children house_owner hh_hasloans ln_hh_disp_inc

g dpost = (wave > 2008)
g cbk_past_mean = contr_weekly_hours

reghdfe populist_party `full_controls' [pw=crosswgt], a(persnr federal_state wave) vce(cl federal_state) res(pp_res_ife)

preserve
collapse (mean) pp_res_ife [pw=crosswgt], by(cbk_past_mean dpost)

reshape wide pp_res_ife, i(cbk_past_mean) j(dpost)

g pp_res_ife_diff = pp_res_ife1 - pp_res_ife0

noi di ">>>data>>>pp_het_cbk_past_mean_ife.csv"
noi di "exposure,pp_before,pp_after,diff"
forval n = 1/`=_N' {
    noi di string(cbk_past_mean[`n']) ","   ///
           string(pp_res_ife0[`n']) ","     ///
           string(pp_res_ife1[`n']) ","     ///
           string(pp_res_ife_diff[`n'])
}
noi di "<<<data<<<"
restore