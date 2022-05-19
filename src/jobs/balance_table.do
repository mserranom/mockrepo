
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


********************************************************************************
* Wildcards Definitions                                                        *
********************************************************************************

* outcome variables
loc dep_vars       political_supporter populist_party
* time-varying individual controls
loc controls_t_ind curr_unempl self_employed in_education retired educ_years
* time-invariant individual controls
loc controls_c_ind male age age_sq residence_gdr89 married migrant_status
* time-varying household controls
loc controls_t_hh  hh_size hh_children house_owner hh_hasloans ln_hh_disp_inc
* specific time-varying county controls
* loc controls_t_kk  ln_foreigner ln_gdp ln_gdp_pc ln_pop

********************************************************************************
* Define Variable Titles                                                       *
********************************************************************************

* Demographics
loc lbl_male               "Male"
loc lbl_age                "Age"
loc lbl_residence_gdr89    "Residence in GDR in 1989"
loc lbl_married            "Married"
loc lbl_migrant_status     "Direct/Indirect Migrant"
* Education 
loc lbl_voc_educ           "Vocational Degree or Higher"
loc lbl_uni_degree         "University Degree"
loc lbl_educ_years         "Years of Education"
* Occupational Status
loc lbl_curr_unempl        "Currently Unemployed"
loc lbl_self_employed      "Self-Employed"
loc lbl_in_education       "In Education"
loc lbl_retired            "Retired"
* Household
loc lbl_hh_size            "Household Size"
loc lbl_hh_children        "Number of Children in HH"
loc lbl_house_owner        "Home-Ownership"
loc lbl_hh_hasloans        "Presence of Outstanding Loans"
loc lbl_ln_hh_disp_inc     "ln(Annual Household Disposable Income)"

********************************************************************************
* Open Dataset                                                                 *
********************************************************************************

u "${mydata}/final_data", clear

* exclude individuals younger than 16
soepdrop if age < 16

* rescale gdp
g gdp_resc = gdp / 1000000

* dpost
g dpost = (wave > 2008)

********************************************************************************
* Balance Table                                                                *
********************************************************************************

noi di ">>>table>>>balance_table.tex>>>"

* Table Header
noi di "\begin{table}[H]"
noi di "\centering"
noi di "\caption{Balance}"
noi di "\label{tab:balance}"
noi di "\resizebox{\textwidth}{!}{%"
noi di "\begin{tabular}{l*{1}{cccc}}"
noi di "\toprule"
noi di "& \multicolumn{1}{c}{(1)} & " _c
noi di "\multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{T-test}\\"
noi di "& \multicolumn{1}{c}{(Pre Credit Shock)} & " _c
noi di "& \multicolumn{1}{c}{(Post Credit Shock)} & " _c
noi di "\multicolumn{1}{c}{P-value} \\"
noi di "Variable & \multicolumn{1}{c}{Mean/SE} & \multicolumn{1}{c}{Mean/SE}" _c
noi di " & (2) - (1) \\"
noi di "\midrule"

noi mean `controls_t_ind' `controls_c_ind' `controls_t_hh' [pw=crosswgt] ///
    if !dpost, vce(cl kkz_rek)
loc pre_clust = e(N_clust)
loc pre_obs   = e(N)
mat pre_mean = r(table)'
mat pre_mean = pre_mean[1..., 1..2]
noi mean `controls_t_ind' `controls_c_ind' `controls_t_hh' [pw=crosswgt] ///
    if dpost, vce(cl kkz_rek)
loc post_clust = e(N_clust)
loc post_obs   = e(N)
mat post_mean = r(table)'
mat post_mean = post_mean[1..., 1..2]

loc counter = 1
foreach var in `controls_t_ind' `controls_c_ind' `controls_t_hh' {

    reghdfe `var', a(kkz_rek wave) vce(cl kkz_rek) res(res_`var')
    noi reg res_`var' dpost [pw=crosswgt], vce(cl kkz_rek)
    mat mat_test = r(table)'
    mat mat_test = mat_test[1, 1..4]

    if (`counter' == 1) mat test_results = mat_test
    else                mat test_results = test_results \ mat_test

    loc counter = `counter' + 1
}

mat rtab = pre_mean , post_mean , test_results

forval r = 1/`=rowsof(rtab)' {

    loc p_means
    loc p_serr

    forval c = 1/6 {
        if (mod(`c', 2)) == 1 {
            loc p_means  "`p_means' & `=string(rtab[`r', `c'], "%9.3fc")'"
            if (`c' == 5) {
                if(rtab[`r', 8] < 0.01) loc p_means "`p_means'***"
                else if (rtab[`r', 8] < 0.05) loc p_means "`p_means'**"
                else if (rtab[`r', 8] < 0.1)  loc p_means "`p_means'*"
                else                          loc p_means "`p_means'"
            }
        }

        else {
            loc p_serr "`p_serr' & (`=string(rtab[`r', `c'], "%9.3fc")')"
            if (`c' == 6) loc p_serr "`p_serr'"
        }

    }

    di "`lbl_`=word("`: rownames rtab'", `r')'' `p_means'" " \\"
    di "`p_serr'" " \\"

}

di "\midrule"
di "Number of Observations & `pre_obs' & `post_obs' & \\"
di "Number of Clusters     & `pre_clust' & `post_clust' & \\"
di "\midrule"


********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***