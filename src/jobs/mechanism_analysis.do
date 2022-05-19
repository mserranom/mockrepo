

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

* outcome variables
loc dep_vars       political_supporter populist_party
* time-varying individual controls
loc controls_t_ind curr_unempl self_employed in_education retired educ_years
* time-invariant individual controls
loc controls_c_ind male age age_sq residence_gdr89 married migrant_status
* time-varying household controls
loc controls_t_hh  hh_size hh_children house_owner hh_hasloans ln_hh_disp_inc
* specific time-varying county controls
loc controls_t_kk  ln_foreigner ln_gdp ln_gdp_pc ln_pop

loc controls_stubs controls_t_ind controls_c_ind controls_t_hh controls_t_kk
token "`controls_stubs'"
loc n: word count `controls_stubs'

forval c = 1/`n' {
    foreach var in ```c''' {
        loc missing_``c'' `missing_``c''' & !missing(`var')
    }
}

********************************************************************************
* Open Dataset and Treatment Creation                                          *
********************************************************************************

noi di "Open Dataset and Treatment Creation ..."

u "${mydata}/final_data", clear

* exclude individuals younger than 16
soepdrop if age < 16

* quadratic time trend
g wave_sq = wave^2

* Generate After-Shock Dummy Variable 2009 onwards
g dpost = (wave > 2008)

* previous firms exposure as baseline (standardized)
su cbk_past_mean [aw=crosswgt] if !missing(cbk_past_mean)
g  cbk_past_mean_post = (cbk_past_mean / `r(sd)') * dpost

g iunemp_post = curr_unempl * dpost
g iunempl_exp_post = cbk_past_mean_post * curr_unempl

bys persnr : g repl_ind = _n
bys persnr : egen total_repl = count(repl_ind)

********************************************************************************
* Individual Unemployment Interaction                                          *
********************************************************************************

noi di "TWFE Reduced Form Tables with Individual Unemployment Interaction ... "

loc titleps "Populist Support"
loc titlepp "Intention to Vote for a Populist Party"

loc panS stat(N N_clust Ymean Dsd Uimean r2_within,                          ///
         labels("Number of Observations"                                     ///
                "Number of Counties"                                         ///
                "Outcome Mean (\%)"                                          ///
                "$ sd\left(Exposure_k\right) $ (\%)"                         ///
                "Unemployment Mean (\%)"                                     ///
                "Within $ R^2 $")                                            ///
         fmt(%9.0fc %9.0fc %9.3fc %9.3fc %9.3fc))                            ///
         prehead("\midrule") posthead("") nonumbers                          ///
         prefoot("\\") postfoot("")

loc panFEF stat(kFE yFE iFE iCtl hCtl kCtl kTrends,                          ///
           labels("County-Level FE"                                          ///
                  "Wave FE"                                                  ///
                  "Individual FE"                                            ///
                  "Individual Controls"                                      ///
                  "Household Controls"                                       ///
                  "Regional Controls"                                        ///
                  "County Time Trends"))                                     ///
           d(*) prehead("") posthead("\midrule") nonumbers                   ///
           prefoot("") postfoot("")

* rescale coefficients and standard errors 100
loc opts star(* 0.1 ** 0.05 *** 0.01) b(3) se(3) booktabs                    ///
    varlab(iunemp_post               "$ u_{ikt}\ \times \ Post $"            ///
    iunempl_exp_post "$ Exposure_{k}\ \times \ u_{ikt}\ \times \ Post $")    ///
    nogaps nomtitles substitute(\_ _)
    * transform(@*100 100) nogaps nomtitles substitute(\_ _)
*cbk_past_mean_post "$ Exposure_{k}\ \times \ Post $"

* loc kvar k(cbk_past_mean_post iunemp_post iunempl_exp_post)
loc kvar k(iunemp_post iunempl_exp_post)

loc foot postfoot("\bottomrule"                                              ///
                  "\end{tabular}"                                            ///
                  "}%"                                                       ///
                  "\caption*{\footnotesize \textit{Notes:}~@note}"           ///
                  "\end{table}")                                             ///
         d(*) noobs nonumbers                                                ///
         prehead("") posthead("") prefoot("")

foreach out of varlist `dep_vars' {

    noi di "Iterating `out' ... "

    if ("`out'" == "political_supporter") loc estn ps
    else                                  loc estn pp

    loc head prehead( "\begin{table}"                                        ///
                      "\centering"                                           ///
                      "\caption{@title}"                                     ///
                      "\label{tab:rf_`estn'_cbk_past_mean_iunempl}"          ///
                      "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"           ///
                      "\resizebox{\textwidth}{!}{%"                          ///
                      "\begin{tabular}{l*{1}cccccccc}"                       ///
                      "\toprule")                                            ///
             posthead(" & \multicolumn{1}{c}{(1)}"                           ///
                      " & \multicolumn{1}{c}{(2)}"                           ///
                      " & \multicolumn{1}{c}{(3)}"                           ///
                      " & \multicolumn{1}{c}{(4)}"                           ///
                      " & \multicolumn{1}{c}{(5)}"                           ///
                      " & \multicolumn{1}{c}{(6)}"                           ///
                      " & \multicolumn{1}{c}{(7)}"                           ///
                      " & \multicolumn{1}{c}{(8)}"                           ///
                      " \\ ")                                                ///
             title(   "The Effect of the Credit Shock"                       ///
                      "on `title`estn'': "                                   ///
                      "Heterogeneity Effects on Layoffs")                    ///
             d(*) noobs prefoot("") postfoot("") nonumbers

    noi su `out' if !missing(`out') & !missing(cbk_past_mean)                ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        & !missing(curr_unempl) [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su curr_unempl if !missing(curr_unempl) & !missing(`out')            ///
        & !missing(cbk_past_mean) & !missing(kkz_rek) & !missing(wave)       ///
        [aw=crosswgt]
    loc unempl_mean = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave) & !missing(curr_unempl)
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * baseline: FEonly
    noi reghdfe `out' iunemp_post iunempl_exp_post        ///
        curr_unempl [pw=crosswgt], a(kkz_rek wave) vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Uimean  `unempl_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     No
        estadd loc iCtl    No
        estadd loc hCtl    No
        estadd loc kCtl    No
        estadd loc kTrends No
        estimates store `estn'_base

    noi su `out' if !missing(`out') & !missing(cbk_past_mean)                ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind' [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su curr_unempl if !missing(`out') & !missing(cbk_past_mean)          ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind' [aw=crosswgt]
    loc unempl_mean = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * adding individual controls (time-varying and invariant)
    noi reghdfe `out' iunemp_post iunempl_exp_post        ///
        `controls_c_ind' `controls_t_ind' [pw=crosswgt],                     ///
        a(kkz_rek wave) vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Uimean  `unempl_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     No
        estadd loc iCtl    Yes
        estadd loc hCtl    No
        estadd loc kCtl    No
        estadd loc kTrends No
        estimates store `estn'_ind

    noi su `out' if !missing(`out') & !missing(cbk_past_mean)                ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su curr_unempl if !missing(`out') & !missing(cbk_past_mean)          ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' [aw=crosswgt]
    loc unempl_mean = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh'
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * adding time-varying household controls
    noi reghdfe `out' iunemp_post iunempl_exp_post        ///
        `controls_c_ind' `controls_t_ind' `controls_t_hh' [pw=crosswgt],     ///
        a(kkz_rek wave) vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Uimean  `unempl_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     No
        estadd loc iCtl    Yes
        estadd loc hCtl    Yes
        estadd loc kCtl    No
        estadd loc kTrends No
        estimates store `estn'_hh

    * adding linear-quadratic time trends
    noi reghdfe `out' iunemp_post iunempl_exp_post        ///
        `controls_c_ind' `controls_t_ind' `controls_t_hh' [pw=crosswgt],     ///
        a(kkz_rek wave i.kkz_rek#c.wave i.kkz_rek#c.wave_sq)                 ///
        vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Uimean  `unempl_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     No
        estadd loc iCtl    Yes
        estadd loc hCtl    Yes
        estadd loc kCtl    No
        estadd loc kTrends Yes
        estimates store `estn'_trends

    noi su `out' if !missing(`out') & !missing(cbk_past_mean)                ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' `missing_controls_t_kk' [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su curr_unempl if !missing(`out') & !missing(cbk_past_mean)          ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' `missing_controls_t_kk' [aw=crosswgt]
    loc unempl_mean = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' `missing_controls_t_kk'
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * adding time-varying county controls
    noi reghdfe `out' iunemp_post iunempl_exp_post        ///
        `controls_c_ind' `controls_t_ind'                                    ///
        `controls_t_hh' `controls_t_kk' [pw=crosswgt],                       ///
        a(kkz_rek wave) vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Uimean  `unempl_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     No
        estadd loc iCtl    Yes
        estadd loc hCtl    Yes
        estadd loc kCtl    Yes
        estadd loc kTrends No
        estimates store `estn'_kreise

    noi su `out' if !missing(`out') & !missing(cbk_past_mean)                ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' & total_repl > 1 [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su curr_unempl if !missing(`out') & !missing(cbk_past_mean)          ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' & total_repl > 1 [aw=crosswgt]
    loc unempl_mean = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' & total_repl > 1
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * adding individualFE
    noi reghdfe `out' iunemp_post iunempl_exp_post        ///
        `controls_t_ind' `controls_t_hh' [pw=crosswgt],                      ///
        a(persnr kkz_rek wave) vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Uimean  `unempl_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     Yes
        estadd loc iCtl    Yes
        estadd loc hCtl    Yes
        estadd loc kCtl    No
        estadd loc kTrends No
        estimates store `estn'_ife

    * adding individualFE and linear-quadratic time trends
    noi reghdfe `out' iunemp_post iunempl_exp_post        ///
        `controls_t_ind' `controls_t_hh' [pw=crosswgt],                      ///
        a(persnr kkz_rek wave i.kkz_rek#c.wave i.kkz_rek#c.wave_sq)          ///
        vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Uimean  `unempl_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     Yes
        estadd loc iCtl    Yes
        estadd loc hCtl    Yes
        estadd loc kCtl    No
        estadd loc kTrends Yes
        estimates store `estn'_ifetrends

    noi su `out' if !missing(`out') & !missing(cbk_past_mean)                ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_t_ind' `missing_controls_t_hh'                     ///
        `missing_controls_t_kk' & total_repl > 1 [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su curr_unempl if !missing(`out') & !missing(cbk_past_mean)          ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_t_ind' `missing_controls_t_hh'                     ///
        `missing_controls_t_kk' & total_repl > 1 [aw=crosswgt]
    loc curr_unempl = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_t_ind' `missing_controls_t_hh'                     ///
        `missing_controls_t_kk' & total_repl > 1
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * adding individualFE and time-varying county controls
    noi reghdfe `out' iunemp_post iunempl_exp_post        ///
        `controls_t_ind' `controls_t_hh' `controls_t_kk' [pw=crosswgt],      ///
        a(persnr kkz_rek wave) vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Uimean  `unempl_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     Yes
        estadd loc iCtl    Yes
        estadd loc hCtl    Yes
        estadd loc kCtl    Yes
        estadd loc kTrends No
        estimates store `estn'_kife


    noi {

        di ">>>table>>>rf_`estn'_cbk_past_mean_iunempl.tex>>>"
        esttab `estn'_base                                                   ///
               `estn'_ind                                                    ///
               `estn'_hh                                                     ///
               `estn'_kreise                                                 ///
               `estn'_trends                                                 ///
               `estn'_ife                                                    ///
               `estn'_kife                                                   ///
               `estn'_ifetrends,                                             ///
               `opts' `head'
        esttab `estn'_base                                                   ///
               `estn'_ind                                                    ///
               `estn'_hh                                                     ///
               `estn'_kreise                                                 ///
               `estn'_trends                                                 ///
               `estn'_ife                                                    ///
               `estn'_kife                                                   ///
               `estn'_ifetrends,                                             ///
               `opts' `panS' `kvar'
        esttab `estn'_base                                                   ///
               `estn'_ind                                                    ///
               `estn'_hh                                                     ///
               `estn'_kreise                                                 ///
               `estn'_trends                                                 ///
               `estn'_ife                                                    ///
               `estn'_kife                                                   ///
               `estn'_ifetrends,                                             ///
               `opts' `panFEF'
        esttab `estn'_base                                                   ///
               `estn'_ind                                                    ///
               `estn'_hh                                                     ///
               `estn'_kreise                                                 ///
               `estn'_trends                                                 ///
               `estn'_ife                                                    ///
               `estn'_kife                                                   ///
               `estn'_ifetrends,                                             ///
               `opts' `foot'                                                 ///
                note("This table shows ... \hl{XXX}.")
        di "<<<table<<<"

    } // noi


    noi di "Done!"

} // outcomes

********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***