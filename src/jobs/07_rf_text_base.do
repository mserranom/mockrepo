

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

bys persnr : g repl_ind = _n
bys persnr : egen total_repl = count(repl_ind)


************************************************************
* Amplify Text Analysis Results                            *
************************************************************

foreach s in manifesto protokolle {
    * Rescale Rooduijn scores for baseline with text analysis
    replace `s'_bf_rood_slda   = `s'_bf_rood_slda * 100
    replace `s'_pop_rood_slda  = `s'_pop_rood_slda * 100
    egen    `s'_both_rood_slda = rowmean(`s'_bf_rood_slda `s'_pop_rood_slda)
    * Rescale Rooduijn scores for baseline with dictionary
    replace `s'_bf   = `s'_bf * 100
    replace `s'_pop_rood  = `s'_pop_rood * 100
    egen    `s'_both_rood = rowmean(`s'_bf `s'_pop_rood)
} // Corpus

********************************************************************************
* TWFE Baseline with Text Analysis                                             *
********************************************************************************

foreach out of varlist `dep_vars' {

    if strpos("`out'", "manifesto") loc estn mani
    else                            loc estn prot

    if strpos("`out'", "bf")        loc estn `estn'_bf
    else if strpos("`out'", "pop")  loc estn `estn'_pop
    else                            loc estn `estn'_both

    if strpos("`out'", "slda")      loc estn `estn'_slda
    else                            loc estn `estn'

    su `out' [aw=crosswgt] if !missing(`out')
    replace `out' = (`out' / r(sd))

    noi su `out' if !missing(`out') & !missing(cbk_past_mean)                ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' `missing_controls_t_kk' [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' `missing_controls_t_kk'
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * adding time-varying county controls
    noi reghdfe `out' cbk_past_mean_post `controls_c_ind' `controls_t_ind'   ///
        `controls_t_hh' `controls_t_kk' [pw=crosswgt],                       ///
        a(kkz_rek wave) vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
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
        `missing_controls_t_hh' [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh'
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * adding linear-quadratic time trends
    noi reghdfe `out' cbk_past_mean_post                                     ///
        `controls_c_ind' `controls_t_ind' `controls_t_hh' [pw=crosswgt],     ///
        a(kkz_rek wave i.kkz_rek#c.wave i.kkz_rek#c.wave_sq)                 ///
        vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
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
        `missing_controls_t_hh' & total_repl > 1 [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_c_ind' `missing_controls_t_ind'                    ///
        `missing_controls_t_hh' & total_repl > 1
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * adding individualFE
    noi reghdfe `out' cbk_past_mean_post                                     ///
        `controls_t_ind' `controls_t_hh' [pw=crosswgt],                      ///
        a(persnr kkz_rek wave) vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     Yes
        estadd loc iCtl    Yes
        estadd loc hCtl    Yes
        estadd loc kCtl    No
        estadd loc kTrends No
        estimates store `estn'_ife

    noi su `out' if !missing(`out') & !missing(cbk_past_mean)                ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_t_ind' `missing_controls_t_hh'                     ///
        `missing_controls_t_kk' & total_repl > 1 [aw=crosswgt]
    loc outcome_mean = round(`r(mean)' * 100, 0.001)
    noi su cbk_past_mean if !missing(cbk_past_mean) & !missing(`out')        ///
        & !missing(kkz_rek) & !missing(wave)                                 ///
        `missing_controls_t_ind' `missing_controls_t_hh'                     ///
        `missing_controls_t_kk' & total_repl > 1
    loc treat_sd     = round(`r(sd)' * 100, 0.001)

    * adding individualFE and time-varying county controls
    noi reghdfe `out' cbk_past_mean_post `controls_t_ind' `controls_t_hh'    ///
        `controls_t_kk' [pw=crosswgt], a(persnr kkz_rek wave) vce(cl kkz_rek)
        estadd loc Ymean   `outcome_mean'
        estadd loc Dsd     `treat_sd'
        estadd loc kFE     Yes
        estadd loc yFE     Yes
        estadd loc iFE     Yes
        estadd loc iCtl    Yes
        estadd loc hCtl    Yes
        estadd loc kCtl    Yes
        estadd loc kTrends No
        estimates store `estn'_kife

} // Outcomes

loc headMain prehead("\begin{table}[H]"                                      ///
                     "\centering"                                            ///
                     "\caption{@title}"                                      ///
                     "\label{tab:rf_text_cbk_past_mean_main_scores}"         ///
                     "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"            ///
                     "\resizebox{\textwidth}{!}{%"                           ///
                     "\begin{tabular}{l*{1}cccccccccccc}"                    ///
                     "\toprule")                                             ///
    posthead(" & \multicolumn{4}{c}{\textbf{Banking and Financial Crisis}}"  ///
             " & \multicolumn{4}{c}{\textbf{Populism}}"                      ///
             " & \multicolumn{4}{c}{\textbf{Combined}}"                      ///
             " \\ "                                                          ///
             " & \multicolumn{1}{c}{(1)}"                                    ///
             " & \multicolumn{1}{c}{(2)}"                                    ///
             " & \multicolumn{1}{c}{(3)}"                                    ///
             " & \multicolumn{1}{c}{(4)}"                                    ///
             " & \multicolumn{1}{c}{(5)}"                                    ///
             " & \multicolumn{1}{c}{(6)}"                                    ///
             " & \multicolumn{1}{c}{(7)}"                                    ///
             " & \multicolumn{1}{c}{(8)}"                                    ///
             " & \multicolumn{1}{c}{(9)}"                                    ///
             " & \multicolumn{1}{c}{(10)}"                                   ///
             " & \multicolumn{1}{c}{(11)}"                                   ///
             " & \multicolumn{1}{c}{(12)}"                                   ///
             " \\ "                                                          ///
             " \midrule")                                                    ///
    title("The Effect of the Credit Shock on"                                ///
    "Political Preferences: Outcome as Topic Model Scores")                  ///
    d(*) noobs prefoot("") postfoot("") nonumbers

loc headDict prehead("\begin{table}"                                         ///
                     "\centering"                                            ///
                     "\caption{@title}"                                      ///
                     "\label{tab:rf_text_cbk_past_mean_main_dict}"           ///
                     "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"            ///
                     "\resizebox{\textwidth}{!}{%"                           ///
                     "\begin{tabular}{l*{1}cccccccccccc}"                    ///
                     "\toprule")                                             ///
    posthead(" & \multicolumn{4}{c}{\textbf{Banking and Financial Crisis}}"  ///
             " & \multicolumn{4}{c}{\textbf{Populism}}"                      ///
             " & \multicolumn{4}{c}{\textbf{Combined}}"                      ///
             " \\ "                                                          ///
             " & \multicolumn{1}{c}{(1)}"                                    ///
             " & \multicolumn{1}{c}{(2)}"                                    ///
             " & \multicolumn{1}{c}{(3)}"                                    ///
             " & \multicolumn{1}{c}{(4)}"                                    ///
             " & \multicolumn{1}{c}{(5)}"                                    ///
             " & \multicolumn{1}{c}{(6)}"                                    ///
             " & \multicolumn{1}{c}{(7)}"                                    ///
             " & \multicolumn{1}{c}{(8)}"                                    ///
             " & \multicolumn{1}{c}{(9)}"                                    ///
             " & \multicolumn{1}{c}{(10)}"                                   ///
             " & \multicolumn{1}{c}{(11)}"                                   ///
             " & \multicolumn{1}{c}{(12)}"                                   ///
             " \\ "                                                          ///
             " \midrule")                                                    ///
    title("The Effect of the Credit Shock on"                                ///
    "Political Preferences: Outcome as Dictionary Scores")                   ///
    d(*) noobs prefoot("") postfoot("") nonumbers

* rescale coefficients and standard errors 100
loc opts star(* 0.1 ** 0.05 *** 0.01) b(3) se(3) booktabs                    ///
         varlab(cbk_past_mean_post "$ Exposure_{k}\ \times \ Post $")        ///
         nogaps nomtitles substitute(\_ _)


loc kvar k(cbk_past_mean_post)

loc panProt prehead(" \\ "                                                   ///
                    "\multicolumn{13}{l}{\textit{Panel A}: "                 ///
                    "Parliamentary Debates}"                                 ///
                    " \\ \\")                                                ///
posthead("") prefoot("") postfoot("") noobs nonumbers

loc panMani prehead(" \\ "                                                   ///
                    "\multicolumn{13}{l}{\textit{Panel B}: "                 ///
                    "Electoral Manifestos}"                                  ///
                    " \\ \\")                                                ///
posthead("") prefoot("") postfoot("") noobs nonumbers

loc panS stat(N N_clust Ymean Dsd r2_within,                                 ///
         labels("Number of Observations"                                     ///
                "Number of Counties"                                         ///
                "Outcome Mean (\%)"                                          ///
                "$ sd\left(Exposure_k\right) $ (\%)"                         ///
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

local foot postfoot("\bottomrule"                                            ///
                    "\end{tabular}"                                          ///
                    "}%"                                                     ///
                    "\caption*{\footnotesize \textit{Notes:}~@note}"         ///
                    "\end{table}")                                           ///
           d(*) noobs nonumbers                                              ///
           prehead("") posthead("") prefoot("")

noi {
    * slda
    di ">>>table>>>rf_text_cbk_past_mean_main_scores.tex>>>"
    esttab prot_bf_slda_kreise                                               ///
           prot_bf_slda_trends                                               ///
           prot_bf_slda_ife                                                  ///
           prot_bf_slda_kife                                                 ///
           prot_pop_slda_kreise                                              ///
           prot_pop_slda_trends                                              ///
           prot_pop_slda_ife                                                 ///
           prot_pop_slda_kife                                                ///
           prot_both_slda_kreise                                             ///
           prot_both_slda_trends                                             ///
           prot_both_slda_ife                                                ///
           prot_both_slda_kife,                                              ///
           `opts' `headMain'

    esttab prot_bf_slda_kreise                                               ///
           prot_bf_slda_trends                                               ///
           prot_bf_slda_ife                                                  ///
           prot_bf_slda_kife                                                 ///
           prot_pop_slda_kreise                                              ///
           prot_pop_slda_trends                                              ///
           prot_pop_slda_ife                                                 ///
           prot_pop_slda_kife                                                ///
           prot_both_slda_kreise                                             ///
           prot_both_slda_trends                                             ///
           prot_both_slda_ife                                                ///
           prot_both_slda_kife,                                              ///
           `opts' `panProt' `kvar'

    esttab prot_bf_slda_kreise                                               ///
           prot_bf_slda_trends                                               ///
           prot_bf_slda_ife                                                  ///
           prot_bf_slda_kife                                                 ///
           prot_pop_slda_kreise                                              ///
           prot_pop_slda_trends                                              ///
           prot_pop_slda_ife                                                 ///
           prot_pop_slda_kife                                                ///
           prot_both_slda_kreise                                             ///
           prot_both_slda_trends                                             ///
           prot_both_slda_ife                                                ///
           prot_both_slda_kife,                                              ///
           `opts' `panS'

    esttab mani_bf_slda_kreise                                               ///
           mani_bf_slda_trends                                               ///
           mani_bf_slda_ife                                                  ///
           mani_bf_slda_kife                                                 ///
           mani_pop_slda_kreise                                              ///
           mani_pop_slda_trends                                              ///
           mani_pop_slda_ife                                                 ///
           mani_pop_slda_kife                                                ///
           mani_both_slda_kreise                                             ///
           mani_both_slda_trends                                             ///
           mani_both_slda_ife                                                ///
           mani_both_slda_kife,                                              ///
           `opts' `panMani' `kvar'

    esttab mani_bf_slda_kreise                                               ///
           mani_bf_slda_trends                                               ///
           mani_bf_slda_ife                                                  ///
           mani_bf_slda_kife                                                 ///
           mani_pop_slda_kreise                                              ///
           mani_pop_slda_trends                                              ///
           mani_pop_slda_ife                                                 ///
           mani_pop_slda_kife                                                ///
           mani_both_slda_kreise                                             ///
           mani_both_slda_trends                                             ///
           mani_both_slda_ife                                                ///
           mani_both_slda_kife,                                              ///
           `opts' `panS'

    esttab prot_bf_slda_kreise                                               ///
           prot_bf_slda_trends                                               ///
           prot_bf_slda_ife                                                  ///
           prot_bf_slda_kife                                                 ///
           prot_pop_slda_kreise                                              ///
           prot_pop_slda_trends                                              ///
           prot_pop_slda_ife                                                 ///
           prot_pop_slda_kife                                                ///
           prot_both_slda_kreise                                             ///
           prot_both_slda_trends                                             ///
           prot_both_slda_ife                                                ///
           prot_both_slda_kife,                                              ///
            `opts' `panFEF'

    esttab prot_bf_slda_kreise                                               ///
           prot_bf_slda_trends                                               ///
           prot_bf_slda_ife                                                  ///
           prot_bf_slda_kife                                                 ///
           prot_pop_slda_kreise                                              ///
           prot_pop_slda_trends                                              ///
           prot_pop_slda_ife                                                 ///
           prot_pop_slda_kife                                                ///
           prot_both_slda_kreise                                             ///
           prot_both_slda_trends                                             ///
           prot_both_slda_ife                                                ///
           prot_both_slda_kife,                                              ///
           `opts' `foot'                                                     ///
           note("This table shows ... \hl{XXX}.")
     di "<<<table<<<"

* dictionaries
    di ">>>table>>>rf_text_cbk_past_mean_main_dict.tex>>>"
    esttab prot_bf_kreise                                                    ///
           prot_bf_trends                                                    ///
           prot_bf_ife                                                       ///
           prot_bf_kife                                                      ///
           prot_pop_kreise                                                   ///
           prot_pop_trends                                                   ///
           prot_pop_ife                                                      ///
           prot_pop_kife                                                     ///
           prot_both_kreise                                                  ///
           prot_both_trends                                                  ///
           prot_both_ife                                                     ///
           prot_both_kife,                                                   ///
           `opts' `headDict'

    esttab prot_bf_kreise                                                    ///
           prot_bf_trends                                                    ///
           prot_bf_ife                                                       ///
           prot_bf_kife                                                      ///
           prot_pop_kreise                                                   ///
           prot_pop_trends                                                   ///
           prot_pop_ife                                                      ///
           prot_pop_kife                                                     ///
           prot_both_kreise                                                  ///
           prot_both_trends                                                  ///
           prot_both_ife                                                     ///
           prot_both_kife,                                                   ///
           `opts' `panProt' `kvar'

    esttab prot_bf_kreise                                                    ///
           prot_bf_trends                                                    ///
           prot_bf_ife                                                       ///
           prot_bf_kife                                                      ///
           prot_pop_kreise                                                   ///
           prot_pop_trends                                                   ///
           prot_pop_ife                                                      ///
           prot_pop_kife                                                     ///
           prot_both_kreise                                                  ///
           prot_both_trends                                                  ///
           prot_both_ife                                                     ///
           prot_both_kife,                                                   ///
           `opts' `panS'

    esttab mani_bf_kreise                                                    ///
           mani_bf_trends                                                    ///
           mani_bf_ife                                                       ///
           mani_bf_kife                                                      ///
           mani_pop_kreise                                                   ///
           mani_pop_trends                                                   ///
           mani_pop_ife                                                      ///
           mani_pop_kife                                                     ///
           mani_both_kreise                                                  ///
           mani_both_trends                                                  ///
           mani_both_ife                                                     ///
           mani_both_kife,                                                   ///
           `opts' `panMani' `kvar'

    esttab mani_bf_kreise                                                    ///
           mani_bf_trends                                                    ///
           mani_bf_ife                                                       ///
           mani_bf_kife                                                      ///
           mani_pop_kreise                                                   ///
           mani_pop_trends                                                   ///
           mani_pop_ife                                                      ///
           mani_pop_kife                                                     ///
           mani_both_kreise                                                  ///
           mani_both_trends                                                  ///
           mani_both_ife                                                     ///
           mani_both_kife,                                                   ///
           `opts' `panS'

    esttab prot_bf_kreise                                                    ///
           prot_bf_trends                                                    ///
           prot_bf_ife                                                       ///
           prot_bf_kife                                                      ///
           prot_pop_kreise                                                   ///
           prot_pop_trends                                                   ///
           prot_pop_ife                                                      ///
           prot_pop_kife                                                     ///
           prot_both_kreise                                                  ///
           prot_both_trends                                                  ///
           prot_both_ife                                                     ///
           prot_both_kife,                                                   ///
           `opts' `panFEF'

    esttab prot_bf_kreise                                                    ///
           prot_bf_trends                                                    ///
           prot_bf_ife                                                       ///
           prot_bf_kife                                                      ///
           prot_pop_kreise                                                   ///
           prot_pop_trends                                                   ///
           prot_pop_ife                                                      ///
           prot_pop_kife                                                     ///
           prot_both_kreise                                                  ///
           prot_both_trends                                                  ///
           prot_both_ife                                                     ///
           prot_both_kife,                                                   ///
           `opts' `foot'                                                     ///
            note("This table shows ... \hl{XXX}.")
    di "<<<table<<<"
}

********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***