

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

********************************************************************************
* TWFE Reduced Form Tables Indicator Variables from Statistics                 *
********************************************************************************

noi di "TWFE Reduced Form Tables with Binary Treatment from Statistics ... "

loc titleps "Political Support"
loc titlepp "Intention to Vote for a Populist Party"

loc opts star(* 0.1 ** 0.05 *** 0.01) b(3) se(3) booktabs                    ///
         varlab(exposure "$ D_{kt} $")                                       ///
         transform(@*100 100) nogaps nomtitles substitute(\_ _)

loc kvar k(exposure)

loc panS stat(N N_clust Ymean Dsplit r2_within,                              ///
         labels("Number of Observations"                                     ///
                "Number of Counties"                                         ///
                "Outcome Mean (\%)"                                          ///
                "Treatment Assignment (\%)"                                  ///
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

    loc head prehead( "\begin{table}[H]"                                     ///
                      "\centering"                                           ///
                      "\caption{@title}"                                     ///
                      "\label{tab:rf_`estn'_cbk_past_mean}"                  ///
                      "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"           ///
                      "\resizebox{\textwidth}{!}{%"                          ///
                      "\begin{tabular}{l*{1}cccccccccc}"                     ///
                      "\toprule")                                            ///
             posthead(" & \multicolumn{2}{c}{\textbf{Median}}"               ///
                      " & \multicolumn{2}{c}{\textbf{75th}}"                 ///
                      " & \multicolumn{2}{c}{\textbf{90th}}"                 ///
                      " & \multicolumn{2}{c}{\textbf{25th -- 75th}}"         ///
                      " & \multicolumn{2}{c}{\textbf{10th -- 90th}}"         ///
                      " \\ "                                                 ///
                      " & \multicolumn{1}{c}{(1)}"                           ///
                      " & \multicolumn{1}{c}{(2)}"                           ///
                      " & \multicolumn{1}{c}{(3)}"                           ///
                      " & \multicolumn{1}{c}{(4)}"                           ///
                      " & \multicolumn{1}{c}{(5)}"                           ///
                      " & \multicolumn{1}{c}{(6)}"                           ///
                      " & \multicolumn{1}{c}{(7)}"                           ///
                      " & \multicolumn{1}{c}{(8)}"                           ///
                      " & \multicolumn{1}{c}{(9)}"                           ///
                      " & \multicolumn{1}{c}{(10)}"                          ///
                      " \\ ")                                                ///
             title(   "The Effect of the Credit Shock"                       ///
                      "on `title`estn'': "                                   ///
                      "Difference-in-Differences with Binary Treatment")     ///
             d(*) noobs prefoot("") postfoot("") nonumbers

    loc cards 

    foreach stat in p50 p75 p90 iqr 1090 {

        noi di "Treatment: `stat' "

        if      ("`stat'" == "iqr")  loc name_stat p25
        else if ("`stat'" == "1090") loc name_stat p10
        else                         loc name_stat `stat'

        su `out' if !missing(`out') & !missing(cbk_past_mean_`stat')         ///
            & !missing(kkz_rek) & !missing(wave)                             ///
            `missing_controls_c_ind' `missing_full_controls' [aw=crosswgt]
        loc outcome_mean = round(`r(mean)' * 100, 0.001)

        su cbk_past_mean if !missing(`out') & !missing(cbk_past_mean)        ///
            & !missing(kkz_rek) & !missing(wave)                             ///
            `missing_controls_c_ind' `missing_full_controls' [aw=crosswgt], d
        loc treat_split = round(`r(`name_stat')' * 100, 0.001)

        * without individualFE
        noi reghdfe `out' cbk_past_mean_`stat'_post                          ///
            `controls_c_ind' `full_controls' [pw=crosswgt],                  ///
            a(kkz_rek wave) vce(cl kkz_rek)
            estadd loc Ymean   `outcome_mean'
            estadd loc Dsplit  `treat_split'
            estadd loc kFE     Yes
            estadd loc yFE     Yes
            estadd loc iFE     No
            estadd loc iCtl    Yes
            estadd loc hCtl    Yes
            estadd loc kCtl    Yes
            estadd loc kTrends No
            estimates store `estn'_`stat'_noife

        su `out' if !missing(`out') & !missing(cbk_past_mean_`stat')         ///
            & !missing(kkz_rek) & !missing(wave)                             ///
            `missing_full_controls' & total_repl > 1 [aw=crosswgt]
        loc outcome_mean = round(`r(mean)' * 100, 0.001)

        su cbk_past_mean if !missing(`out') & !missing(cbk_past_mean)        ///
            & !missing(kkz_rek) & !missing(wave)                             ///
            `missing_full_controls' & total_repl > 1 [aw=crosswgt], d
        loc treat_split = round(`r(`name_stat')' * 100, 0.001)

        * with individualFE
        noi reghdfe `out' cbk_past_mean_`stat'_post                          ///
            `full_controls' [pw=crosswgt],                                   ///
            a(persnr kkz_rek wave) vce(cl kkz_rek)
            estadd loc Ymean   `outcome_mean'
            estadd loc Dsplit  `treat_split'
            estadd loc kFE     Yes
            estadd loc yFE     Yes
            estadd loc iFE     Yes
            estadd loc iCtl    Yes
            estadd loc hCtl    Yes
            estadd loc kCtl    Yes
            estadd loc kTrends No
            estimates store `estn'_`stat'_ife

        loc cards `cards' cbk_past_mean_`stat'_post exposure

    } // dummies

    noi {

        di ">>>table>>>rf_`estn'_cbk_past_mean_dummies.tex>>>"
        esttab `estn'_p50_noife                                              ///
               `estn'_p50_ife                                                ///
               `estn'_p75_noife                                              ///
               `estn'_p75_ife                                                ///
               `estn'_p90_noife                                              ///
               `estn'_p90_ife                                                ///
               `estn'_iqr_noife                                              ///
               `estn'_iqr_ife                                                ///
               `estn'_1090_noife                                             ///
               `estn'_1090_ife,                                              ///
               rename(`cards') `opts' `head'
        esttab `estn'_p50_noife                                              ///
               `estn'_p50_ife                                                ///
               `estn'_p75_noife                                              ///
               `estn'_p75_ife                                                ///
               `estn'_p90_noife                                              ///
               `estn'_p90_ife                                                ///
               `estn'_iqr_noife                                              ///
               `estn'_iqr_ife                                                ///
               `estn'_1090_noife                                             ///
               `estn'_1090_ife,                                              ///
                rename(`cards') `opts' `panS' `kvar'
        esttab `estn'_p50_noife                                              ///
               `estn'_p50_ife                                                ///
               `estn'_p75_noife                                              ///
               `estn'_p75_ife                                                ///
               `estn'_p90_noife                                              ///
               `estn'_p90_ife                                                ///
               `estn'_iqr_noife                                              ///
               `estn'_iqr_ife                                                ///
               `estn'_1090_noife                                             ///
               `estn'_1090_ife,                                              ///
                rename(`cards') `opts' `panFEF'
        esttab `estn'_p50_noife                                              ///
               `estn'_p50_ife                                                ///
               `estn'_p75_noife                                              ///
               `estn'_p75_ife                                                ///
               `estn'_p90_noife                                              ///
               `estn'_p90_ife                                                ///
               `estn'_iqr_noife                                              ///
               `estn'_iqr_ife                                                ///
               `estn'_1090_noife                                             ///
               `estn'_1090_ife,                                              ///
                rename(`cards') `opts' `foot'                                ///
                note("This table shows ... \hl{XXX}.")
        di "<<<table<<<"

    } // noi

} // outcomes

********************************************************************************
* Event-Study Plots                                                            *
********************************************************************************

noi di "Event Study Plots ... "

foreach out of varlist `dep_vars' {

    noi di "Iterating `out' ... "

    if ("`out'" == "political_supporter") loc estn ps
    else                                  loc estn pp

    foreach stat in std p50 p75 p90 iqr 1090 {

        unab interaction : in*_cbk_past_mean_`stat'

        **************************
        ** without individualFE **
        **************************

        reghdfe `out' cbk_past_mean_`stat'_pre cbk_past_mean_`stat'_post     ///
            `controls_c_ind' `full_controls' [pw=crosswgt],                  ///
            a(kkz_rek wave) vce(cl kkz_rek)

        mat results_pool = r(table)'
        mat pool_pre     = results_pool[1, 1...]
        mat pool_post    = results_pool[2, 1...]

        reghdfe `out' `interaction'                                          ///
            `controls_c_ind' `full_controls' [pw=crosswgt],                  ///
            a(kkz_rek wave) vce(cl kkz_rek)

        mat results_interaction = r(table)'
        mat results_interaction = results_interaction[1..17, 1...]
        mat results_interaction = results_interaction \ pool_pre
        mat results_interaction = results_interaction \ pool_post

        preserve
        clear

        svmat results_interaction, n(col)
        g wave = _n - 1 + 2000
        replace wave = wave + 1 if wave > 2007
        replace wave = 0 if wave == 2018
        replace wave = 1 if wave == 2019
        noi di ">>>data>>>dynamic_did_cbk_past_mean_`estn'_`stat'_noife.csv"
        noi di "wave,b,se,t,pvalue,ll,ul,df,crit"
        forval n = 1/`=_N' {
            noi di wave[`n'] ","            ///
                string(b[`n']) ","      ///
                string(se[`n']) ","     ///
                string(t[`n']) ","      ///
                string(pvalue[`n']) "," ///
                string(ll[`n']) ","     ///
                string(ul[`n']) ","     ///
                string(df[`n']) ","     ///
                string(crit[`n'])
        }
        noi di "<<<data<<<"

        restore

        ***********************
        ** with individualFE **
        ***********************

        reghdfe `out' cbk_past_mean_`stat'_pre cbk_past_mean_`stat'_post     ///
            `full_controls' [pw=crosswgt],                  ///
            a(persnr kkz_rek wave) vce(cl kkz_rek)

        mat results_pool = r(table)'
        mat pool_pre     = results_pool[1, 1...]
        mat pool_post    = results_pool[2, 1...]

        reghdfe `out' `interaction' `full_controls' [pw=crosswgt],           ///
            a(persnr kkz_rek wave) vce(cl kkz_rek)

        mat results_interaction = r(table)'
        mat results_interaction = results_interaction[1..17, 1...]
        mat results_interaction = results_interaction \ pool_pre
        mat results_interaction = results_interaction \ pool_post

        preserve
        clear

        svmat results_interaction, n(col)
        g wave = _n - 1 + 2000
        replace wave = wave + 1 if wave > 2007
        replace wave = 0 if wave == 2018
        replace wave = 1 if wave == 2019
        noi di ">>>data>>>dynamic_did_cbk_past_mean_`estn'_`stat'_ife.csv"
        noi di "wave,b,se,t,pvalue,ll,ul,df,crit"
        forval n = 1/`=_N' {
            noi di wave[`n'] ","            ///
                string(b[`n']) ","      ///
                string(se[`n']) ","     ///
                string(t[`n']) ","      ///
                string(pvalue[`n']) "," ///
                string(ll[`n']) ","     ///
                string(ul[`n']) ","     ///
                string(df[`n']) ","     ///
                string(crit[`n'])
        }
        noi di "<<<data<<<"
        restore

    } // cutoffs

} // outcomes

********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***