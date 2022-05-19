

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

loc dep_vars        political_supporter populist_party
loc controls_base   male age age_sq                                          ///
                    curr_unempl self_employed in_education retired           ///
                    educ_years
loc controls_hh     hh_size hh_children                                      ///
                    house_owner hh_hasloans ln_hh_disp_inc
loc controls_kreise ln_pop ln_gdp foreigner

loc first_lag = (2009 - 2000 + 1) - 1

********************************************************************************
* Open Dataset and Treatment Interactions                                      *
********************************************************************************

u "${mydata}/final_data", clear

* exclude individuals younger than 16
soepdrop if age < 16

* Generate After-Shock Dummy Variable 2009 onwards
g dpost = (wave > 2008)
g dpre =  (wave < 2008)

unab treatment_vars : cbk_*

tab wave, g(in)
unab totalvar : in? in??

foreach treat of varlist `treatment_vars' {

    su `treat' [aw=crosswgt] if !missing(`treat'), d
    g  `treat'_std = (`treat' / `r(sd)')

    g `treat'_iqr  = 0 if `treat' <= `r(p25)' & !missing(`treat')
    replace `treat'_iqr  = 1 if `treat'  > `r(p75)' & !missing(`treat')
    g `treat'_1090 = 0 if `treat' <= `r(p10)' & !missing(`treat')
    replace `treat'_1090 = 1 if `treat'  > `r(p90)' & !missing(`treat')

    g `treat'_std_pre   = `treat'_std * dpre
    g `treat'_std_post  = `treat'_std * dpost
    g `treat'_iqr_pre   = `treat'_iqr * dpre
    g `treat'_iqr_post  = `treat'_iqr * dpost
    g `treat'_1090_pre  = `treat'_1090 * dpre
    g `treat'_1090_post = `treat'_1090 * dpost

    foreach var of varlist `totalvar' {

        if ("`var'" != "in`first_lag'") {
            g `var'_`treat'_std  = `var' * `treat'_std
            g `var'_`treat'_iqr  = `var' * `treat'_iqr
            g `var'_`treat'_1090 = `var' * `treat'_1090
        }

    } // interactions

    foreach stat in p50 p75 p90 {

        g `treat'_`stat'      = (`treat' > `r(`stat')') if !missing(`treat')
        g `treat'_`stat'_pre  = `treat'_`stat' * dpre
        g `treat'_`stat'_post = `treat'_`stat' * dpost

        foreach var of varlist `totalvar' {
            if ("`var'" != "in`first_lag'") {
                g `var'_`treat'_`stat' = `var' * `treat'_`stat'
            }
        } // interactions

    } // stats

} // treatment_vars

********************************************************************************
* Reduced Form TWFE Indicator Variables from Statistics                        *
********************************************************************************

local opts star(* 0.1 ** 0.05 *** 0.01) b(3) se(3) booktabs                  ///
           varlab(exposure "$ D_{k}\ \times \ Post $")                       ///
           transform(@*100 100) nogaps nomtitles substitute(\_ _)

local kvar k(exposure)

local panS stat(N r2_a N_clust,                             ///
        labels("Number of Observations"                     ///
                "Adjusted $ R $-Squared"                    ///
                "Number of Counties")                       ///
        fmt(%9.0fc %9.3fc %9.0fc))                          ///
        prehead("\midrule") posthead("") nonumbers          ///
        prefoot("\\") postfoot("")

local panFEF stat(kFE yFE iFE iCtl hCtl rCtl,                   ///
                labels("County-Level FE"                        ///
                    "Wave FE"                                   ///
                    "Individual FE"                             ///
                    "Individual Controls"                       ///
                    "Household Controls"                        ///
                    "Regional Controls"))                       ///
                d(*) prehead("") posthead("\midrule") nonumbers ///
                prefoot("") postfoot("")

    local foot postfoot("\bottomrule"                                        ///
                        "\end{tabular}"                                      ///
                        "}%"                                                 ///
                        "\caption*{\footnotesize \textit{Notes:}~@note}"     ///
                        "\end{table}")                                       ///
                d(*) noobs nonumbers                                         ///
                prehead("") posthead("") prefoot("")

foreach treat of varlist `treatment_vars' {

    foreach out of varlist `dep_vars' {

        if ("`out'" == "political_supporter") loc estname ps
        else                                  loc estname pp

        loc cards 

        foreach stat in p50 p75 p90 iqr 1090 {

            noi reghdfe `out' `treat'_`stat'_post ///
                `controls_base' `controls_hh' `controls_kreise' ///
                [pw=crosswgt], absorb(kkz_rek wave) vce(cluster kkz_rek)
            estadd loc kFE  Yes
            estadd loc yFE  Yes
            estadd loc iFE  No
            estadd loc iCtl Yes
            estadd loc hCtl Yes
            estadd loc rCtl Yes
            estimates store `estname'_`stat'

            loc cards `cards' `treat'_`stat'_post exposure

        }

    }

    local headPS prehead("\begin{table}"                                     ///
                         "\centering"                                        ///
                         "\caption{@title}"                                  ///
                         "\label{tab:rf_ps_dummy_`treat'}"                   ///
                         "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"        ///
                         "\resizebox{\textwidth}{!}{%"                       ///
                         "\begin{tabular}{l*{1}ccccc}"                       ///
                         "\toprule")                                         ///
        posthead(" & \multicolumn{5}{c}{\textbf{Intention to Vote "          ///
                 "for Populist Party}}"                                      ///
                 " \\ "                                                      ///
                 " & \multicolumn{1}{c}{\textbf{Median}}"                    ///
                 " & \multicolumn{1}{c}{\textbf{75\textsuperscript{th}}}"    ///
                 " & \multicolumn{1}{c}{\textbf{90\textsuperscript{th}}}"    ///
                 " & \multicolumn{1}{c}{\textbf{25\textsuperscript{th}"      ///
                 "-- 75\textsuperscript{th}}}"                               ///
                 " & \multicolumn{1}{c}{\textbf{10\textsuperscript{th}"      ///
                 "-- 90\textsuperscript{th}}}"                               ///
                 " \\ ")                                                     ///
        title("The Effect of the Credit Shock"                               ///
              "on Political Preferences: Binary Treatment")                  ///
        d(*) noobs prefoot("") postfoot("") nonumbers

    local headPP prehead("\begin{table}"                                     ///
                         "\centering"                                        ///
                         "\caption{@title}"                                  ///
                         "\label{tab:rf_pp_dummy_`treat'}"                   ///
                         "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"        ///
                         "\resizebox{\textwidth}{!}{%"                       ///
                         "\begin{tabular}{l*{1}ccccc}"                       ///
                         "\toprule")                                         ///
        posthead(" & \multicolumn{5}{c}{\textbf{Intention to Vote "          ///
                 "for Populist Party}}"                                      ///
                 " \\ "                                                      ///
                 " & \multicolumn{1}{c}{\textbf{Median}}"                    ///
                 " & \multicolumn{1}{c}{\textbf{75\textsuperscript{th}}}"    ///
                 " & \multicolumn{1}{c}{\textbf{90\textsuperscript{th}}}"    ///
                 " & \multicolumn{1}{c}{\textbf{25\textsuperscript{th}"      ///
                 "-- 75\textsuperscript{th}}}"                               ///
                 " & \multicolumn{1}{c}{\textbf{10\textsuperscript{th}"      ///
                 "-- 90\textsuperscript{th}}}"                               ///
                 " \\ ")                                                     ///
        title("The Effect of the Credit Shock"                               ///
                "on Political Preferences: Binary Treatment")                ///
        d(*) noobs prefoot("") postfoot("") nonumbers

    noi {

        di ">>>table>>>rf_ps_`treat'_dummies.tex>>>"
        esttab ps_p50                                                        ///
               ps_p75                                                        ///
               ps_p90                                                        ///
               ps_iqr                                                        ///
               ps_1090,                                                      ///
               rename(`cards') `opts' `headF'
        esttab ps_p50                                                        ///
               ps_p75                                                        ///
               ps_p90                                                        ///
               ps_iqr                                                        ///
               ps_1090,                                                      ///
               rename(`cards') `opts' `panS' `kvar'
        esttab ps_p50                                                        ///
               ps_p75                                                        ///
               ps_p90                                                        ///
               ps_iqr                                                        ///
               ps_1090,                                                      ///
               rename(`cards') `opts' `panFEF'
        esttab ps_p50                                                        ///
               ps_p75                                                        ///
               ps_p90                                                        ///
               ps_iqr                                                        ///
               ps_1090,                                                      ///
               rename(`cards') `opts' `foot'                                 ///
               note("This table shows ... \hl{XXX}.")
        di "<<<table<<<"

        di ">>>table>>>rf_pp_`treat'_dummies.tex>>>"
        esttab pp_p50                                                        ///
               pp_p75                                                        ///
               pp_p90                                                        ///
               pp_iqr                                                        ///
               pp_1090,                                                      ///
               rename(`cards') `opts' `headF'
        esttab pp_p50                                                        ///
               pp_p75                                                        ///
               pp_p90                                                        ///
               pp_iqr                                                        ///
               pp_1090,                                                      ///
               rename(`cards') `opts' `panS' `kvar'
        esttab pp_p50                                                        ///
               pp_p75                                                        ///
               pp_p90                                                        ///
               pp_iqr                                                        ///
               pp_1090,                                                      ///
               rename(`cards') `opts' `panFEF'
        esttab pp_p50                                                        ///
               pp_p75                                                        ///
               pp_p90                                                        ///
               pp_iqr                                                        ///
               pp_1090,                                                      ///
               rename(`cards') `opts' `foot'                                 ///
               note("This table shows ... \hl{XXX}.")
        di "<<<table<<<"

    } // noi

} // treatment_vars


********************************************************************************
* Event-Study Plots                                                            *
********************************************************************************

foreach treat of varlist `treatment_vars' {

    foreach out of varlist `dep_vars' {

        foreach stat in std p50 p75 p90 iqr 1090 {

            unab interaction : in*_`treat'_`stat'

            noi reghdfe `out' `treat'_`stat'_pre `treat'_`stat'_post         ///
                `controls_base' `controls_hh' `controls_kreise'              ///
                [pw=crosswgt], absorb(kkz_rek wave) vce(cluster kkz_rek)

            mat results_pool = r(table)'
            mat pool_pre     = results_pool[1, 1...]
            mat pool_post    = results_pool[2, 1...]

            noi reghdfe `out' `interaction'                                  ///
                `controls_base' `controls_hh' `controls_kreise'              ///
                [pw=crosswgt], absorb(kkz_rek wave) vce(cluster kkz_rek)

            mat results_interaction = r(table)'
            mat results_interaction = results_interaction[1..16, 1...]
            mat results_interaction = results_interaction \ pool_pre
            mat results_interaction = results_interaction \ pool_post

            preserve
            clear

            svmat results_interaction, n(col)
            g wave = _n - 1 + 2000
            replace wave = wave + 1 if wave > 2007
            replace wave = 0 if wave == 2018
            replace wave = 1 if wave == 2019
            noi di ">>>data>>>event_study_plot_`stat'.csv"
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

        } // dummies

    } // outcomes

} // treatment_vars

********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***