

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

********************************************************************************
* Open Dataset and Treatment Creation                                          *
********************************************************************************

u "${mydata}/final_data", clear

* exclude individuals younger than 16
soepdrop if age < 16

* Generate After-Shock Dummy Variable 2009 onwards
g dpost = (wave > 2008)

unab treatment_vars : cbk_*

foreach treat of varlist `treatment_vars' {
    * exposure interacted with indicator variable
    g `treat'_post = `treat' * dpost
    * standardized treatment
    su `treat' [aw=crosswgt] if !missing(`treat')
    g `treat'_post_std = (`treat' / `r(sd)') * dpost
}

********************************************************************************
* TWFE Reduced Form Tables                                                     *
********************************************************************************

unab treatment_vars : cbk_*_post_std

local panS stat(N r2_a N_clust,                                              ///
                labels("Number of Observations"                              ///
                        "Adjusted $ R $-Squared"                             ///
                        "Number of Counties")                                ///
                fmt(%9.0fc %9.3fc %9.0fc))                                   ///
            prehead("\midrule") posthead("") nonumbers                       ///
            prefoot("\\") postfoot("")

local panFEF stat(kFE yFE iFE iCtl hCtl rCtl,                                ///
                labels("County-Level FE"                                     ///
                       "Wave FE"                                             ///
                       "Individual FE"                                       ///
                       "Individual Controls"                                 ///
                       "Household Controls"                                  ///
                       "Regional Controls"))                                 ///
                d(*) prehead("") posthead("\midrule") nonumbers              ///
                prefoot("") postfoot("")

local foot postfoot("\bottomrule"                                            ///
                    "\end{tabular}"                                          ///
                    "}%"                                                     ///
                    "\caption*{\footnotesize \textit{Notes:}~@note}"         ///
                    "\end{table}")                                           ///
            d(*) noobs nonumbers                                             ///
            prehead("") posthead("") prefoot("")

foreach treat of varlist `treatment_vars' {

    foreach out of varlist `dep_vars' {

        if ("`out'" == "political_supporter") loc estname ps
        else                                  loc estname pp

        * TWFE continuous treatment nocontrols
        noi reghdfe `out' `treat' [pw=crosswgt],                             ///
            absorb(kkz_rek wave) vce(cluster kkz_rek)
        estadd loc kFE  Yes
        estadd loc yFE  Yes
        estadd loc iFE  No
        estadd loc iCtl No
        estadd loc hCtl No
        estadd loc rCtl No
        estimates store `estname'_base

        * TWFE continuous treatment individual controls
        noi reghdfe `out' `treat'                                            ///
            `controls_base' [pw=crosswgt],                                   ///
            absorb(kkz_rek wave) vce(cluster kkz_rek)
        estadd loc kFE  Yes
        estadd loc yFE  Yes
        estadd loc iFE  No
        estadd loc iCtl Yes
        estadd loc hCtl No
        estadd loc rCtl No
        estimates store `estname'_ind

        * TWFE continuous treatment household controls
        noi reghdfe `out' `treat'                                            ///
            `controls_base' `controls_hh' [pw=crosswgt],                     ///
            absorb(kkz_rek wave) vce(cluster kkz_rek)
        estadd loc kFE  Yes
        estadd loc yFE  Yes
        estadd loc iFE  No
        estadd loc iCtl Yes
        estadd loc hCtl Yes
        estadd loc rCtl No
        estimates store `estname'_hh

        * TWFE continuous treatment district controls
        noi reghdfe `out' `treat'                                            ///
            `controls_base' `controls_hh' `controls_kreise' [pw=crosswgt],   ///
            absorb(kkz_rek wave) vce(cluster kkz_rek)
        estadd loc kFE  Yes
        estadd loc yFE  Yes
        estadd loc iFE  No
        estadd loc iCtl Yes
        estadd loc hCtl Yes
        estadd loc rCtl Yes
        estimates store `estname'_kreise

        * TWFE continuous treatment individual FE
        noi reghdfe populist_party `treat'                                   ///
            `controls_base' `controls_hh' `controls_kreise' [pw=crosswgt],   ///
            absorb(persnr kkz_rek wave) vce(cluster kkz_rek)
        estadd loc kFE  Yes
        estadd loc yFE  Yes
        estadd loc iFE  Yes
        estadd loc iCtl Yes
        estadd loc hCtl Yes
        estadd loc rCtl Yes
        estimates store `estname'_full

    } // outcomes

    local headPS prehead("\begin{table}"                                     ///
                         "\centering"                                        ///
                         "\caption{@title}"                                  ///
                         "\label{tab:rf_ps_`treat'}"                         ///
                         "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"        ///
                         "\resizebox{\textwidth}{!}{%"                       ///
                         "\begin{tabular}{l*{1}ccccc}"                       ///
                         "\toprule")                                         ///
        posthead(" & \multicolumn{5}{c}{\textbf{Political Support}}"         ///
                 " \\ "                                                      ///
                 " & \multicolumn{1}{c}{(1)}"                                ///
                 " & \multicolumn{1}{c}{(2)}"                                ///
                 " & \multicolumn{1}{c}{(3)}"                                ///
                 " & \multicolumn{1}{c}{(4)}"                                ///
                 " & \multicolumn{1}{c}{(5)}"                                ///
                 " \\ ")                                                     ///
        title("The Effect of the Credit Shock"                               ///
              "on Political Preferences: Baseline Results")                  ///
        d(*) noobs prefoot("") postfoot("") nonumbers

    local headPP prehead("\begin{table}"                                     ///
                         "\centering"                                        ///
                         "\caption{@title}"                                  ///
                         "\label{tab:rf_pp_`treat'}"                         ///
                         "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}"        ///
                         "\resizebox{\textwidth}{!}{%"                       ///
                         "\begin{tabular}{l*{1}ccccc}"                       ///
                         "\toprule")                                         ///
        posthead(" & \multicolumn{5}{c}{\textbf{Intention to Vote "          ///
                 "for Populist Party}}"                                      ///
                 " \\ "                                                      ///
                 " & \multicolumn{1}{c}{(1)}"                                ///
                 " & \multicolumn{1}{c}{(2)}"                                ///
                 " & \multicolumn{1}{c}{(3)}"                                ///
                 " & \multicolumn{1}{c}{(4)}"                                ///
                 " & \multicolumn{1}{c}{(5)}"                                ///
                 " \\ ")                                                     ///
        title("The Effect of the Credit Shock"                               ///
                "on Political Preferences: Baseline Results")                ///
        d(*) noobs prefoot("") postfoot("") nonumbers

    * rescale coefficients and standard errors 100
    local opts star(* 0.1 ** 0.05 *** 0.01) b(3) se(3) booktabs              ///
               varlab(`treat' "$ Exposure_{k}\ \times \ Post $")             ///
               transform(@*100 100) nogaps nomtitles substitute(\_ _)

    local kvar k(`treat')

    noi {

        di ">>>table>>>ps_rf_`treat'.tex>>>"
        esttab ps_base                                                       ///
            ps_ind                                                           ///
            ps_hh                                                            ///
            ps_kreise                                                        ///
            ps_full,                                                         ///
            `opts' `headPS'
        esttab ps_base                                                       ///
            ps_ind                                                           ///
            ps_hh                                                            ///
            ps_kreise                                                        ///
            ps_full,                                                         ///
            `opts' `panS' `kvar'
        esttab ps_base                                                       ///
            ps_ind                                                           ///
            ps_hh                                                            ///
            ps_kreise                                                        ///
            ps_full,                                                         ///
            `opts' `panFEF'
        esttab ps_base                                                       ///
            ps_ind                                                           ///
            ps_hh                                                            ///
            ps_kreise                                                        ///
            ps_full,                                                         ///
            `opts' `foot'                                                    ///
            note("This table shows ... \hl{XXX}.")
        di "<<<table<<<"

        di ">>>table>>>pp_rf_`treat'.tex>>>"
        esttab pp_base                                                       ///
            pp_ind                                                           ///
            pp_hh                                                            ///
            pp_kreise                                                        ///
            pp_full,                                                         ///
            `opts' `headPP'
        esttab pp_base                                                       ///
            pp_ind                                                           ///
            pp_hh                                                            ///
            pp_kreise                                                        ///
            pp_full,                                                         ///
            `opts' `panS' `kvar'
        esttab pp_base                                                       ///
            pp_ind                                                           ///
            pp_hh                                                            ///
            pp_kreise                                                        ///
            pp_full,                                                         ///
            `opts' `panFEF'
        esttab pp_base                                                       ///
            pp_ind                                                           ///
            pp_hh                                                            ///
            pp_kreise                                                        ///
            pp_full,                                                         ///
            `opts' `foot'                                                    ///
            note("This table shows ... \hl{XXX}.")
        di "<<<table<<<"

    } // noi

} // treatment

********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***