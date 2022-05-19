* mock example with the fake data
gl mydata = "~/Documents/Projects/CreditPopulism/tests/data/soep"

u "${mydata}/final_data", clear

drop if age < 16

* outcome variables
loc dep_vars political_supporter populist_party

* time-invariant individual controls
loc controls_c_ind male age age_sq residence_gdr89

* full controls without time-invariant individual controls
loc full_controls  curr_unempl self_employed in_education retired educ_years hh_size hh_children house_owner hh_hasloans ln_hh_disp_inc

g cbk_past_mean_p50 = (federal_state > 6)

foreach stat in p50 {

    reghdfe populist_party `controls_c_ind' `full_controls' if wave < 2009, a(federal_state wave) vce(cl federal_state) res(pp_pre)
    reghdfe populist_party `controls_c_ind' `full_controls' if wave >= 2009, a (federal_state wave) vce(cl federal_state) res(pp_post)

    su wave, mean
    loc minyear = r(min)
    loc maxyear = r(max)

    forval w = `minyear'/`maxyear' {

        foreach d in 0 1 {

            mat addw = (`w', `d')
            mat coln addw = wave treat

            if (`w' < 2009) mean pp_pre  if cbk_past_mean_`stat' == `d' & wave == `w' [pw=crosswgt]
            else            mean pp_post if cbk_past_mean_`stat' == `d' & wave == `w' [pw=crosswgt]

            if (`w' == `minyear') mat res_`d' = r(table)', addw
            else                  mat res_`d' = res_`d' \ (r(table)', addw)

        } // binary treatment

    } // waves

    mat results = res_0 \ res_1

}

qui {

********************************************************************************
* Outcome Trajectories                                                         *
********************************************************************************

noi di "Mean Outcome Trajectories ... "

foreach out of varlist `dep_vars' {

    noi di "Iterating `out' ... "

    if ("`out'" == "political_supporter") loc estn ps
    else                                  loc estn pp

    foreach stat in p50 {

        noi di "Treatment: `stat'"

        reghdfe populist_party `controls_c_ind'                              ///
            `full_controls' [pw=crosswgt], a(federal_state wave)             ///
            vce(cl federal_state) res(`estn'_res_`stat'_noife)

        reghdfe populist_party `full_controls' [pw=crosswgt],                ///
            a(federal_state wave) vce(cl federal_state)                      ///
            res(`estn'_res_`stat'_ife)

        levelsof wave, l(levels)

        foreach i in noife ife {

            foreach w in `levels' {

                foreach d in 0 1 {

                    mat addw = (`w', `d')
                    mat coln addw = wave treat
                    mean `estn'_res_`stat'_`i' if ///
                        wave == `w' & cbk_past_mean_`stat' == `d' [pw=crosswgt]

                    if (`w' == 2000) mat res_`d' = r(table)', addw
                    else             mat res_`d' = res_`d' \ (r(table)' , addw)

                } // binary treatment

            } // waves

            mat results = res_0 \ res_1

            preserve
            clear
            svmat results, n(col)

            noi di ">>>data>>>`estn'_mean_outcome_cbk_past_mean_`stat'_`i'.csv"
            noi di "b,se,t,pvalue,ll,ul,df,crit,wave,treat"
            forval n = 1/`=_N' {
                noi di string(b[`n']) ","      ///
                       string(se[`n']) ","     ///
                       string(t[`n']) ","      ///
                       string(pvalue[`n']) "," ///
                       string(ll[`n']) ","     ///
                       string(ul[`n']) ","     ///
                       string(df[`n']) ","     ///
                       string(crit[`n']) ","   ///
                       wave[`n'] ","           ///
                       treat[`n']
            }
            noi di "<<<data<<<"
            restore

        } // feselection

    } // cutoffs

} // outcomes

*** Not working
********************************************************************************
* Outcome Trajectories                                                         *
********************************************************************************

noi di "Mean Outcome Trajectories ... "

foreach out of varlist `dep_vars' {

    noi di "Iterating `out' ... "

    if ("`out'" == "political_supporter") loc estn ps
    else                                  loc estn pp

    foreach stat in p50 {
    * foreach stat in p50 p75 p90 iqr 1090 {

        noi di "Treatment: `stat'"

        reghdfe populist_party `controls_c_ind'                              ///
            `full_controls' [pw=crosswgt] if wave < 2009,                    ///
            a(kkz_rek wave) vce(cl kkz_rek)                                  ///
            res(`estn'_res_`stat'_noife_pre)

        reghdfe populist_party `controls_c_ind'                              ///
            `full_controls' [pw=crosswgt] if wave >= 2009,                   ///
            a(kkz_rek wave) vce(cl kkz_rek)                                  ///
            res(`estn'_res_`stat'_noife_post)

        reghdfe populist_party                                               ///
            `full_controls' [pw=crosswgt] if wave < 2009,                    ///
            a(kkz_rek wave) vce(cl kkz_rek) res(`estn'_res_`stat'_ife_pre)

        reghdfe populist_party                                               ///
            `full_controls' [pw=crosswgt] if wave >= 2009,                   ///
            a(kkz_rek wave) vce(cl kkz_rek) res(`estn'_res_`stat'_ife_post)

        su wave, mean
        loc minyear = r(min)
        loc maxyear = r(max)

        foreach i in noife ife {

            forval w = `minyear'/`maxyear' {

                foreach d in 0 1 {

                    mat addw = (`w', `d')
                    mat coln addw = wave treat

                    if (`w' < 2009) mean `estn'_res_`stat'_`i'_pre if ///
                        wave == `w' & cbk_past_mean_`stat' == `d' [pw=crosswgt]
                    else mean `estn'_res_`stat'_`i'_post if ///
                        wave == `w' & cbk_past_mean_`stat' == `d' [pw=crosswgt]

                    if (`w' == `minyear') mat res_`d' = r(table)', addw
                    else mat res_`d' = res_`d' \ (r(table)', addw)

                } // binary treatment

            } // waves

            mat results = res_0 \ res_1

            preserve
            clear

            svmat results, n(col)
            noi di ">>>data>>>`estn'_mean_outcome_cbk_past_mean_`stat'_`i'.csv"
            noi di "b,se,t,pvalue,ll,ul,df,crit,wave,treat"

            forval n = 1/`=_N' {
                noi di string(b[`n']) ","      ///
                       string(se[`n']) ","     ///
                       string(t[`n']) ","      ///
                       string(pvalue[`n']) "," ///
                       string(ll[`n']) ","     ///
                       string(ul[`n']) ","     ///
                       string(df[`n']) ","     ///
                       string(crit[`n']) ","   ///
                       wave[`n'] ","           ///
                       treat[`n']
            }

            noi di "<<<data<<<"
            restore

        } // iFEselection

    } // cutoffs

} // outcomes



********************************************************************************
* Before-After Design                                                          *
********************************************************************************

noi di "Before-After Design ... "

foreach out of varlist `dep_vars' {

    noi di "Iterating `out' ... "

    if ("`out'" == "political_supporter") loc estn ps
    else                                  loc estn pp

    foreach stat in p50 {

        foreach i in noife ife {

            noi di "Treatment: `stat' - `i'"

            if ("`i'" == "noife") {

                reghdfe `out' `controls_c_ind' `full_controls' [pw=crosswgt] ///
                    if wave < 2009 & !missing(cbk_past_mean_`stat'),         ///
                    a(federal_state wave) vce(cl federal_state)              ///
                    res(`estn'_res_`i'_pre)

                reghdfe `out' `controls_c_ind' `full_controls' [pw=crosswgt] ///
                    if wave > 2008 & !missing(cbk_past_mean_`stat'),         ///
                    a(federal_state wave) vce(cl federal_state)              ///
                    res(`estn'_res_`i'_post)

            } // NOindividualFE

            else {

                reghdfe `out' `full_controls' [pw=crosswgt]                  ///
                    if wave < 2009 & !missing(cbk_past_mean_`stat'),         ///
                    a(persnr federal_state wave) vce(cl federal_state)       ///
                    res(`estn'_res_`i'_pre)

                reghdfe `out' `full_controls' [pw=crosswgt]                  ///
                    if wave > 2008 & !missing(cbk_past_mean_`stat'),         ///
                    a(persnr federal_state wave) vce(cl federal_state)       ///
                    res(`estn'_res_`i'_post)

            } // YESindividualFE

            foreach d in 0 1 {

                mean `estn'_res_`i'_pre ///
                    if cbk_past_mean_`stat' == `d' [pw=crosswgt]
                mat  pool_pre_`d'  = r(table)'

                mean `estn'_res_`i'_post ///
                    if cbk_past_mean_`stat' == `d' [pw=crosswgt]
                mat  pool_post_`d' = r(table)'

            }

            mat pool_mean_pre  = pool_pre_0 \ pool_pre_1
            mat col_ids = (0, 0 \ 0, 1)
            mat coln col_ids = period treat
            mat pool_mean_pre = pool_mean_pre , col_ids
            mat pool_mean_post = pool_post_0 \ pool_post_1
            mat col_ids = (1, 0 \ 1, 1)
            mat coln col_ids = period treat
            mat pool_mean_post = pool_mean_post , col_ids

            mat results = pool_mean_pre \ pool_mean_post

            preserve
            clear

            svmat results, n(col)
            noi di ">>>data>>>`estn'_did_design_cbk_past_mean_`stat'_`i'.csv"
            noi di "b,se,t,pvalue,ll,ul,df,crit,period,treat"
            forval n = 1/`=_N' {
                noi di string(b[`n']) ","      ///
                       string(se[`n']) ","     ///
                       string(t[`n']) ","      ///
                       string(pvalue[`n']) "," ///
                       string(ll[`n']) ","     ///
                       string(ul[`n']) ","     ///
                       string(df[`n']) ","     ///
                       string(crit[`n']) ","   ///
                       period[`n'] ","         ///
                       treat[`n']
            }
            noi di "<<<data<<<"
            restore

        } // iFEselection

    } // cutoffs

} // outcomes

} // qui

********************************************************************************
* Before-After Design                                                          *
********************************************************************************

noi di "Before-After Design ... "

foreach out of varlist `dep_vars' {

    noi di "Iterating `out' ... "

    if ("`out'" == "political_supporter") loc estn ps
    else                                  loc estn pp

    * foreach stat in p50 {
    foreach stat in p50 p75 p90 iqr 1090 {

        foreach i in noife ife {

            noi di "Treatment: `stat' - `i'"

            if ("`i'" == "noife") {

                reghdfe `out' `controls_c_ind' `full_controls' [pw=crosswgt] ///
                    if wave < 2009 & !missing(cbk_past_mean_`stat'),         ///
                    a(kkz_rek wave) vce(cl kkz_rek)                          ///
                    res(`estn'_res_`stat'_`i'_pre)

                reghdfe `out' `controls_c_ind' `full_controls' [pw=crosswgt] ///
                    if wave > 2008 & !missing(cbk_past_mean_`stat'),         ///
                    a(kkz_rek wave) vce(cl kkz_rek)                          ///
                    res(`estn'_res_`stat'_`i'_post)

            } // NOindividualFE

            else {

                reghdfe `out' `full_controls' [pw=crosswgt]                  ///
                    if wave < 2009 & !missing(cbk_past_mean_`stat'),         ///
                    a(persnr kkz_rek wave) vce(cl kkz_rek)                   ///
                    res(`estn'_res_`stat'_`i'_pre)

                reghdfe `out' `full_controls' [pw=crosswgt]                  ///
                    if wave > 2008 & !missing(cbk_past_mean_`stat'),         ///
                    a(persnr kkz_rek wave) vce(cl kkz_rek)                   ///
                    res(`estn'_res_`stat'_`i'_post)

            } // YESindividualFE

            foreach d in 0 1 {

                mean `estn'_res_`stat'_`i'_pre ///
                    if cbk_past_mean_`stat' == `d' [pw=crosswgt]
                mat  pool_pre_`d'  = r(table)'

                mean `estn'_res_`stat'_`i'_post ///
                    if cbk_past_mean_`stat' == `d' [pw=crosswgt]
                mat  pool_post_`d' = r(table)'

            }

            mat pool_mean_pre  = pool_pre_0 \ pool_pre_1
            mat col_ids = (0, 0 \ 0, 1)
            mat coln col_ids = period treat
            mat pool_mean_pre = pool_mean_pre , col_ids
            mat pool_mean_post = pool_post_0 \ pool_post_1
            mat col_ids = (1, 0 \ 1, 1)
            mat coln col_ids = period treat
            mat pool_mean_post = pool_mean_post , col_ids

            mat results = pool_mean_pre \ pool_mean_post

            preserve
            clear

            svmat results, n(col)
            noi di ">>>data>>>`estn'_did_design_cbk_past_mean_`stat'_`i'.csv"
            noi di "b,se,t,pvalue,ll,ul,df,crit,period,treat"

            forval n = 1/`=_N' {
                noi di string(b[`n']) ","      ///
                       string(se[`n']) ","     ///
                       string(t[`n']) ","      ///
                       string(pvalue[`n']) "," ///
                       string(ll[`n']) ","     ///
                       string(ul[`n']) ","     ///
                       string(df[`n']) ","     ///
                       string(crit[`n']) ","   ///
                       period[`n'] ","         ///
                       treat[`n']
            }

            noi di "<<<data<<<"
            restore

        } // iFEselection

    } // cutoffs

} // outcomes


********************************************************************************

*** Code Chunk Before-After Design that not sure it is a good idea

********************************************************************************
* Before-After Design                                                          *
********************************************************************************

noi di "Before-After Design ... "

foreach out of varlist `dep_vars' {

    noi di "Iterating `out' ... "

    if ("`out'" == "political_supporter") loc estn ps
    else                                  loc estn pp

    * foreach stat in p50 {
    foreach stat in p50 p75 p90 iqr 1090 {

        foreach i in noife ife {

            noi di "Treatment: `stat' - `i'"

            if ("`i'" == "noife") {

                reghdfe `out' `controls_c_ind' `full_controls' [pw=crosswgt] ///
                    if wave < 2009 & !missing(cbk_past_mean_`stat'),         ///
                    a(kkz_rek wave) vce(cl kkz_rek)                          ///
                    res(`estn'_res_`stat'_`i'_pre)

                reghdfe `out' `controls_c_ind' `full_controls' [pw=crosswgt] ///
                    if wave > 2008 & !missing(cbk_past_mean_`stat'),         ///
                    a(kkz_rek wave) vce(cl kkz_rek)                          ///
                    res(`estn'_res_`stat'_`i'_post)

            } // NOindividualFE

            else {

                reghdfe `out' `full_controls' [pw=crosswgt]                  ///
                    if wave < 2009 & !missing(cbk_past_mean_`stat'),         ///
                    a(persnr kkz_rek wave) vce(cl kkz_rek)                   ///
                    res(`estn'_res_`stat'_`i'_pre)

                reghdfe `out' `full_controls' [pw=crosswgt]                  ///
                    if wave > 2008 & !missing(cbk_past_mean_`stat'),         ///
                    a(persnr kkz_rek wave) vce(cl kkz_rek)                   ///
                    res(`estn'_res_`stat'_`i'_post)

            } // YESindividualFE

            foreach d in 0 1 {

                mean `estn'_res_`stat'_`i'_pre ///
                    if cbk_past_mean_`stat' == `d' [pw=crosswgt]
                mat  pool_pre_`d'  = r(table)'

                mean `estn'_res_`stat'_`i'_post ///
                    if cbk_past_mean_`stat' == `d' [pw=crosswgt]
                mat  pool_post_`d' = r(table)'

            }

            mat pool_mean_pre  = pool_pre_0 \ pool_pre_1
            mat col_ids = (0, 0 \ 0, 1)
            mat coln col_ids = period treat
            mat pool_mean_pre = pool_mean_pre , col_ids
            mat pool_mean_post = pool_post_0 \ pool_post_1
            mat col_ids = (1, 0 \ 1, 1)
            mat coln col_ids = period treat
            mat pool_mean_post = pool_mean_post , col_ids

            mat results = pool_mean_pre \ pool_mean_post

            preserve
            clear

            svmat results, n(col)
            noi di ">>>data>>>`estn'_did_design_cbk_past_mean_`stat'_`i'.csv"
            noi di "b,se,t,pvalue,ll,ul,df,crit,period,treat"

            forval n = 1/`=_N' {
                noi di string(b[`n']) ","      ///
                       string(se[`n']) ","     ///
                       string(t[`n']) ","      ///
                       string(pvalue[`n']) "," ///
                       string(ll[`n']) ","     ///
                       string(ul[`n']) ","     ///
                       string(df[`n']) ","     ///
                       string(crit[`n']) ","   ///
                       period[`n'] ","         ///
                       treat[`n']
            }

            noi di "<<<data<<<"
            restore

        } // iFEselection

    } // cutoffs

} // outcomes