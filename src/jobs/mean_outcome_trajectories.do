

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
* loc dep_vars       populist_party

* time-invariant individual controls
loc controls_c_ind male age age_sq residence_gdr89 married migrant_status

* full controls without time-invariant individual controls
loc full_controls  curr_unempl self_employed in_education retired            ///
                   educ_years hh_size hh_children house_owner hh_hasloans    ///
                   ln_hh_disp_inc ln_foreigner ln_pop

********************************************************************************
* Open Dataset and Treatment Interactions                                      *
********************************************************************************

noi di "Open Dataset and Binary Treatment ..."

u "${mydata}/final_data", clear

* exclude individuals younger than 16
soepdrop if age < 16

su cbk_past_mean [aw=crosswgt] if !missing(cbk_past_mean), d

foreach s in p50 p75 p90 {
    g cbk_past_mean_`s' = (cbk_past_mean > `r(`s')') if !missing(cbk_past_mean)
} // stats

g cbk_past_mean_iqr = 0 if ///
    cbk_past_mean <= `r(p25)' & !missing(cbk_past_mean)
replace cbk_past_mean_iqr = 1 if ///
    cbk_past_mean > `r(p75)' & !missing(cbk_past_mean)
g cbk_past_mean_1090 = 0 if ///
    cbk_past_mean <= `r(p10)' & !missing(cbk_past_mean)
replace cbk_past_mean_1090 = 1 if ///
    cbk_past_mean > `r(p90)' & !missing(cbk_past_mean)

********************************************************************************
* Outcome Trajectories                                                         *
********************************************************************************

noi di "Mean Outcome Trajectories ... "

tab wave, g(in)
unab ydummies : in1-in8 in10-in18
* unab ydummies: in? in??

foreach out of varlist `dep_vars' {

    noi di "Iterating `out' ... "

    if ("`out'" == "political_supporter") loc estn ps
    else                                  loc estn pp

    foreach stat in p50 {
    * foreach stat in p50 p75 p90 iqr 1090 {

        noi di "Treatment: `stat'"

        reghdfe `out' `controls_c_ind' `full_controls' ///
            [pw=crosswgt], a(kkz_rek wave) vce(cl kkz_rek) ///
            res(`estn'_res_`stat'_noife)

        reghdfe `out' `full_controls' ///
            [pw=crosswgt], a(persnr kkz_rek wave) vce(cl kkz_rek) ///
            res(`estn'_res_`stat'_ife)

        foreach i in noife ife {

            foreach d in 0 1 {
                reg `estn'_res_`stat'_`i' `ydummies' ///
                   if cbk_past_mean_`stat' == `d', vce(cl kkz_rek)
                mat results_`d' = r(table)'
                mat results_`d' = results_`d'[1..17, 1...]
                mat addt = J(`=rowsof(results_`d')', 1, `d')
                mat coln addt = treat
                mat results_`d' = results_`d' , addt
            }

            mat results = results_0 \ results_1

            preserve
            clear

            svmat results, n(col)
            bys treat : g wave = _n - 1 + 2000
            replace wave = wave + 1 if wave > 2007
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
* Closing Commands                                                             *
********************************************************************************

} // qui

***