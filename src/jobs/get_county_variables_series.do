

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
* Export Macro Variables Series for Counties                                   *
********************************************************************************

loc macro_vars gdp_pc uemprate foreigner
loc depvars    political_supporter populist_party
u "${mydata}/final_data", clear

preserve
collapse (mean) `macro_vars', by(kkz_rek wave)
order kkz_rek wave `macro_vars'
noi di ">>>data>>>soep_county_macro_series.csv"
noi di "kkz_rek,wave,gdp_pc,uemprate,foreigner"
forval n = 1/`=_N' {
    noi di string(kkz_rek[`n']) ","          ///
           string(wave[`n']) ","             ///
           string(gdp_pc[`n']) ","           ///
           string(uemprate[`n']) ","         ///
           string(foreigner[`n'])
}
noi di "<<<data<<<"
restore

********************************************************************************
* Export Outcome Variables Series for Counties                                 *
********************************************************************************

preserve
collapse (mean) `depvars' [pw=crosswgt], by(kkz_rek wave)
order kkz_rek wave `depvars'
noi di ">>>data>>>soep_county_outcomes_series.csv"
noi di "kkz_rek,wave,political_supporter,populist_party"
forval n = 1/`=_N' {
    noi di string(kkz_rek[`n']) ","          ///
           string(wave[`n']) ","             ///
           string(populist_support[`n']) "," ///
           string(populist_party[`n'])
}
noi di "<<<data<<<"
restore

********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***