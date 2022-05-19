

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
* Merge Kreise Level Data with Macro Variables from GENESIS and the            *
* County-Level Commerzbank Dependence.                                         *
********************************************************************************

noi di as text "Merge Kreise Data with County Macrodata ... " _c

* load kreis longitudinal dataset and exclude household without id
u "${soep36}/regionl", clear
soepdrop if missing(hid) | hid == -2

* remove individuals with missing kreis recoded codes
replace kkz_rek = . if kkz_rek < 0
soepdrop if missing(kkz_rek)

* remove movers between pre-post
* bys hid : g aux1 = kkz_rek if syear == 2006
* bys hid : g aux2 = kkz_rek if syear == 2007
* bys hid : egen tmp_kkz_2006 = total(aux1)
* bys hid : egen tmp_kkz_2007 = total(aux2)
* soepdrop if tmp_kkz_2006 != tmp_kkz_2007
* soepdrop aux* tmp*

* clean-up missing values in the macro variables
loc macrosoep = "kr_area kr_emprate kr_uemprate kr_foreigner kr_hhinc kr_gdp_pc bula"
foreach var of varlist `macrosoep' {
    replace `var' = . if `var' < 0
}

mer 1:m hid syear using ${soep36}/pequiv, ///
    keepus(y11101) gen(merge_pequiv)
soepkeep if merge_pequiv >= 3

g cpi = y11101
replace cpi = . if cpi < 0

collapse (mean) kr_area kr_emprate kr_uemprate kr_foreigner kr_hhinc         ///
                kr_gdp_pc bula kkz kkz_rek cpi, by(hid syear)

lab var hid       "Current Household ID"
lab var syear     "Survey Year"
lab var cpi       "Consumer Price Index (2016 = 100)"
lab var kkz_rek   "Recoded District Code"

lab var kr_area   "Area (km2)"
lab var kr_emprate  "Employment Rate, Annual Average"
lab var kr_uemprate "Unemployment Rate, Annual Average"
lab var kr_foreigner "Share of Foreigners"

* merge macro data with kreise recoded codes
mer m:1 kkz_rek syear using "${mydata}/macro_variables", ///
    keepus(kkz_rek pop gdp ausl gdp_each urban) gen(merge_macro)
mer m:1 kkz_rek using "${mydata}/county_exposure", gen(merge_treat)
soepkeep if merge_macro >= 3 & merge_treat >= 3

lab var urban         "Urban District"
lab var cbk_mean      "County-Level Commerzbank Dependence (Current Sample)"
lab var cbk_past_mean "County-Level Commerzbank Dependence (Past Sample)"

********************************************************************************
* Create Control Variables at District Level                                   *
********************************************************************************

noi di as text "Generate Control Variables at District Level ... " _c

* dummy variable : district of former gdr
g east = 0
* five bundeslander added at reunification
replace east = 1 if inrange(bula, 12, 16)
* berlin-ost district
replace east = 1 if kkz == 11200
lab var east "District in Former GDR Area"

**********************************************************************
* Dummy Variable for Lander where the Landesbanks suffered the same  *
* crisis of Commerzbank, from Puri, Rocholl and Steffen (2011).      *
**********************************************************************
g       crisis_landesbank = 0
replace crisis_landesbank = 1 if inlist(bula, 5, 9, 14)
lab var crisis_landesbank "Landkreis in Crisis"

* adjust monetary terms to inflation (2016 constant prices)
replace gdp   = (gdp / cpi) * 100
lab var gdp   "Regional GDP at Constant Prices (2016)"
replace kr_hhinc = (kr_hhinc / cpi) * 100
lab var kr_hhinc "Regional Household Income at Constant Prices (2016), EUR"

* regional gdp per capita
replace kr_gdp_pc = (kr_gdp_pc / cpi) * 100 * 1000
lab var kr_gdp_pc "Regional GDP Per Capita at Constant Prices (SOEP, EUR)"
replace gdp_each  = (gdp_each / cpi) * 100
lab var gdp_each  "Regional GDP Per Capita at Constant Prices (ext, EUR)"

* population density
g pop_density = pop / kr_area
lab var pop_density "Population Density"

* filling ausl
replace ausl = kr_foreigner if missing(ausl)

* logvalues
g ln_pop       = ln(pop)
g ln_gdp       = ln(gdp)
g ln_emprate   = ln(kr_emprate)
g ln_uemprate  = ln(kr_uemprate)
g ln_pop_d     = ln(pop_density)
g ln_gdp_pc    = ln(kr_gdp_pc)
g ln_gdp_each  = ln(gdp_each)
g ln_hhinc     = ln(kr_hhinc)
g ln_foreigner = ln(kr_foreigner)

* labels logvalues
lab var ausl         "Share of Foreigners"
lab var pop          "Population"
lab var ln_hhinc     "ln Regional Household Income"
lab var ln_pop       "ln Population"
lab var ln_gdp       "ln GDP"
lab var ln_emprate   "ln Employment Rate"
lab var ln_uemprate  "ln Unemployment Rate"
lab var ln_pop_d     "ln Population Density"
lab var ln_gdp_pc    "ln GDP Per Capita"
lab var ln_foreigner "ln Share of Foreigners"

rename kr_area      area
rename kr_gdp_pc    gdp_pc
rename kr_emprate   emprate
rename kr_uemprate  uemprate
rename kr_hhinc     hhinc
rename kr_foreigner foreigner

********************************************************************************
* Restrict Sample Years                                                        *
********************************************************************************

noi di as text "Restrict Sample Between 2000 and 2017 ... " _c

soepdrop if syear < 2000 | syear > 2017

soepkeep hid syear kkz_rek cbk_* area emprate uemprate ln_*emprate foreigner ///
         ln_foreigner hhinc ln_hhinc gdp* ln_gdp* *pop*                      ///
         urban ausl east crisis_landesbank

* rename survey year in wave for merging
rename syear wave

********************************************************************************
* Closing Commands (Save Temporary Dataset)                                    *
********************************************************************************

compress
save "${mydata}/kreis_all_waves", replace

noi di as text "Done!"

} // qui

***