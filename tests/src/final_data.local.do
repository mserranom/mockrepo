********************************************************************************
* @project CreditPopulism                                                      *
* @author  Alessandro Pizzigolotto (NHH), Nicolò Fraccaroli (Brown)            *
********************************************************************************

* substitute global variables just here to re-direct on local folders
gl soep36 = "~/Documents/Data/SOEP"
gl mydata = "~/Documents/Projects/CreditPopulism/tests/data/soep"

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
* Create the Individual Dataset                                                *
********************************************************************************

noi di as text "Assemble Single Waves Cleaned Before ... " _c
loc waves     r s t u v w x y z ba bb bc bd be bf bg bh

noi di as text "q ... " _c

u "${mydata}/repeated_cross_wave_q", clear

foreach w of local waves {
    noi di as text "`w' ... " _c
    append using "${mydata}/repeated_cross_wave_`w'"
}

********************************************************************************
* Merge Kreis Level Information                                                *
********************************************************************************

* noi di as text "Rename Household ID and merge Kreise Panel ... " _c
* 
* rename hhnr hid
* 
* mer m:1 hid wave using "${mydata}/kreis_all_waves", gen(merge_kreis)
* soepkeep if merge_kreis >= 3
* soepdrop merge_*

********************************************************************************
* Merge Text-Analysis Output                                                   *
********************************************************************************

* noi di as text "Merge Text Analysis Indexes ... " _c
* 
* loc setnames manifesto_scores                ///
*              manifesto_scores_slda_fakenews  ///
*              manifesto_scores_slda_rooduijn  ///
*              protokolle_scores               ///
*              protokolle_scores_slda_fakenews ///
*              protokolle_scores_slda_rooduijn
* 
* foreach s in `setnames' {
*     mer m:1 supporter_party wave using "${mydata}/`s'", gen(merge_text)
*     soepkeep if merge_text == 1 | merge_text == 3
*     soepdrop merge_*
* }

********************************************************************************
* Closing Commands (Save Temporary Dataset)                                    *
********************************************************************************

compress
save "${mydata}/final_data", replace

noi di as text "Done!"

***