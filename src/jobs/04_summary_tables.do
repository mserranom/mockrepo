

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

loc desc_demo     male birth_year age residence_gdr89 married migrant_status
loc desc_educ     voc_educ uni_degree educ_years
loc desc_labo     curr_unempl is_in_wa is_in_lf self_employed in_education ///
                  retired egp contr_weekly_hours months_ue labgro
loc desc_hh       hh_size hh_children house_owner hh_hasloans hh_disp_inc
loc desc_kreise   gdp_resc gdp_pc pop_resc pop_density uemprate hhinc ///
                  foreigner east
loc desc_interest cbk_past_mean
loc desc_depvars  populist_party political_supporter ///
                  protokolle_bf_rood_slda protokolle_pop_rood_slda

********************************************************************************
* Define Variable Titles                                                       *
********************************************************************************

* Demographics
loc lbl_male               "Male"
loc lbl_birth_year         "Birth Year"
loc lbl_age                "Age"
loc lbl_residence_gdr89    "Residence in GDR in 1989"
loc lbl_married            "Married"
loc lbl_migrant_status     "Direct/Indirect Migrant"
* Education 
loc lbl_voc_educ           "Vocational Degree or Higher"
loc lbl_uni_degree         "University Degree"
loc lbl_educ_years         "Years of Education"
* Occupational Status
loc lbl_curr_unempl        "Currently Unemployed"
loc lbl_is_in_wa           "In Working Age"
loc lbl_is_in_lf           "In Labour Force"
loc lbl_self_employed      "Self-Employed"
loc lbl_in_education       "In Education"
loc lbl_retired            "Retired"
loc lbl_egp                "EGP Score (Job Prestige Scale)"
loc lbl_contr_weekly_hours "Contractual Working Hours per Week"
loc lbl_months_ue          "Officially Unemployed Prev. Yr. No. Months"
loc lbl_labgro             "Monthly Gross Earnings (in 2016 EUR)"
* Household
loc lbl_hh_size            "Household Size"
loc lbl_hh_children        "Number of Children in HH"
loc lbl_house_owner        "Home-Ownership"
loc lbl_hh_hasloans        "Presence of Outstanding Loans"
loc lbl_hh_disp_inc        "Annual Household Disposable Income (in 2016 EUR)"
* Kreise
loc lbl_gdp_resc            "County GDP (in 2016 mln EUR)"
loc lbl_gdp_pc              "County GDP per capita (in 2016 EUR)"
loc lbl_pop_resc            "Population (1000 units)"
loc lbl_pop_density         "Population Density (units/km2)"
loc lbl_uemprate            "Unemployment Rate"
loc lbl_foreigner           "Share of Foreigners"
loc lbl_east                "County of Former GDR"
loc lbl_hhinc               "Average Household Income (in 2016 EUR)"
* Variables of Interest
loc lbl_cbk_past_mean       "County-Level Commerzbank Exposure"

* Outcomes
loc lbl_political_supporter      "Political Supporter"
loc lbl_populist_party           "Intention to Vote for Populist Party"
loc lbl_protokolle_bf_rood_slda  "Banking and Financial Crisis Index (sLDA)"
loc lbl_protokolle_pop_rood_slda "Populism Index (sLDA)"

********************************************************************************
* Open Dataset                                                                 *
********************************************************************************

u "${mydata}/final_data", clear

* exclude individuals younger than 16
soepdrop if age < 16

* rescale gdp
g gdp_resc = gdp / 1000000
* rescale pop
g pop_resc = pop / 1000

replace protokolle_bf_rood_slda   = protokolle_bf_rood_slda * 100
replace protokolle_pop_rood_slda  = protokolle_pop_rood_slda * 100

********************************************************************************
* Summary Statistics                                                           *
********************************************************************************

loc count = 1

noi di ">>>table>>>summary_statistics.tex>>>"

* Table Header
noi di "\begin{table}[H]"
noi di "\centering"
noi di "\caption{Summary Statistics}"
noi di "\label{tab:summary_statistics}"
noi di "\resizebox{\textwidth}{!}{%"
noi di "\begin{tabular}{l*{1}{rrrrrr}}"
noi di "\toprule"

foreach h in Mean SD Median Min Max "Non-Missing Obs." {
    noi di " & \multicolumn{1}{c}{`h'}" _c
} // Centered Header

noi di "\\"

foreach vars in desc_demo desc_educ desc_labo desc_hh ///
    desc_kreise desc_interest desc_depvars {

        noi di "\midrule"
        noi di "\multicolumn{7}{l}{\textbf" _c

        if ("`vars'" == "desc_demo") {
            noi di "{Panel~A: Demographic Variables}" _c
        }
        if ("`vars'" == "desc_educ") {
            noi di "{Panel~B: Education}" _c
        }
        if ("`vars'" == "desc_labo") {
            noi di "{Panel~C: Occupational Status}" _c
        }
        if ("`vars'" == "desc_hh") {
            noi di "{Panel~D: Household Variables}" _c
        }
        if ("`vars'" == "desc_kreise") {
            noi di "{Panel~E: County-Level Variables}" _c
        }
        if ("`vars'" == "desc_depvars") {
            noi di "{Panel~F: Outcome Variables}" _c
        }
        if ("`vars'" == "desc_interest") {
            * noi di "{Panel~G: Variable of Interest and IV}" _c
            noi di "{Panel~G: Variable of Interest}" _c
        }

        noi di "}\\"

        if ("`vars'" == "desc_kreise"  | "`vars'" == "desc_interest") {
            preserve
            collapse (mean) ``vars'', by(kkz_rek wave)
            * Calculate Summary Statistics
            tabstat ``vars'', s(mean sd p50 min max N) c(s) save
            mat mat_`count' = r(StatTotal)'
        }

        else {
            * Calculate Summary Statistics with weights
            tabstat ``vars'' [aw=crosswgt], s(mean sd p50 min max N) c(s) save
            mat mat_`count' = r(StatTotal)'
        }

        * Iterate Over Variables
        forval r = 1/`=rowsof(mat_`count')' {
            * Row Title
            noi di "`lbl_`=word("`: rownames mat_`count''", `r')'' "
            * Statistics
            forval c = 1/6 {
                local comma = 3 * (`c' < 6)
                noi di " & " ///
                    string(mat_`count'[`r',`c'], "%9.`comma'fc") _c
            }

            noi di "\\"
        
        } // Statistics

        if ("`vars'" == "desc_kreise"  | "`vars'" == "desc_interest") {
            restore
        }

        loc count = `count' + 1

    } // Groups

* Table Footer
noi di "\bottomrule"
noi di "\end{tabular}"
noi di "}%"
noi di "\caption*{\textit{Notes:} This table shows ... \hl{XXX}.}"
noi di "\end{table}"

noi di "<<<table<<<"

********************************************************************************
* Closing Commands                                                             *
********************************************************************************

} // qui

***