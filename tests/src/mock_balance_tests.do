gl mydata = "~/Documents/Projects/CreditPopulism/tests/data/soep"

u "${mydata}/final_data", clear

drop if age < 16

g dpost = (wave > 2008)

loc desc_depvars  populist_party political_supporter
loc desc_demo     male
* loc desc_demo     male age age_sq residence_gdr89 married migrant_status
loc desc_educ     voc_educ uni_degree educ_years
loc desc_labo     curr_unempl self_employed in_education retired
loc desc_hh       hh_size hh_children house_owner hh_hasloans hh_disp_inc
* loc desc_kreise   gdp gdp_pc pop pop_density uemprate foreigner east hhinc

* keep only all not missing

noi di ">>>table>>>balance_table.tex>>>"

* Table Header
noi di "\begin{table}[H]"
noi di "\centering"
noi di "\caption{Balance}"
noi di "\label{tab:balance}"
noi di "\resizebox{\textwidth}{!}{%"
noi di "\begin{tabular}{l*{1}{cccc}}"
noi di "\toprule"
noi di "& \multicolumn{1}{c}{(1)} & " _c
noi di "\multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{T-test}\\"
noi di "& \multicolumn{1}{c}{(Pre Credit Shock)} & " _c
noi di "& \multicolumn{1}{c}{(Post Credit Shock) & \multicolumn{1}{c}{P-value}}"
noi di "Variable & \multicolumn{1}{c}{Mean/SE} & \multicolumn{1}{c}{Mean/SE}" _c
noi di " & (2) - (1)"

foreach blocks in desc_demo {

    foreach var in ``blocks'' {

        noi di "`lbl_`'var' &" _c

        noi reghdfe `var' if !dpost [pw=crosswgt], a(wave kkz_rek) vce(cl kkz_rek)
        mat block = r(table)[1..2, 1]
        reghdfe `var' if dpost  [pw=crosswgt], a(wave kkz_rek) vce(cl kkz_rek)
        mat block = block , r(table)[1..2, 1]

        noi reghdfe `var' dpost [pw=crosswgt], a(wave kkz_rek) vce(cl kkz_rek)
        loc ptest = r(table)[4, 1]

        noi di "&" string(block[1, 1], "%9.3fc") " & " _c
        noi di     string(block[1, 2], "%9.3fc") " &" _c
        noi di     string(`ptest', "9.3fc") "\\"
        noi di "& (" string(block[2, 1], "%9.3fc") ") & " _c
        noi di   "(" string(block[2, 2], "%9.3fc") ") & \\"

    }

}