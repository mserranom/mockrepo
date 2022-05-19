* rapidly visualising sample design using sample size for each wave
* fix it after to harmonise this code in the workflow

set varabbrev off

gl PATH = "/Users/dubidub/Documents/Data/SOEP"
gl TABLES = "/Users/dubidub/Documents/Projects/CreditPopulism/docs/milestones/tables"

* ppathl should include all the SOEP waves with indication of the sample as well

u "${PATH}/ppathl.dta", clear

* change labels language
lab lang EN

* define age
g age = syear - gebjahr

preserve
* select the same sample in the two-way tabulation 
keep if psample > 0 & age >= 16 & syear >= 2000 & syear <= 2016
* keep unique values of sample members within the sub-sample
duplicates drop psample, force
sort psample
* retrieve label values for sample member variable
dec psample, g(samples)
* split by whitespace the label values
split samples
* create label value assignment after substring
forv i = 1/`=_N' {
    loc s = samples2[`i']
    loc vlabs `vlabs' `s' "\multirow{3}{*}{`s'}"
    loc elabs `elabs' `s' "\midrule"
    loc s `i' "`s'"
    loc strnames `strnames' `s'
}
restore

* define new label for the sample member variable
la var psample "Sample Type"
la def samples `strnames'
la val psample samples

loc vlabs `vlabs' Total "\multirow{3}*{\textbf{Total}}"
numlist "2000/2016"
loc cols `r(numlist)' "\textbf{Total}"

qui sum psample if age >= 16 & syear >= 2000 & syear < 2009
sca npre  = r(N)
qui sum psample if age >= 16 & syear >= 2009 & syear <=2016
sca npost = r(N)
qui sum psample if age >= 16 & syear >= 2000 & syear <= 2016
sca spre  = round(npre /r(N) * 100, 0.01) 
sca spost = round(npost/r(N) * 100, 0.01)

estpost tab psample syear if age >= 16 & syear >= 2000 & syear <= 2016, esample
esttab using "${TABLES}/samples.tex",                                        ///
    cell(b(fmt(g)) rowpct(fmt(2) par((\textcolor{orange}{ })))               ///
    colpct(fmt(2) par((\textcolor{darkblue}{ }))))                           ///
    varlabels(`vlabs', elist(`elabs')) coll(none) eqlabels(`cols')           ///
    substitute(" 0" "---" "(\textcolor{orange}{0.00})" "---"                 ///
        "(\textcolor{darkblue}{0.00})" "---")                                ///
        bookt uns noobs nonum nomti f r                                      ///
prehead( "\resizebox{\textwidth}{!}{%"                                       ///
         "\begin{tabular}{cccccccccc|ccccccccc}"                             ///
         "\toprule"                                                          ///
         "\multirow{3}{*}{\textbf{Sample Type}} &"                           ///
         "\multicolumn{17}{c}{\textbf{Survey Year}} & \\"                    ///
         "\cline{2-18}"                                                      ///
         "& \multicolumn{17}{c}{} & \\")                                     ///
prefoot( "\midrule"                                                          ///
         "& \multicolumn{9}{c}{"                                             ///
         "T\textsubscript{1} = 9: \textbf{`=npre' (`=spre')}}"               ///
         "& \multicolumn{8}{c}{"                                             ///
         "T\textsubscript{2} = 8: \textbf{`=npost' (`=spost')}} & \\")       ///
postfoot("\bottomrule"                                                       ///
         "\end{tabular}"                                                     ///
         "}%")
