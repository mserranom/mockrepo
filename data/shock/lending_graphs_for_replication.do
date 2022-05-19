twoway ///
(connected lrl_diff_2004 year if bank=="allothers" & year>=2004, lwidth(medthick) msymbol(D) msize(medlarge))  ///
(connected lrl_diff_2004 year if bank=="privothers" & year>=2004, lwidth(medthick) msymbol(O) msize(medlarge)) ///
(connected lrl_diff_2004 year if bank=="commerzbank" & year>=2004, lwidth(medthick) msymbol(X) msize(large)), ///
ytitle("Ln lending stock (relative to 2004)") xtitle("") xlabel(#10) ///
legend(label(1 "All other banks") ///
label(2 "All other commercial banks") ///
label(3 "Commerzbank") order(1 3 2)) ///
graphregion(color(white))
