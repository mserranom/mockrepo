////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////// This do-file preprocesses the raw BvD Amadeus ///////
////////// dataset and defines key outcome and control   ///////
////////// variables.                                    ///////
//////////                                               ///////
////////// Table of contents:                            ///////
////////// ------------------                            ///////
//////////                                               ///////
////////// 1. Preprocess raw BvD AMADEUS data            ///////
////////// 	1.1 Drops                                    ///////
////////// 	1.2 Adjust region                            ///////
////////// 	1.3 Firm-bank lending relationships          ///////
////////// 	1.4 Controls and fixed effects               ///////
////////// 2. Collapse dataset (2008-12 employ. growth)  ///////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

* set path (insert your path here; required for the code to run)
global 		path  "../Replication Package"

* additional functions required to run the code:
* (commented out, only need to be installed once)
*ssc 		install keeporder
*ssc 		install unique
*ssc 		install egenmore
*ssc 		install winsor2

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////// *** 1. Preprocess raw BvD AMADEUS data *** //////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

* base dataset:		BvD AMADEUS Financials, downloaded via WRDS on 18 September 2018
* restrictions: 	country = Germany, period = 2007-2018

* note:				This file is not contained in the replication package,
*					as the database is a commercial subscription. BvD AMADEUS
*					data is available to subscribers via WRDS or direct web
*					interface (https://amadeus.bvdinfo.com/).

*					The file needs to be placed in the "../Replication Package/Data/Amadeus/" 
*					folder for the code to run.

* -> dataset huge, load only relevant variables
use			idnr repbas dateinc closdate_year		///
			nace_prim_code zipcode region cash		///
			toas empl staf ncli culi				///
			using "$path/Data/Amadeus/All_Amadeus_DE_Sep_2018.dta", clear		
compress

////////////////////////////////////////////////////////////////
/////// *** 1.1 Drops *** //////////////////////////////////////
////////////////////////////////////////////////////////////////

* remove observations with missing employment (main outcome variable)
count		// 8,159,910
drop		if empl == . // -3,029,553

* remove observations with missing counties (main spillover level)
drop		if region == "" // -2,576

* remove financial and public sector firms
* see e.g. Klapper, Laeven, and Rajan (JFE 2006) but translated to NACE Rev. 2
destring	nace_prim_code, replace
drop		if nace_prim_code == . 								// missing industry; -2,484
drop		if inrange(floor(nace_prim_code/100),64,70) 		// financials and related; -1,310,912
drop		if inlist(floor(nace_prim_code/100),81,82,84) 		// support & administrative activities, public administration; -227,365
drop		if inlist(floor(nace_prim_code/100),85) 			// education; -45,926
drop		if inlist(floor(nace_prim_code/100),86,87,88) 		// healthcare and social; -121,952
drop		if inlist(floor(nace_prim_code/100),90,91,92) 		// performing arts, culture, gambling; -24,678
drop		if inlist(floor(nace_prim_code/100),94,97,98,99) 	// organisations & households; -83,534
														
* de-duplicate (ensure that firm x year uniquely identifies an observation)
unique 		idnr closdate_year
* number of unique values of idnr closdate_year is 3,300,547
* number of records is 3,310,930
gen			consolidated = repbas == "Consolidated data"
bys			idnr: egen has_consolidated = max(consolidated)
drop		if has_consolidated == 1 & consolidated == 0 // drop unconsolidated statement if consolidated available
isid		idnr closdate_year
drop		*consolidated repbas

////////////////////////////////////////////////////////////////
/////// *** 1.2 Adjust region *** //////////////////////////////
////////////////////////////////////////////////////////////////

* destring zip code
assert 		region != ""
destring 	zipcode, replace

* ensure consistent spelling
replace		region = "Hamburg  Freie und Hansestadt" if region == "Hamburg"

* drop if region = Bundesland and no zip code available
* (region defined as entire federal state but cannot be corrected using the zip code)
* (only adjustments for "Flaechenlaender", i.e., not for Berlin/Bremen/Hamburg)
drop		if region == "Baden-Wuerttemberg"		& zipcode == .
drop		if region == "Bayern"					& zipcode == .
drop		if region == "Brandenburg"				& zipcode == .
drop		if region == "Hessen"					& zipcode == .
drop		if region == "Niedersachsen"			& zipcode == .
drop		if region == "Mecklenburg-Vorpommern"	& zipcode == .
drop		if region == "Rheinland-Pfalz"			& zipcode == .
drop		if region == "Nordrhein-Westfalen"		& zipcode == .
drop		if region == "Saarland"					& zipcode == .
drop		if region == "Sachsen"					& zipcode == .
drop		if region == "Sachsen-Anhalt"			& zipcode == .
drop		if region == "Schleswig-Holstein"		& zipcode == .
drop		if region == "Thueringen"				& zipcode == . // total: -864

* set region to missing if region = Bundesland ("Flaechenland") but zip code is available
replace		region = ""		if region == "Baden-Wuerttemberg"
replace		region = ""		if region == "Bayern"
replace		region = ""		if region == "Brandenburg"
replace		region = ""		if region == "Hessen"
replace		region = ""		if region == "Niedersachsen"
replace		region = ""		if region == "Mecklenburg-Vorpommern"
replace		region = ""		if region == "Rheinland-Pfalz"
replace		region = ""		if region == "Nordrhein-Westfalen"
replace		region = ""		if region == "Saarland"
replace		region = ""		if region == "Sachsen"
replace		region = ""		if region == "Sachsen-Anhalt"
replace		region = ""		if region == "Schleswig-Holstein"
replace		region = ""		if region == "Thueringen"

* assign region of other firms in the same 5-digit zip code if region missing
gsort		zipcode -region
by			zipcode: replace region = region[_n-1] if region == "" & region[_n-1] != "" & zipcode != .

* use 4-digit zip code for regions that are still unassigned
gen			zipcode4 = floor(zipcode/10)
gsort		zipcode4 -region
by			zipcode4: replace region = region[_n-1] if region == "" & region[_n-1] != "" & zipcode4 != .
drop		zipcode4

* manually assign remaining regions by zip code
replace		region = "Anhalt-Bitterfeld"		if region == "" & zipcode == 6354
replace		region = "Anhalt-Bitterfeld"		if region == "" & zipcode == 6731
replace		region = "Anhalt-Bitterfeld"		if region == "" & zipcode == 6733
replace		region = "Anhalt-Bitterfeld"		if region == "" & zipcode == 6734
replace		region = "Anhalt-Bitterfeld"		if region == "" & zipcode == 6756
replace		region = "Dessau-Rosslau"			if region == "" & zipcode == 6812
replace		region = "Wittenberg"				if region == "" & zipcode == 6879
replace		region = "Oder-Spree"				if region == "" & zipcode == 15501
replace		region = "Goettingen"				if region == "" & zipcode == 34346
replace		region = "Goettingen"				if region == "" & zipcode == 37073
replace		region = "Goettingen"				if region == "" & zipcode == 37075
replace		region = "Goettingen"				if region == "" & zipcode == 37077
replace		region = "Goettingen"				if region == "" & zipcode == 37079
replace		region = "Goettingen"				if region == "" & zipcode == 37081
replace		region = "Goettingen"				if region == "" & zipcode == 37083
replace		region = "Goettingen"				if region == "" & zipcode == 37085
replace		region = "Goettingen"				if region == "" & zipcode == 37115
replace		region = "Goettingen"				if region == "" & zipcode == 37120
replace		region = "Goettingen"				if region == "" & zipcode == 37124
replace		region = "Goettingen"				if region == "" & zipcode == 37127
replace		region = "Goettingen"				if region == "" & zipcode == 37130
replace		region = "Goettingen"				if region == "" & zipcode == 37133
replace		region = "Goettingen"				if region == "" & zipcode == 37136
replace		region = "Goettingen"				if region == "" & zipcode == 37139
replace		region = "Goettingen"				if region == "" & zipcode == 37412
replace		region = "Goettingen"				if region == "" & zipcode == 37431
replace		region = "Goettingen"				if region == "" & zipcode == 37434
replace		region = "Goettingen"				if region == "" & zipcode == 37520
replace		region = "Goettingen"				if region == "" & zipcode == 37534
replace		region = "Goettingen"				if region == "" & zipcode == 37539
replace		region = "Mettmann"					if region == "" & zipcode == 40703
replace		region = "Steinfurt"				if region == "" & zipcode == 49486
replace		region = "Leverkusen"				if region == "" & zipcode == 51368
replace		region = "Wetteraukreis"			if region == "" & zipcode == 61150
replace		region = "Traunstein"				if region == "" & zipcode == 83291
replace		region = "Garmisch-Partenkirchen"	if region == "" & zipcode == 82475
assert 		region != "" // each obs has an assigned region

////////////////////////////////////////////////////////////////
/////// *** 1.3 Firm-bank lending relationships *** ////////////
////////////////////////////////////////////////////////////////

* number of firms (pre bank relationship match)
unique		idnr // 870,788

* add information on banking relationships
preserve
	// BvD AMADEUS Bankers dataset, downloaded via WRDS on 19 September 2018
	// restriction: country = germany
	use			"$path/Data/Amadeus/All_Amadeus_Banker_Sep_2018.dta", clear
	keep		idnr bnk_name
	isid		idnr bnk_name
	tempfile	Amadeus_Banker
	save		`Amadeus_Banker', replace
restore

* note:				This file is not contained in the replication package,
*					as the database is a commercial subscription. BvD AMADEUS
*					data is available to subscribers via WRDS or direct web
*					interface (https://amadeus.bvdinfo.com/).

*					The file needs to be placed in the "../Replication Package/Data/Amadeus/" 
*					folder for the code to run.

joinby		idnr using `Amadeus_Banker' // restricts dataset to firms with non-missing banker information

* number of firms post bank relationship match
unique		idnr // 621,343 (-249,445 firms w/o bank lending relationship)

* banking relationship with Commerzbank
tab 		bnk_name if regexm(lower(bnk_name),"commerzbank")
replace		bnk_name = "Commerzbank AG" if regexm(lower(bnk_name),"commerzbank") // ensure consistent spelling
duplicates	drop
bys			idnr: egen bnk_num = nvals(bnk_name) // number of bank relatioships
gen			temp = regexm(lower(bnk_name),"commerzbank")
bys			idnr: egen cb_dep_d = max(temp) // banking relationship with Commerzbank indicator

* de-duplicate
drop		bnk_name temp
duplicates	drop
isid		idnr closdate_year
							 
* commerzbank dependence as % of total number of banking relationships
gen			cb_dep = cb_dep_d/bnk_num	
								 
////////////////////////////////////////////////////////////////
/////// *** 1.4 Controls and fixed effects *** /////////////////
////////////////////////////////////////////////////////////////

* firm controls as of 2007
gen			age_07 				= abs(year(dateinc)-2007)+1 // firm age in 2007
assert		age_07 				> 0 if age_07 != .
gen			ln_age_07 			= ln(age_07)
gen			temp				= empl if closdate_year == 2007
bys			idnr: egen empl_07 	= min(temp) // employment in 2007
drop		temp
gen			temp				= toas if closdate_year == 2007
bys			idnr: egen toas_07 	= min(temp) // total assets in 2007
drop		temp
gen			temp				= cash if closdate_year == 2007
bys			idnr: egen cash_07 	= min(cash) // cash holdings in 2007
drop		temp
gen			size_bin_07 		= 1 if inrange(empl_07,1,49) 	& empl_07 != .
replace		size_bin_07 		= 2 if inrange(empl_07,50,249) 	& empl_07 != .
replace		size_bin_07 		= 3 if inrange(empl_07,250,999) & empl_07 != .
replace		size_bin_07 		= 4 if empl_07 >= 1000 			& empl_07 != .

* fixed effects
gen			ind_fe	= floor(nace_prim_code/100)			// industry fixed effects (2-digit NACE)
bys			idnr: 	gen	firm_fe		= _n == 1			// firm fixed effects
replace		firm_fe = sum(firm_fe)
bys			region: gen	region_fe	= _n == 1			// county fixed effects
replace		region_fe = sum(region_fe)

* define and winsorize ratios (used for descriptives only)
foreach		var of varlist toas staf ncli culi cash {
				replace	`var' = `var'/1000 // rescale
			}
gen			wage = (staf/empl)*1000
gen			liab = ncli+culi
gen			leve = liab/toas
winsor2		leve wage, cuts(1 99) replace

* label variables
label		var bnk_num 		"Number of banking relationships"
label		var cb_dep_d 		"Commerzbank borrower (0/1)"
label		var cb_dep 			"Commerzbank indicator / total banking relationships"
label		var age_07 			"Firm age as of 2007"
label		var ln_age_07 		"ln(Firm age as of 2007)"
label		var empl_07 		"Employment as of 2007"
label		var toas_07 		"Total assets as of 2007"
label		var cash_07 		"Cash holdings as of 2007"
label		var size_bin_07 	"Firm size class (1-4) in 2007"
label		var ind_fe 			"Industry fixed effects (2-digit NACE)"
label		var firm_fe 		"Firm fixed effects"
label		var region_fe		"Region fixed effects"
label		var wage 			"Staff expenses / number of employees"
label		var liab 			"Total liabilities"
label		var leve			"Total liabilities / total assets"

* restrict to relevant variables
keeporder	idnr closdate_year zipcode region 					///
			dateinc nace_prim_code toas empl 					///
			staf wage liab leve size_bin_07 age_07 				///
			ln_age_07 empl_07 toas_07 cash_07 bnk_num cb_dep_d	///
			cb_dep ind_fe firm_fe region_fe

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
/// *** 2. Collapse dataset (2008-12 employment growth) *** ////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
	
* restrict sample to relevant years
keep		if inlist(closdate_year,2008,2012)

* 2008-2012 employment growth
bys			idnr (closdate_year): gen empl_g = 2*((empl-empl[_n-1])/(empl+empl[_n-1]))

* base restrictions
keep		if empl_g 		!= . 	// non-missing employment growth. restricts dataset to one obs per firm
keep		if size_bin_07 	!= .	// non-missing 2007 size bucket (i.e. 2007 employment available)
keep		if age_07		!= .	// non-missing firm age
isid		idnr

* average region CB dependence (ex firm i)
bys			region: egen tot_treat 			= total(cb_dep)
bys			region: gen  count_treat_ex_i 	= _N - 1
gen			cb_dep_ic = (tot_treat-cb_dep)/count_treat_ex_i
drop		tot_treat count_treat_ex_i

* indicator: CB dependence >= 50%
gen			cb_dep_d_50		= cb_dep >= 0.50

* average region CB dependence (ex firm i) based on indicators
bys			region: egen tot_treat 			= total(cb_dep_d_50)
bys			region: gen  count_treat_ex_i 	= _N - 1
gen			cb_dep_d_50_ic = (tot_treat-cb_dep_d_50)/count_treat_ex_i
drop		tot_treat count_treat_ex_i

* interaction w/ average CB share in region
gen			cb_depXcb_dep_ic				= cb_dep_ic*cb_dep
gen			inv_cb_depXcb_dep_ic			= cb_dep_ic*abs(cb_dep-1)
gen			cb_dep_d_50Xcb_dep_d_50_ic		= cb_dep_d_50_ic*cb_dep_d_50
gen			inv_cb_dep_d_50Xcb_dep_d_50_ic	= cb_dep_d_50_ic*abs(cb_dep_d_50-1)

label		var empl_g 							"2008-12 employment growth"
label		var cb_dep_ic						"Regional Commerzbank dependence"
label		var cb_dep_d_50						"Commerzbank dependence >= 50%"
label		var cb_dep_d_50_ic					"Regional CB dependence (based on >= 50% indicator)"
label		var cb_depXcb_dep_ic 				"CB dep x avg. CB share in region"
label		var inv_cb_depXcb_dep_ic			"Non-CB dep x avg. CB share in region"
label		var cb_dep_d_50Xcb_dep_d_50_ic		"CB dep (0/1) x avg. CB share in region"
label		var inv_cb_dep_d_50Xcb_dep_d_50_ic	"Non-CB dep (0/1) x avg. CB share in region"

* save master dataset
compress
save		"$path/Data/_Processed/Master dataset.dta", replace

* eof
