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
* Retrieve all information from Single Waves of the SOEP                       *
********************************************************************************

* clean and prepare waves 2000-2016
loc waves     q r s t u v w x y z ba bb bc bd be bf bg bh

* indicator for current wave in the iteration
loc current_wave = 2000

* cleanup data wave-by-wave to work faster
noi di as text "Assemble Individual Data from Single Waves ... " _c

foreach w of local waves {
    
    * combine the necessary datasets from the SOEPRemote
    noi di as text "`w' ... " _c

    u "${soep36}/raw/`w'p", clear

    mer 1:1 persnr  using "${soep36}/raw/`w'pgen", gen(merge_pgen)
    mer 1:1 persnr  using "${soep36}/raw/ppfad", gen(merge_pgen2)
    mer 1:1 persnr  using "${soep36}/raw/`w'pequiv", ///
        keepus(w1110* i111* d11107* d11109* y11101*) gen(merge_pequiv)
    mer 1:1 persnr  using "${soep36}/raw/`w'pkal", gen(merge_pkal)
    mer m:1 hhnrakt using "${soep36}/raw/`w'h", gen(merge_h)
    mer m:1 hhnrakt using "${soep36}/raw/`w'hbrutto", gen(merge_hbrutto)
    mer m:1 hhnrakt using "${soep36}/raw/`w'hgen", gen(merge_hgen)
    keep if merge_pgen  >= 3 & merge_pgen2   >= 3 ///
             & merge_pequiv >= 3 & merge_pkal    >= 3 ///
             & merge_h      >= 3 & merge_hbrutto >= 3 & merge_hgen >= 3

    * substring for the last two digits of the wave year
    loc short_wave = substr("`current_wave'", 3, 2)

    * generate cross-sectional weights
    g crosswgt = w11105`short_wave'
    replace crosswgt = . if crosswgt < 0
    lab var crosswgt "Cross-Sectional Weighting Factor"

    * generate wave variable
    g wave = `current_wave'
    lab var wave    "Survey Wave"
    lab var hhnr    "Original Household Number"
    lab var persnr  "Never Changing Person ID"

    * more generic sample identifier
    g sample_group = ""
    replace sample_group = "A"  if sample1 == 1
    replace sample_group = "B"  if inlist(sample1, 2, 3, 4, 5, 6)
    replace sample_group = "C"  if sample1 == 7
    replace sample_group = "D"  if inlist(sample1, 8, 9)
    replace sample_group = "E"  if inlist(sample1, 10, 11)
    replace sample_group = "F"  if inlist(sample1, 12, 13)
    replace sample_group = "G"  if inlist(sample1, 14, 15)
    replace sample_group = "H"  if sample1 == 16
    replace sample_group = "I"  if sample1 == 19
    replace sample_group = "J"  if sample1 == 20
    replace sample_group = "K"  if sample1 == 21
    replace sample_group = "L1" if inlist(sample1, 63, 64, 65, 66)
    replace sample_group = "L2" if inlist(sample1, 60, 61, 62)
    replace sample_group = "M1" if sample1 == 24
    replace sample_group = "M2" if sample1 == 29
    replace sample_group = "M3" if sample1 == 30
    replace sample_group = "M4" if sample1 == 31
    replace sample_group = "M5" if sample1 == 34
    replace sample_group = "N"  if sample1 == 33
    replace sample_group = "O"  if sample1 == 35
    replace sample_group = "P"  if sample1 == 36
    replace sample_group = "Q"  if sample1 == 37
    lab var sample_group "Subsample Identifier (Broad)"

    ************************************************************
    * Step 1: Demographic Variables                            *
    ************************************************************
    
    * dummy variable : gender
    g male = sex
    replace male = . if male <  0
    replace male = 0 if male == 2
    lab var male "Male"
    lab def gender_lbl 0 "Female" 1 "Male"
    lab val male gender_lbl

    * birth year and age (simple method)
    g birth_year = gebjahr
    replace birth_year = . if birth_year < 0
    lab var birth_year "Year of Birth"
    g age    = wave - birth_year
    g age_sq = age^2
    lab var age    "Age"
    lab var age_sq "Age-Squared"

    * federal state of residence
    g federal_state = `w'bula
    lab def fed_lbl 1 "Schleswig Holstein" ///
                    2 "Hamburg" ///
                    3 "Niedersachsen" ///
                    4 "Bremen" ///
                    5 "NRW" ///
                    6 "Hessen" ///
                    7 "Rheinland-Pfalz/Saarland" ///
                    8 "Baden-Wuerttemberg" ///
                    9 "Bayern" ///
                    10 "Saarland" ///
                    11 "Berlin" ///
                    12 "Brandenburg" ///
                    13 "Mecklenburg-Vorpommern" ///
                    14 "Sachsen" ///
                    15 "Sachsen-Anhalt" ///
                    16 "Thueringen"
    lab val federal_state fed_lbl
    replace federal_state = . if federal_state < 0
    lab var federal_state "Federal State of Residence"

    * dummy variable : in former GDR before reunification
    g residence_gdr89 = loc1989
    recode  residence_gdr89 (-2 2 3 = 0) (-1 = .)
    lab var residence_gdr89 "Lived in Former GDR"

    * dummy variable : marital status
    g married = `w'famstd
    replace married = . if married < 0
    replace married = 1 if inlist(married, 6, 7, 8)
    replace married = 0 if inlist(married, 2, 3, 4, 5)
    lab var married "Married"
    lab def marital_status 0 "Not Married" 1 "Married"
    lab val married marital_status

    * categorical variable: migration background
    g migrant_status = migback
    replace migrant_status = . if migrant_status < 0
    recode  migrant_status (1 = 0) (2 3 = 1)
    lab var migrant_status "Direct/Indirect Migrant"

    * codes: country of origin
    g country_origin = corigin
    replace country_origin = . if country_origin < 0
    lab var country_origin "Country of Origin"

    ************************************************************
    * Step 2: Variables on Employment                          *
    ************************************************************

    * categorical variable : current employment
    g employment_status = lfs`short_wave'
    replace employment_status = . if employment_status < 0
    lab var employment_status "Current Employment"

    * categorical variable : current occupational status
    g current_occupation = stib`short_wave'
    replace current_occupation = . if current_occupation < 0
    lab var current_occupation "Current Occupational Status"

    * dummy variable : currently registered as employed
    if      "`w'" == "q"  g curr_unempl = qp04
    else if "`w'" == "r"  g curr_unempl = rp09
    else if "`w'" == "s"  g curr_unempl = sp10
    else if "`w'" == "t"  g curr_unempl = tp13
    else if "`w'" == "u"  g curr_unempl = up05
    else if "`w'" == "v"  g curr_unempl = vp07
    else if "`w'" == "w"  g curr_unempl = wp04
    else if "`w'" == "x"  g curr_unempl = xp10
    else if "`w'" == "y"  g curr_unempl = yp15
    else if "`w'" == "z"  g curr_unempl = zp06
    else if "`w'" == "ba" g curr_unempl = bap06
    else if "`w'" == "bb" g curr_unempl = bbp06
    else if "`w'" == "bc" g curr_unempl = bcp08
    else if "`w'" == "bd" g curr_unempl = bdp15
    else if "`w'" == "be" g curr_unempl = bep09
    else if "`w'" == "bf" g curr_unempl = bfp15
    else if "`w'" == "bg" g curr_unempl = bgp13
    else if "`w'" == "bh" g curr_unempl = bhp_14

    recode  curr_unempl (-1 -2 -3 = .) (2 = 0)
    lab var curr_unempl "Currently Unemployed"

    * unemployment history : officially unemployed prev. yr. no. months
    g months_ue = `w'p1d02
    replace months_ue = 0 if months_ue == -2
    replace months_ue = . if months_ue <   0
    lab var months_ue "No. Months Officially Unemployed Prev. Yr."

    * employment indicator : working-age population?
    g is_in_wa  = inrange(age, 15, 64)
    lab var is_in_wa "In Working Age"

    g was_in_wa = inrange(age, 16, 65) // last year
    lab var was_in_wa "Was In Working Age Last Year"

    * employment indicator : labor force (when working age)
    g is_in_lf = inlist(employment_status, ///
        11, 6, 4, 5, 9, 10, 8, 12) if is_in_wa == 1
    lab var is_in_lf "In Labour Force"
    replace is_in_lf = 0 ///
        if inlist(current_occupation, 10, 13, 15) & is_in_wa == 1

    * employment indicator : in employment (when working age)
    g is_in_emp = inlist(employment_status, 11, 12) if is_in_wa == 1
    lab var is_in_emp "Employed"
    * employment indicator: self-employed (when in employment)
    g self_employed = (inrange(current_occupation, 430, 433) & is_in_emp == 1)
    lab var self_employed "Self-Employed"
    * employment indicator : in education 
    g in_education = (current_occupation == 11 & is_in_emp == 0)
    lab var in_education "In Education"
    * employment indicator : retired
    g retired = (current_occupation == 13 & is_in_emp == 0)
    lab var retired "Retired"

    * restrict (un)employment indicators to working age population
    replace months_ue = . if is_in_wa == 0
    
    * contractual working hours per week
    g contr_weekly_hours = `w'vebzeit
    replace contr_weekly_hours = . if contr_weekly_hours < 0
    lab var contr_weekly_hours "Working Hours Per Week"

    * training according to current occupation
    g job_as_training = erljob`short_wave'
    replace job_as_training = . if job_as_training < 0
    recode  job_as_training (3 4 = .) (2 = 0)
    lab var job_as_training "Training According to Current Occupation"

    * job prestige classification (EGP, the lower the better)
    g egp = egp88_`short_wave'
    replace egp = . if egp < 0
    lab var egp "Job Prestige Classification (EGP Score)"

    ************************************************************
    * Step 3: Variables on Education                           *
    ************************************************************

    * Education according to CASMIN classification
    g education = casmin`short_wave'
    lab var education "Education Level"
    replace education = . if education < 0
    label define educ_lbl 0 "In school"                          ///
                          1 "Inadequately completed"             ///
                          2 "General elementary school"          ///
                          3 "Basic vocational qualification"     ///
                          4 "Intermediate general qualification" ///
                          5 "Intermediate vocational training"   ///
                          6 "General maturity certificate"       ///
                          7 "Vocational maturity certificate"    ///
                          8 "Lower tertiary education"           ///
                          9 "Higher tertiary education"
    label values education educ_lbl

    * dummy variable : vocational qualification or higher
    g       voc_educ = .
    replace voc_educ = 0 if inrange(education, 0, 2)
    replace voc_educ = 1 if inrange(education, 3, 9)
    lab var voc_educ "Vocational Qualification or Higher"

    * dummy variable : tertiary degree
    g       uni_degree = .
    replace uni_degree = 0 if inrange(education, 0, 7)
    replace uni_degree = 1 if inlist(education, 8, 9)
    lab var uni_degree "Tertiary Degree"

    * years of education (CNEF equivalent)
    g educ_years = d11109`short_wave'
    replace educ_years = . if educ_years < 0
    lab var educ_years "Years of Education"

    ************************************************************
    * Step 4: Variables on Income                              *
    ************************************************************

    * consumer price index (constant prices as of 2016)
    g cpi = y11101`short_wave'
    lab var cpi "Consumer Price Index (2016 = 100)"

    * individual monthly gross labor income
    g labgro = labgro`short_wave'
    replace labgro = . if labgro < 0
    * set individual income to zero if missing
    replace labgro = 0 if missing(labgro)
    lab var labgro "Ind. Monthly Gross Labour Income (Constant Prices 2016)"
    * restrict income to working age population
    replace labgro = . if is_in_wa == 0
    * adjust individual income for inflation
    replace labgro = (labgro / cpi) * 100
    g ln_labgro = ln(labgro)
    lab var ln_labgro "ln Monthly Gross Labour Income"

    ************************************************************
    * Step 5: Variables on Political Attitudes (y)             *
    ************************************************************

    * dummy variable : any political interest?
    if      "`w'" == "q"  g political_interest = qp115
    else if "`w'" == "r"  g political_interest = rp110
    else if "`w'" == "s"  g political_interest = sp110
    else if "`w'" == "t"  g political_interest = tp117
    else if "`w'" == "u"  g political_interest = up122
    else if "`w'" == "v"  g political_interest = vp128
    else if "`w'" == "w"  g political_interest = wp118
    else if "`w'" == "x"  g political_interest = xp127
    else if "`w'" == "y"  g political_interest = yp129
    else if "`w'" == "z"  g political_interest = zp122
    else if "`w'" == "ba" g political_interest = bap127
    else if "`w'" == "bb" g political_interest = bbp128
    else if "`w'" == "bc" g political_interest = bcp124
    else if "`w'" == "bd" g political_interest = bdp130
    else if "`w'" == "be" g political_interest = bep118
    else if "`w'" == "bf" g political_interest = bfp143
    else if "`w'" == "bg" g political_interest = bgp143
    else if "`w'" == "bh" g political_interest = bhp_182
    recode  political_interest (-1 -5 = .) (1=4) (2=3) (3=2) (4=1)
    recode  political_interest (1 2 = 0) (3 4 = 1)
    lab var political_interest "Strong Political Interest"

    * dumy variable : any involvement in local politics ? 
         if "`w'" == "r"  g local_politics = rp0308
    else if "`w'" == "t"  g local_politics = tp1405
    else if "`w'" == "v"  g local_politics = vp0308
    else if "`w'" == "x"  g local_politics = xp0308
    else if "`w'" == "y"  g local_politics = yp1806
    else if "`w'" == "z"  g local_politics = zp0308
    else if "`w'" == "bb" g local_politics = bbp0308
    else if "`w'" == "bd" g local_politics = bdp1107
    else if "`w'" == "bf" g local_politics = bfp1108
    else if "`w'" == "bh" g local_politics = bhp_10_08
    else                  g local_politics = .

    if inlist("r", "v", "x", "z", "bb", "bf") ///
        recode local_politics (-1 = .) (4=1) (3=2) (2=3) (1=4)
    if inlist("`w'", "t", "y", "bd") ///
        recode local_politics (-1 = .) (5=1) (4=2) (2 1 = 4)
    recode  local_politics (1 2 = 0) (3 4 = 1)
    replace local_politics = . if local_politics < 0
    lab var local_politics "Strong Participation in Local Politics"

    * categorical variable : voting for political extremes
    if      "`w'" == "v"  g extreme_voter = vp133
    else if "`w'" == "z"  g extreme_voter = zp126
    else if "`w'" == "be" g extreme_voter = bep122
    else if "`w'" == "bj" g extreme_voter = bjp_171
    else                  g extreme_voter = .
    if inlist("`w'", "v", "z", "be") recode extreme_voter (-1 = .)
    lab var extreme_voter "Extreme Voter"
    
    * dummy variable : right-wing extreme (upper 5% of distribution)
    g right_wing = 0
    if inlist("`w'", "v", "z", "be") ///
        replace right_wing = 1 if inrange(extreme_voter, 8, 10)
    replace right_wing = . if extreme_voter == .
    lab var right_wing "Right-Wing Extreme Voter"

    * dummy variable : left-wing extreme
    g left_wing = 0
    if inlist("`w'", "v", "z", "be") ///
        replace left_wing = 1 if inrange(extreme_voter, 0, 1)
    replace left_wing = . if extreme_voter == .
    lab var left_wing "Left-Wing Extreme Voter"

    * dummy variable : extreme voter
    if inlist("`w'", "v", "z", "be") ///
       replace extreme_voter = inlist(extreme_voter, 0, 1, 8, 9, 10) ///
                                if !missing(extreme_voter)

    * dummy variable : political supporter ? 
    if      "`w'" == "q"  g political_supporter = qp116
    else if "`w'" == "r"  g political_supporter = rp111
    else if "`w'" == "s"  g political_supporter = sp111
    else if "`w'" == "t"  g political_supporter = tp118
    else if "`w'" == "u"  g political_supporter = up123
    else if "`w'" == "v"  g political_supporter = vp129
    else if "`w'" == "w"  g political_supporter = wp119
    else if "`w'" == "x"  g political_supporter = xp128
    else if "`w'" == "y"  g political_supporter = yp130
    else if "`w'" == "z"  g political_supporter = zp123
    else if "`w'" == "ba" g political_supporter = bap128
    else if "`w'" == "bb" g political_supporter = bbp129
    else if "`w'" == "bc" g political_supporter = bcp125
    else if "`w'" == "bd" g political_supporter = bdp131
    else if "`w'" == "be" g political_supporter = bep119
    else if "`w'" == "bf" g political_supporter = bfp144
    else if "`w'" == "bg" g political_supporter = bgp144
    else if "`w'" == "bh" g political_supporter = bhp_183
    recode  political_supporter (-8/-1 = .) (2 3 = 0)
    lab var political_supporter "Political Party Supporter"

    * if supporter : support intensity from 1 to 5
    if      "`w'" == "q"  g supporter_intensity = qp11702
    else if "`w'" == "r"  g supporter_intensity = rp113
    else if "`w'" == "s"  g supporter_intensity = sp11202
    else if "`w'" == "t"  g supporter_intensity = tp11902
    else if "`w'" == "u"  g supporter_intensity = up12402
    else if "`w'" == "v"  g supporter_intensity = vp13002
    else if "`w'" == "w"  g supporter_intensity = wp12002
    else if "`w'" == "x"  g supporter_intensity = xp12902
    else if "`w'" == "y"  g supporter_intensity = yp13102
    else if "`w'" == "z"  g supporter_intensity = zp12402
    else if "`w'" == "ba" g supporter_intensity = bap12902
    else if "`w'" == "bb" g supporter_intensity = bbp13002
    else if "`w'" == "bc" g supporter_intensity = bcp12602
    else if "`w'" == "bd" g supporter_intensity = bdp13202
    else if "`w'" == "be" g supporter_intensity = bep12002
    else if "`w'" == "bf" g supporter_intensity = bfp14502
    else if "`w'" == "bg" g supporter_intensity = bgp146
    else if "`w'" == "bh" g supporter_intensity = bhp_185
    recode  supporter_intensity (-8/-1 = .) (5=1) (4=2) (3=3) (2=4) (1=5)
    lab var supporter_intensity "Political Party Support Intensity"
    
    * if supporter : political party supported
    if      "`w'" == "q"  g supporter_party = qp11701
    else if "`w'" == "r"  g supporter_party = rp112
    else if "`w'" == "s"  g supporter_party = sp11201
    else if "`w'" == "t"  g supporter_party = tp11901
    else if "`w'" == "u"  g supporter_party = up12401
    else if "`w'" == "v"  g supporter_party = vp13001
    else if "`w'" == "w"  g supporter_party = wp12001
    else if "`w'" == "x"  g supporter_party = xp12901
    else if "`w'" == "y"  g supporter_party = yp13101
    else if "`w'" == "z"  g supporter_party = zp12401
    else if "`w'" == "ba" g supporter_party = bap12901
    else if "`w'" == "bb" g supporter_party = bbp13001
    else if "`w'" == "bc" g supporter_party = bcp12601
    else if "`w'" == "bd" g supporter_party = bdp13201
    else if "`w'" == "be" g supporter_party = bep12001
    else if "`w'" == "bf" g supporter_party = bfp14501
    else if "`w'" == "bg" g supporter_party = bgp14501
    else if "`w'" == "bh" g supporter_party = bhp_184_01
    replace supporter_party = . if supporter_party < 0
    lab var supporter_party "Political Party Supported"
    
    * clean-up hybrid groups : assign to the more regional party
    replace supporter_party = 2 if supporter_party == 10 // CDU
    replace supporter_party = 3 if supporter_party == 13 // CSU
    replace supporter_party = 4 ///
        if inlist(supporter_party, 11, 14, 22) // FDP
    replace supporter_party = 5 ///
        if inlist(supporter_party, 9, 15, 23) // Alliance90/Gruene
    replace supporter_party = 6 ///
        if inlist(supporter_party, 16, 17, 19, 20, 24) // Linke
    replace supporter_party = 7 ///
        if inlist(supporter_party, 12, 18, 21, 25) // NPD
    replace supporter_party = 27 ///
        if inlist(supporter_party, 27, 30, 31) // AfD

    * AfD and other parties
    replace supporter_party = 8 if !inrange(supporter_party, 1, 7)  ///
        & supporter_party != 27 & !missing(supporter_party)
    * other parties on ninth group
    replace supporter_party = 9 if supporter_party == 8
    * AfD on eight group
    replace supporter_party = 8 if supporter_party == 27

    lab def parties 1 "SPD"    ///
                    2 "CDU"    ///
                    3 "CSU"    ///
                    4 "FDP"    ///
                    5 "Gruene" ///
                    6 "Linke"  ///
                    7 "NPD"    ///
                    8 "AfD"    ///
                    9 "Others"
    lab val supporter_party parties

    * PopuList.org classification for populist parties
    g populist_party = ///
        (inrange(supporter_party, 6, 8) & political_supporter == 1) ///
        if !missing(political_supporter)

    ************************************************************
    * Step 6: Variables at Household-Level                     *
    ************************************************************

    * number of household members
    bys hhnrakt : egen hh_size = count(persnr)
    lab var hh_size "Household Size"
    * number of children in the household
    g hh_children = d11107`short_wave'
    replace hh_children = . if hh_children < 0
    lab var hh_children "No. of Children in the Household"

    * dummy variable : home-ownership
    g house_owner = owner`short_wave'
    recode house_owner (-8/-1 = .) (2 3 4 5 = 0)
    lab var house_owner "House Owner"

    * dummy variable : household has loans on primary residence
    if      "`w'" == "q"  g hasloan_pr = qh31
    else if "`w'" == "r"  g hasloan_pr = rh31
    else if "`w'" == "s"  g hasloan_pr = sh31
    else if "`w'" == "t"  g hasloan_pr = th29
    else if "`w'" == "u"  g hasloan_pr = uh29
    else if "`w'" == "v"  g hasloan_pr = vh28
    else if "`w'" == "w"  g hasloan_pr = wh28
    else if "`w'" == "x"  g hasloan_pr = xh28
    else if "`w'" == "y"  g hasloan_pr = yh29
    else if "`w'" == "z"  g hasloan_pr = zh29
    else if "`w'" == "ba" g hasloan_pr = bah29
    else if "`w'" == "bb" g hasloan_pr = bbh29
    else if "`w'" == "bc" g hasloan_pr = bch29
    else if "`w'" == "bd" g hasloan_pr = bdh29
    else if "`w'" == "be" g hasloan_pr = beh31
    else if "`w'" == "bf" g hasloan_pr = bfh24
    else if "`w'" == "bg" g hasloan_pr = bgh16
    else if "`w'" == "bh" g hasloan_pr = bhh_18
    recode  hasloan_pr (-2 2 = 0)
    replace hasloan_pr = . if hasloan_pr < 0
    
    * dummy variable : household has loans on other expenditures
    if      "`w'" == "q"  g hasloan_ot = qh5501
    else if "`w'" == "r"  g hasloan_ot = rh5001
    else if "`w'" == "s"  g hasloan_ot = sh5001
    else if "`w'" == "t"  g hasloan_ot = th4901
    else if "`w'" == "u"  g hasloan_ot = uh4901
    else if "`w'" == "v"  g hasloan_ot = vh41
    else if "`w'" == "w"  g hasloan_ot = wh41
    else if "`w'" == "x"  g hasloan_ot = xh41
    else if "`w'" == "y"  g hasloan_ot = yh42
    else if "`w'" == "z"  g hasloan_ot = zh42
    else if "`w'" == "ba" g hasloan_ot = bah42
    else if "`w'" == "bb" g hasloan_ot = bbh42
    else if "`w'" == "bc" g hasloan_ot = bch42
    else if "`w'" == "bd" g hasloan_ot = bdh42
    else if "`w'" == "be" g hasloan_ot = beh45
    else if "`w'" == "bf" g hasloan_ot = bfh41
    else if "`w'" == "bg" g hasloan_ot = bgh53
    else if "`w'" == "bh" g hasloan_ot = bhh_46
    replace hasloan_ot = . if hasloan_ot  < 0
    replace hasloan_ot = 0 if hasloan_ot == 2

    * dummy variable : household has outstanding loans
    g hh_hasloans = (hasloan_pr == 1 | hasloan_ot == 1) ///
        if !missing(hasloan_pr) | !missing(hasloan_ot)
    lab var hh_hasloans "Outstanding Loans"

    ************************************************************
    * Yearly Household Disposable Income Computation           *
    ************************************************************

    * market income equals the sum of capital and arned income,
    *   including private transfers and private pensions, before
    *   taxes, social security contributions and monetary social
    *   benefits (post-government income in becker and hauser).
    * disposable household income is defined as household
    *   market income plus public pensions and state monetary
    *   transfers, minus direct tax and social security
    *   contributions (but rental value of owner-occupied homes
    *   is included).
    foreach var of varlist i11102`short_wave' i11105`short_wave' ///
        i11107`short_wave' i11108`short_wave' i11109`short_wave' {
        replace `var' = 0 if `var' < 0
    }
    
    g hh_disp_inc = i11102`short_wave' + i11105`short_wave' + ///
        i11107`short_wave' - i11108`short_wave' - i11109`short_wave'
    
    * adjust for inflation
    replace hh_disp_inc = (hh_disp_inc / cpi) * 100
    lab var hh_disp_inc ///
        "Household Annual Disposable Income (Constant Prices 2016)"
    g ln_hh_disp_inc = ln(hh_disp_inc)
    lab var ln_hh_disp_inc "ln Household Disposable Income"

    ************************************************************
    * Closing Commands                                         *
    ************************************************************

    * Proceed to the next wave
    loc current_wave = `current_wave' + 1

    keep hhnr* persnr wave *wgt sample* male birth_year age* ///
        federal_state residence_gdr89 married migrant_status country_origin ///
        employment_status current_occupation curr_unempl months_ue is_in_wa ///
        was_in_wa is_in_lf is_in_emp self_employed in_education retired ///
        contr_weekly_hours job_as_training egp education voc_educ uni_degree ///
        educ_years cpi *labgro political_interest extreme_voter right_wing ///
        left_wing political_supporter supporter_intensity supporter_party ///
        populist_party hh_size hh_children house_owner hh_hasloans *hh_disp_inc
        
    compress

    * save data in the temporary folder
    save "${mydata}/repeated_cross_wave_`w'", replace

} // wave

noi di as text "Completed!"

***