

## -----------------------------------------------------------------------------
## Package Loading
##

pacman::p_load(
    dplyr,
    data.table,
    haven,
    fst
)

## -----------------------------------------------------------------------------
## Data Loading: Matched Firms with Counties and Bankers
##

# full Amadeus German companies
companies <- as.data.table(
    fst::read_fst(here::here("data", "firms", "amadeus_companies_kkz.fst")))
# data on bankers
bankers <- as.data.table(
    fst::read_fst(here::here("data", "firms", "amadeus_bankers.fst")))
# SOEPRemote codes for counties
soep_counties <- readr::read_delim(
    here::here("data", "soep", "KKZ_KKZREK_Names_SOEPRemote.csv"),
    delim = ";"
)
# past firms' sample
past_firms_data <- haven::read_dta(
    here::here("data", "firms", "amadeus", "firm_panel_KKZ.dta")) %>%
    as.data.table()

## -----------------------------------------------------------------------------
## Past Firms' Data: Collected in July 2020 with partial information
##

past_bankers <- past_firms_data[, .(idnr, bnk_name)]
past_bankers[, total_relationships := .N, by = .(idnr)]
past_bankers[, cbk_relationships := fifelse(
    tolower(bnk_name) %like% "commerzbank", 1, 0)]
past_bankers <- past_bankers[, .(cbk_relationships = sum(cbk_relationships)),
                             by = .(idnr, total_relationships)
]
past_firms_data <- unique(past_firms_data[, -c("plz", "bnk_name", "industry")])
past_firms_data[past_bankers, on = .(idnr),
    c("total_relationships", "cbk_relationships") := list(
        i.total_relationships, i.cbk_relationships
    )]
# calculate firm-level exposure
past_firms_data[, firm_exposure := cbk_relationships / total_relationships]

cols <- c("kkz_rek", "landkreis", "landkreis_rek")
# match and update by reference
past_firms_data[as.data.table(soep_counties), on = .(kkz), (cols) := mget(cols)]

## -----------------------------------------------------------------------------
## Current Available Firms' Data: Collected in March 2022
##

companies <- as.data.table(
    fst::read_fst(here::here("data", "firms", "amadeus_companies_kkz.fst")))

# for code efficiency we filter directly into the bankers table for the firms
# that have been around before 2007 and then joining on companies
# we filter firms that have been around before 2007 and right join on bankers
accounts <- bankers[idnr %in% companies[dateinc_year < 2007, idnr], ]
# count total number of accounts per firm and how many are with Commerzbank
accounts[, total_relationships := .N, by = .(idnr)]
accounts[, cbk_relationships := fifelse(
    tolower(bnk_name) %like% "commerzbank", 1, 0)]
# count total number of accounts with Commerzbank per firm by collapsing
accounts <- accounts[, .(cbk_relationships = sum(cbk_relationships)),
                     by = .(idnr, total_relationships)
]
# new data.table object with the cross-section of our firm sample
exposure_data <- companies[accounts, on = .(idnr)]
# calculate firm-level exposure
exposure_data[, firm_exposure := cbk_relationships / total_relationships]

## -----------------------------------------------------------------------------
## Industry Codes: NACE rev. 2 two digits
##

exposure_data[, nace_two_digits := substr(nace_prim_code, 1, 2)]
industry_shares <- exposure_data[
    !is.na(nace_prim_code), .(kkz_rek, nace_two_digits)
][
    , .(nfirms_ind = .N),
    by = .(kkz_rek, nace_two_digits)
][
    , industry_shares := nfirms_ind / fsum(nfirms_ind),
    by = .(kkz_rek)
]

industry_shares_past_data <- past_firms_data[
    , nace_two_digits := substr(nace_prim_code, 1, 2)
][
    !is.na(nace_prim_code), .(kkz_rek, nace_two_digits)
][
    , .(nfirms_ind = .N),
    by = .(kkz_rek, nace_two_digits)
][
    , industry_shares := nfirms_ind / fsum(nfirms_ind),
    by = .(kkz_rek)
]

fst::write_fst(
    industry_shares,
    here::here("data", "counties", "industry_shares_amadeus.fst")
)
fst::write_fst(
    industry_shares_past_data,
    here::here("data", "counties", "industry_shares_amadeus_prev.fst")
)

## -----------------------------------------------------------------------------
## Financial Data: Remove Duplicates (Consolidated vs. Unconsolidated)
##

financials <- financials[, nrows := .N, by = .(idnr, closdate_year)][
    nrows == 1 | nrows > 1 & repbas %like% "Consolidated"
]

## -----------------------------------------------------------------------------
## Firm-Level Weights using Individual Financial Information
##

vip_years <- seq(2006, 2008)
cols <- c("idnr", "kkz_rek", "closdate_year", "empl", "staf", "cuas", "culi")
cross_financials <- financials[
    exposure_data[, .(idnr, kkz_rek)],
    on = .(idnr)
][
    closdate_year %in% vip_years, ..cols
]
# calculate current ratio
cross_financials[, curr := cuas / culi]
# calculate current ratio weights based on percentile rank
cross_financials[
    !is.na(curr) & !is.nan(curr),
    w.curr := frank(-curr) / fsum(frank(-curr)),
    by = .(closdate_year, kkz_rek)
]
# calculate number of employees weights
cross_financials[
    !is.na(empl), w.empl := empl / fsum(empl),
    by = .(closdate_year, kkz_rek)
]
# calculate payrolls weights
cross_financials[
    !is.na(staf), w.staf := staf / fsum(staf),
    by = .(closdate_year, kkz_rek)
]
# combined current ratio and payrolls weights, we calculate the weights
# considering the same firms and then we do double weighted average on
# the commerzbank dependence and divide by two
cross_financials[
    !is.na(curr) & !is.nan(curr) & !is.na(staf),
    w.curr.staf := frank(-curr) / fsum(frank(-curr)),
    by = .(closdate_year, kkz_rek)
]
cross_financials[
    !is.na(curr) & !is.nan(curr) & !is.na(staf),
    w.staf.curr := staf / fsum(staf),
    by = .(closdate_year, kkz_rek)
]
# combined with empl and curr
cross_financials[
    !is.na(curr) & !is.nan(curr) & !is.na(empl),
    w.curr.empl := frank(-curr) / fsum(frank(-curr)),
    by = .(closdate_year, kkz_rek)
]
cross_financials[
    !is.na(curr) & !is.nan(curr) & !is.na(empl),
    w.empl.curr := empl / fsum(empl),
    by = .(closdate_year, kkz_rek)
]

cols <- names(cross_financials)[
    !grepl("closdate_year|idnr|kkz_rek", names(cross_financials))
]
cross_financials <- data.table::dcast(
    cross_financials, idnr ~ closdate_year,
    value.var = cols
)

## ----------------------------------------------------------------------
## Past Firms' Sample
##

cross_financials_past_sample <- financials[
    past_firms_data[, .(idnr, kkz_rek)],
    on = .(idnr)
][
    closdate_year %in% vip_years, ..cols
]
# calculate current ratio
cross_financials_past_sample[, curr := cuas / culi]
# calculate current ratio weights based on percentile rank
cross_financials_past_sample[
    !is.na(curr) & !is.nan(curr),
    w.curr := frank(-curr) / fsum(frank(-curr)),
    by = .(closdate_year, kkz_rek)
]
# calculate number of employees weights
cross_financials_past_sample[
    !is.na(empl), w.empl := empl / fsum(empl),
    by = .(closdate_year, kkz_rek)
]
# calculate payrolls weights
cross_financials_past_sample[
    !is.na(staf), w.staf := staf / fsum(staf),
    by = .(closdate_year, kkz_rek)
]
# combined current ratio and payrolls weights, we calculate the weights
# considering the same firms and then we do double weighted average on
# the commerzbank dependence and divide by two
cross_financials_past_sample[
    !is.na(curr) & !is.nan(curr) & !is.na(staf),
    w.curr.staf := frank(-curr) / fsum(frank(-curr)),
    by = .(closdate_year, kkz_rek)
]
cross_financials_past_sample[
    !is.na(curr) & !is.nan(curr) & !is.na(staf),
    w.staf.curr := staf / fsum(staf),
    by = .(closdate_year, kkz_rek)
]
# combined with empl and curr
cross_financials_past_sample[
    !is.na(curr) & !is.nan(curr) & !is.na(empl),
    w.curr.empl := frank(-curr) / fsum(frank(-curr)),
    by = .(closdate_year, kkz_rek)
]
cross_financials_past_sample[
    !is.na(curr) & !is.nan(curr) & !is.na(empl),
    w.empl.curr := empl / fsum(empl),
    by = .(closdate_year, kkz_rek)
]

cols <- names(cross_financials_past_sample)[
    !grepl("closdate_year|idnr|kkz_rek", names(cross_financials_past_sample))
]
cross_financials_past_sample <- data.table::dcast(
    cross_financials_past_sample, idnr ~ closdate_year,
    value.var = cols
)

## -----------------------------------------------------------------------------
## Firm-Level Weights with Pooled Information of Firms' Financials
##

vip_years <- seq(2004, 2008)
cols <- c("kkz_rek", "ftype", "closdate_year", "empl", "staf", "cuas", "culi")
# match financials with companies' county code, restrict for the years around
# 2006 and then take mean and median at firm type and county level
weights_stats <- financials[companies[, .(idnr, kkz_rek)], on = .(idnr)][
    closdate_year %in% vip_years, ..cols
] %>%
    collapse::fgroup_by(kkz_rek, ftype) %>%
    collapse::fsummarise(across(empl:culi, fmean))
# assign mean label to column
names(weights_stats) <- c(
    names(weights_stats)[1:2], paste0(names(weights_stats)[3:6], "_mean")
)
# current ratio with averages
weights_stats[, curr_mean := 1 / (cuas_mean / culi_mean)]
# copy into firm identifiers
mean_weights <- merge(
    exposure_data[, .(idnr, kkz_rek, ftype)], weights_stats,
    by = c("kkz_rek", "ftype"), all.x = TRUE
)

cols <- c("empl_mean", "staf_mean", "curr_mean")

# replace all NaN with NA
mean_weights <- mean_weights[
    , lapply(.SD, function(x) replace(x, is.nan(x), NA))
]
# firm-level weights using mean
mean_weights[
    , paste0("w.", cols) := lapply(.SD, function(x) x / fsum(x)),
    by = .(kkz_rek), .SDcols = (cols)
]
# firm-level weights curr+staf combined
mean_weights[
    !is.na(curr_mean) & !is.na(staf_mean),
    w.curr_mean.staf_mean := curr_mean / fsum(curr_mean),
    by = .(kkz_rek)
]
mean_weights[
    !is.na(curr_mean) & !is.na(staf_mean),
    w.staf_mean.curr_mean := staf_mean / fsum(staf_mean),
    by = .(kkz_rek)
]
# firm-level weights curr+empl
mean_weights[
    !is.na(curr_mean) & !is.na(empl_mean),
    w.curr_mean.empl_mean := curr_mean / fsum(curr_mean),
    by = .(kkz_rek)
]
mean_weights[
    !is.na(curr_mean) & !is.na(empl_mean),
    w.empl_mean.curr_mean := empl_mean / fsum(empl_mean),
    by = .(kkz_rek)
]

## -----------------------------------------------------------------------------
## Export Final Firm-Level Data with Exposure and Weights
##

# add cross-section information of financials to the firm-level data
exposure_data <- merge(exposure_data, cross_financials,
                       by = "idnr", all.x = TRUE)
exposure_data <- merge(exposure_data, mean_weights[, -c("ftype", "kkz_rek")],
                       by = "idnr", all.x = TRUE)
# add cross-section information and weights to the past firm-level data
past_firms_data <- merge(past_firms_data, cross_financials_past_sample,
                         by = "idnr", all.x = TRUE)

fst::write_fst(
    exposure_data,
    here::here("data", "firms", "exposure", "firm_exposure.fst")
)
fst::write_fst(
    past_firms_data,
    here::here("data", "firms", "exposure", "firm_exposure_prev.fst")
)
