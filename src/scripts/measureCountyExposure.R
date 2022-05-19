

## -----------------------------------------------------------------------------
## Package Loading
##

pacman::p_load(
    dplyr,
    data.table,
    fst
)

## -----------------------------------------------------------------------------
## Data Loading: Firms Data
##

firms <- as.data.table(
    fst::read_fst(here::here(
        "data", "firms", "exposure", "firm_exposure.fst"
    ))
)

firms_prev <- as.data.table(
    fst::read_fst(here::here(
        "data", "firms", "exposure", "firm_exposure_prev.fst"
    ))
)

## -----------------------------------------------------------------------------
## Compute County-Level Exposure: Recent Firms' Sample
##

# subset data.table keeping just firm exposure and weights by kkz_rek
county_exposure <- firms[, .SD, .SDcols = patterns("kkz_rek|.exposure|w\\.")]
# filter columns with weights on individual data and closdate_year
cols <- names(county_exposure)[
    grepl("w\\.[a-z]{4}_[0-9]{4}$", names(county_exposure))
]
# county exposure using individual financial data weights (single)
county_exposure[
    , paste0("exposure_", cols) := lapply(
        .SD, function(x) fsum(firm_exposure * x)
    ),
    by = .(kkz_rek), .SDcols = (cols)
]
# county exposure using individual financial data weights (combined)
county_exposure[
    , paste0("exposure_w.curr.empl_", seq(2006, 2008)) := lapply(
        seq(2006, 2008), function(x) {
            (
                fsum(firm_exposure * get(paste0("w.curr.empl_", x))) +
                    fsum(firm_exposure * get(paste0("w.empl.curr_", x)))) / 2
        }
    ),
    by = .(kkz_rek)
]
county_exposure[
    , paste0("exposure_w.curr.staf_", seq(2006, 2008)) := lapply(
        seq(2006, 2008), function(x) {
            (
                fsum(firm_exposure * get(paste0("w.curr.staf_", x))) +
                    fsum(firm_exposure * get(paste0("w.staf.curr_", x)))) / 2
        }
    ),
    by = .(kkz_rek)
]
# filter columns of weights with means assigned to ftype firms within the county
cols <- names(county_exposure)[
    grepl("w\\.[a-z]{4}_mean$", names(county_exposure))
]
# county exposure using weights from mean value assigned to ftype
county_exposure[
    , paste0("exposure_", cols) := lapply(
        .SD, function(x) fsum(firm_exposure * x)
    ),
    by = .(kkz_rek), .SDcols = (cols)
]
# county exposure using weights from mean values combined
county_exposure[
    , exposure_w.curr_mean.empl_mean := (
        fsum(w.curr_mean.empl_mean * firm_exposure) +
            fsum(w.empl_mean.curr_mean * firm_exposure)) / 2,
    by = .(kkz_rek)
]
county_exposure[
    , exposure_w.curr_mean.staf_mean := (
        fsum(w.curr_mean.staf_mean * firm_exposure) +
            fsum(w.staf_mean.curr_mean * firm_exposure)) / 2,
    by = .(kkz_rek)
]
# county exposure as arithmetic mean
county_exposure[, exposure_mean := fmean(firm_exposure), by = .(kkz_rek)]

county_exposure <- unique(
    county_exposure[, .SD, .SDcols = patterns("kkz_rek|^exposure_.")]
)

## -----------------------------------------------------------------------------
## Compute County-Level Exposure: Past Firms' Sample
##

past_exposure <- firms_prev[, .SD, .SDcols = patterns("kkz_rek|.exposure|w\\.")]
# filter columns with weights on individual data and closdate_year
cols <- names(past_exposure)[
    grepl("w\\.[a-z]{4}_[0-9]{4}$", names(past_exposure))
]
# county exposure using individual financial data weights (single)
past_exposure[
    , paste0("exposure_past_", cols) := lapply(
        .SD, function(x) fsum(firm_exposure * x)
    ),
    by = .(kkz_rek), .SDcols = (cols)
]
# county exposure using individual financial data weights (combined)
past_exposure[
    , paste0("exposure_past_w.curr.empl_", seq(2006, 2008)) := lapply(
        seq(2006, 2008), function(x) {
            (
                fsum(firm_exposure * get(paste0("w.curr.empl_", x))) +
                    fsum(firm_exposure * get(paste0("w.empl.curr_", x)))) / 2
        }
    ),
    by = .(kkz_rek)
]
past_exposure[
    , paste0("exposure_past_w.curr.staf_", seq(2006, 2008)) := lapply(
        seq(2006, 2008), function(x) {
            (
                fsum(firm_exposure * get(paste0("w.curr.staf_", x))) +
                    fsum(firm_exposure * get(paste0("w.staf.curr_", x)))) / 2
        }
    ),
    by = .(kkz_rek)
]
# county exposure as arithmetic mean
past_exposure[, exposure_past_mean := fmean(firm_exposure), by = .(kkz_rek)]

past_exposure <- unique(
    past_exposure[, .SD, .SDcols = patterns("kkz_rek|^exposure_past_.")]
)

## -----------------------------------------------------------------------------
## Export Final Measures
##

# merge on past exposure because they have all the counties
county_exposure <- merge(
    past_exposure, county_exposure,
    by = "kkz_rek", all.x = T
)

fst::write_fst(
    county_exposure,
    here::here("data", "exposure", "county_exposure.fst")
)
readr::write_delim(
    county_exposure,
    here::here("data", "exposure", "county_exposure.csv"),
    delim = ";"
)
