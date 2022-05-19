# TODO fix after making the script to create the weights

pacman::p_load(
    dplyr,
    readr,
    data.table,
    fst,
    emayili
)

source(here::here("src", "functions", "SOEPRemoteFunctions.R"))

sender <- "Alessandro Pizzigolotto <alessandro.pizzigolotto@nhh.no>"

## -----------------------------------------------------------------------------
## INPUT MACRO VARIABLES
##

# don't pass strings to input because it gets complicated
macro_panel <- readr::read_delim(
    here::here("data", "counties", "macro_genesis.csv")
) %>%
    dplyr::select(-kreis)

soep_make_input(
    df = macro_panel, filename = "input_macro_genesis.do",
    path = here::here("data", "soep"),
    dbname = "macro_variables"
)

soep_deliver_job(
    path = here::here("data", "soep", "input_macro_genesis.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Input Macro Variables"
)

## -----------------------------------------------------------------------------
## INPUT COUNTY-LEVEL EXPOSURE
##

county_exposure <- fst::read_fst(
    here::here("data", "exposure", "county_exposure.fst"))
# keep only 2007 financial data weights
county_exposure <- county_exposure %>%
    dplyr::select(!matches("2006|2008"))

# change names for easier wildcards in Stata
colnames(county_exposure) <- colnames(county_exposure) %>%
    stringr::str_replace_all("w.", "") %>%
    stringr::str_replace_all("\\.", "_") %>%
    stringr::str_replace_all("exposure_", "cbk_") %>%
    stringr::str_replace_all("curr_mean_empl_mean", "ce_mean") %>%
    stringr::str_replace_all("curr_mean_staf_mean", "se_mean") %>%
    stringr::str_replace_all("(![enp][a-z]{1})[a-z]{3}_mean", "\\1_mean") %>%
    stringr::str_replace_all("curr_empl_2007", "ce_2007") %>%
    stringr::str_replace_all("curr_staf_2007", "se_2007") %>%
    stringr::str_replace_all("([a-z]{1})[a-z]{3}_2007", "\\1_2007")

soep_make_input(
    df = county_exposure, filename = "input_county_exposure.do",
    path = here::here("data", "soep"),
    dbname = "county_exposure"
)

soep_deliver_job(
    path = here::here("data", "soep", "input_county_exposure.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Input County Exposure"
)

## -----------------------------------------------------------------------------
## INPUT TEXT ANALYSIS
##

text_files <- c(
    "manifesto_scores",
    "protokolle_scores",
    "manifesto_scores_slda_fakenews",
    "manifesto_scores_slda_rooduijn",
    "protokolle_scores_slda_fakenews",
    "protokolle_scores_slda_rooduijn"
)

for (filename in text_files) {
    scores <- read_delim(
        here::here("out", "data", "text", paste0(filename, ".csv")))
    scores <- scores %>%
        dplyr::mutate(supporter_party = dplyr::case_when(
            party == "SPD" ~ 1,
            party == "CDU/CSU" ~ 2,
            party == "FDP" ~ 4,
            party == "GRUENE" ~ 5,
            party == "PDS/LINKE" ~ 6,
            party == "AfD" ~ 8
        )) %>%
        dplyr::filter(!(party %in% c("Pirates", "independent") | is.na(party)))

    csu <- scores %>%
        dplyr::filter(supporter_party == 2) %>%
        dplyr::mutate(supporter_party = 3)

    scores <- rbind(scores, csu) %>%
        dplyr::select(!matches("._share|^party$")) %>%
        dplyr::rename(wave = year)

    slist <- stringr::str_split(filename, "_") %>%
        unlist()

    if (length(slist) < 3) {
        scores <- scores %>%
            dplyr::rename_with(~ paste0(slist[[1]], "_bf"), bank_and_finance) %>%
            dplyr::rename_with(~ paste0(slist[[1]], "_pop_fake"), popfake) %>%
            dplyr::rename_with(~ paste0(slist[[1]], "_pop_rood"), poprood)
    } else {
        scores <- scores %>%
            dplyr::rename_with(
                ~ paste0(slist[[1]], "_bf_",
                       stringr::str_sub(slist[[4]], 1, 4), "_", slist[[3]]),
                bank_and_finance) %>%
            dplyr::rename_with(
                ~ paste0(slist[[1]], "_pop_",
                       stringr::str_sub(slist[[4]], 1, 4), "_", slist[[3]]),
                populism)
    }

    soep_make_input(
        df = scores,
        filename = paste0(filename, ".do"),
        path = here::here("data", "soep"),
        dbname = filename
    )
    soep_deliver_job(
        path = here::here("data", "soep", paste0(filename, ".do")),
        sender = sender,
        project = "CreditPopulism",
        jobname = filename
    )
}

## IN PROGRESS TODO RANDOM INPUT TO THE SERVER ----

soep_deliver_job(
    path = here::here("src", "jobs", "01_prepare_cross_waves.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Repeated Cross-Section Waves"
)

soep_deliver_job(
    path = here::here("src", "jobs", "02_prepare_county_data.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "County Data"
)

soep_deliver_job(
    path = here::here("src", "jobs", "03_final_data.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Final Dataset"
)

soep_deliver_job(
    path = here::here("src", "jobs", "04_summary_tables.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Summary Tables"
)

soep_deliver_job(
    path = here::here("src", "jobs", "05_rf_base.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Reduced Form Baseline (curr_unempl fixed)"
)

soep_deliver_job(
    path = here::here("src", "jobs", "06_rf_events_base.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Event Study Reduced Form Baseline (curr_unempl fixed)"
)

soep_deliver_job(
    path = here::here("src", "jobs", "07_rf_text_base.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Reduced Form Text Baseline"
)

soep_deliver_job(
    path = here::here("src", "jobs", "mean_outcome_trajectories.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Mean Outcome Trajectories (baseline 2008)"
)

soep_deliver_job(
    path = here::here("src", "jobs", "event_outcome_trajectories.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Event Outcome Trajectories"
)

soep_deliver_job(
    path = here::here("src", "jobs", "exposure_functional_form.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Treatment Functional Forms (curr_unempl fixed)"
)

soep_deliver_job(
    path = here::here("src", "jobs", "get_county_variables_series.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "County Variables Series"
)

soep_deliver_job(
    path = here::here("src", "jobs", "balance_table.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Test Balance Table"
)

soep_deliver_job(
    path = here::here("src", "jobs", "mechanism_analysis.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Exposure interacted with Individual Unemployment"
)

## -----------------------------------------------------------------------------
## TESTS

soep_deliver_job(
    path = here::here("tests", "src", "test.do"),
    sender = sender,
    project = "CreditPopulism",
    jobname = "Testing Consistency in Observations"
)
