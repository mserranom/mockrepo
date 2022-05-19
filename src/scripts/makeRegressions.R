# regressions bla bla

## -----------------------------------------------------------------------------
## Package Loading
##

# install pacman package manager if it is not already installed
if (!require("pacman")) install.packages("pacman")
# load packages with pacman
pacman::p_load(
    dplyr,     # tidyverse : general data manipulation
    tidyr,     # tidyverse : tidy messy data functions
    lubridate, # tidyverse : date manipulation
    collapse,  # C++ based functions and data manipulation
    ggplot2,   # the famous data visualization package big boy
    readr,     # tidyverse : read triangular data
    haven,     # tidyverse : read foreign statistical packages data
    fst        # fast storage serialization of data.frame(s)
)

macro_county_series <- readr::read_delim(here::here("out", "data", "soep_county_macro_series.csv")) %>%
    dplyr::rename(syear = wave)
macro_genesis <- readr::read_delim(here::here("data", "counties", "macro_genesis.csv"))
county_exposure <- fst::read_fst(here::here("data", "exposure", "county_exposure.fst"))

industry_shares_prev <- fst::read_fst(here::here("data", "counties", "industry_shares_amadeus_prev.fst")) %>%
    dplyr::filter(nace_two_digits != "") %>%
    tidyr::complete(kkz_rek, nace_two_digits, fill = list(industry_shares = 0)) %>%
    data.table(.) %>%
    data.table::dcast(kkz_rek ~ nace_two_digits, value.var = "industry_shares")
names(industry_shares_prev) <- c("kkz_rek", paste0("industry_share_", names(industry_shares_prev)[!grepl("kkz_rek", names(industry_shares_prev))]))

county_data <- macro_county_series %>%
    dplyr::left_join(macro_genesis, by = c("kkz_rek", "syear")) %>%
    dplyr::left_join(county_exposure, by = "kkz_rek") %>%
    dplyr::left_join(industry_shares_prev, by = "kkz_rek")

county_data <- county_data %>%
    dplyr::mutate(exposure_past_mean_post = if_else(syear > 2008, exposure_past_mean, 0)) %>%
    dplyr::mutate(ln_uemp = log(uemprate))

county_data %>%
    dplyr::filter(syear %in% seq(2007, 2012)) %>%
    fixest::feols(ln_uemp ~ exposure_past_mean_post | kkz_rek + syear)
