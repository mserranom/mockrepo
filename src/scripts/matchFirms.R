
#' -----------------------------------------------------------------------------
#' Script to match German firms acquired from Amadeus company profiles with
#' the county where the factory is situated using a combination of ZIP codes,
#' kreise names and city names, and harmonise the county codes with the
#' nomenclature used by the German Socio-Economic Panel.
#' -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Package Loading
##

pacman::p_load(
    dplyr, # tidyverse: general data manipulation
    tidyr, # tidyverse: tidy messy data
    stringr, # tidyverse: string manipulation
    stringdist, # string distance calculation for R
    glue, # go easy with strings
    data.table, # extended data.frame
    readr, # read delimited files into tibble
    fst, # fast storage serialization of data.frame(s)
    haven # handle Stata and SAS data
)

## -----------------------------------------------------------------------------
## Data Loading
##

# companies previously acquired from Amadeus
companies <- as.data.table(
    fst::read_fst(
        here::here("data", "firms", "amadeus", "amadeus_companies.fst")
    )
)

## the data.frame for matching county codes with ZIP codes
kreise_codes <- as.data.table(
    fst::read_fst(here::here("data", "counties", "zipcodes_counties.fst"))
)

## -----------------------------------------------------------------------------
## First Step: ZIP Code Matching
##

# matching table zip-kkz keeping unique values in the previous data
match_zips <- unique(kreise_codes[, .(plz, kkz)])
# we keep zip code as integer
companies[, zipcode := as.integer(zipcode)]
# double check that Landkreis and Kreis are at the end of the
# A[B, on = 'a', bb := i.b], join A with B and upddate A by reference
companies[match_zips, on = .(zipcode = plz), kkz := i.kkz]

## -----------------------------------------------------------------------------
## Second Step: String Fuzzy Matching with County Names
##

# reverse epithets of counties just in case
companies[, region_nat := str_replace(
    region_nat, "(Landkreis|Kreis|Eifelkreis)\\s(.+)", "\\2 \\1"
)]
# select from the matching table only names and county codes
match_names <- unique(kreise_codes[, .(kkz, names)])
# select only the region names for unmatched firms in the step before
unmatched_regions <- data.table(
    names = unique(companies[is.na(kkz) & !is.na(region_nat), region_nat])
)
# create grid for matching each region name with names in the matching table
scores <- expand.grid(unmatched_regions[, names], match_names[, names])
# run Jaro-Winkler string distance function
scores$dist <- stringdist::stringdist(scores$Var1, scores$Var2, method = "jw")
colnames(scores) <- c("region_nat", "names", "dist")
# there are some region names that are Bundesland names and we remove it
states <- unique(kreise_codes[bundesland != "Berlin", bundesland])
# select the name with the smallest distance for each region name
match_names <- scores %>%
    dplyr::filter(!(region_nat %in% states)) %>%
    dplyr::group_by(region_nat) %>%
    dplyr::arrange(dist) %>%
    dplyr::slice(1) %>%
    dplyr::select(region_nat, names) %>%
    as.data.table()
# add county codes back from the matching table
match_names[kreise_codes, on = .(names), kkz := i.kkz]
# manual corrections
match_names[region_nat %like% "München  Landeshaupt", names := "München Stadt"]
match_names[region_nat %like% "Lauenburg", names := "Herzogtum Lauenburg Kreis"]
match_names[region_nat %like% "Neuss", names := "Rhein-Neuss Kreis"]
match_names[region_nat %like% "Wendel", names := "St. Wendel Landkreis"]

# add county codes to the unmatched companies
companies[
    match_names,
    on = .(region_nat), kkz := fifelse(is.na(kkz), i.kkz, kkz)
]

## -----------------------------------------------------------------------------
## Third Step: String Fuzzy Matching with "City" Names
##

match_cities[, names := paste0(city_nat, " - ", toupper(bundesland))]
scores <- expand.grid(
    unique(unmatched_cities[is.na(kkz), city_nat]), match_cities[, names]
)
scores$dist <- stringdist::stringdist(scores$Var1, scores$Var2, method = "jw")
colnames(scores) <- c("city_nat", "names", "dist")
scores <- scores %>%
    dplyr::group_by(city_nat) %>%
    dplyr::arrange(dist) %>%
    dplyr::slice(1) %>%
    dplyr::select(city_nat, names) %>%
    as.data.table()
# manual correction of bad matching
nuisance_cities <- c(
    "SENSBACHTAL", "RIEZLERN", "HIRSCHEGG", "TWIEFLINGEN",
    "HOYERSHAUSEN", "ZÖBLITZ", "WIES", "NEUNKIRCHEN",
    "HELSINKI", "SACHSEN", "KUOPIO", "TAMPERE"
)
scores <- scores[!(city_nat %in% nuisance_cities)]
# manual correction city mismatch
city_mismatch <- c(
    "LICHTE", "HADMERSLEBEN", "BRANDENBURG", "MARL", "VOERDE",
    "FREIBURG", "LUDWIGSHAFEN", "ST. WENDEL", "MUNICH"
)
city_correction <- c(
    "Sonneberg - Thüringen", "Oschersleben - Sachsen-Anhalt",
    "Brandenburg an der Havel - Brandenburg",
    "Marl - Nordrhein-Westfalen",
    "Voerde (Niederrhein) - Nordrhein-Westfalen",
    "Freiburg im Breisgau - Baden-Württemberg",
    "Ludwigshafen am Rhein - Rheinland-Pfalz",
    "Sankt Wendel - Saarland", "München - Bayern"
)

for (s in seq_along(city_mismatch)) {
    scores[city_nat == city_mismatch[s], names := toupper(city_correction[s])]
}

scores <- merge(scores, match_cities[, .(names, kkz)], by = "names")
unmatched_cities[
    scores[, .(kkz, city_nat)],
    on = .(city_nat),
    kkz := fifelse(is.na(kkz), i.kkz, kkz)
]
unmatched_cities <- unique(unmatched_cities[!is.na(kkz), .(kkz, city_nat)])
# add county codes to the unmatched companies
companies[unmatched_cities,
    on = .(city_nat),
    kkz := fifelse(is.na(kkz), i.kkz, kkz)
]

# remove unmatched firms
companies <- companies[!is.na(kkz)]

## -----------------------------------------------------------------------------
## Fourth-Step: Link with SOEP County Codes
##

soep_counties <- readr::read_delim(
    here::here("data", "soep", "KKZ_KKZREK_Names_SOEPRemote.csv"),
    delim = ";"
)

companies[, kkz_char := kkz]
companies[, kkz := as.numeric(kkz)]
cols <- c("kkz_rek", "landkreis", "landkreis_rek")
# match and update by reference
companies[as.data.table(soep_counties), on = .(kkz), (cols) := mget(cols)]

## -----------------------------------------------------------------------------
## Write Final Data
##

fst::write_fst(companies, here::here(
    "data", "firms", "amadeus", "amadeus_companies_kkz.fst"
))
