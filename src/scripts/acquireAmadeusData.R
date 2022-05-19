
#' Script to acquire data of all German firms available in the Amadeus database
#' at the time of the script's execution with company profiles, balance sheets,
#' information about the subsidiaries and bank relationships. Connection to the
#' Amadeus database is performed through the Wharton Research Data Services
#' server, credentials required in a .pgpass file in $HOME directory.
#' -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
## Package Loading
##

pacman::p_load(
    glue, # paste strings
    data.table, # extended data.frame C++ based
    RPostgres, # connector for PostgreSQL database
    fst # fast storage serialization of data.frames
)

## -----------------------------------------------------------------------------
## Define WRDS connection, variables and tables
##

## Include user-defined functions to calculate exposure
source(here::here("src", "exposure", "exposureFunctions.R"))

# this is initializes the database connection with the WRDS,
# it can also go in the project's .Rprofile config.
wrds <- RPostgres::dbConnect(Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    port = 9737,
    dbname = "wrds",
    sslmode = "require",
    user = "xxxxxxxxxxx"
)

# amadeus_x company variables
company_vars <- c(
    "idnr", "name", "name_nat", "address", "address_nat", "city",
    "city_nat", "region", "region_nat", "zipcode", "lstatus",
    "statusdate", "statusdate_year", "dateinc", "dateinc_year",
    "consol", "nace_prim_code"
)
# bankers_x company's banks, idnr is the key to join with company vars
bankers_vars <- c("bnk_name", "compcat", "country", "idnr")
# financials_x company's financials
financials_vars <- c(
    "idnr", "empl", "closdate", "closdate_year", "repbas",
    "toas", "loan", "ltdb", "staf", "tshf", "shfd", "liqr",
    "crpe", "solr", "opre", "wkca", "cuas", "culi"
)
# subsidiaries_x company's information
subsidiaries_vars <- c(
    "idnr", "subs_name", "subs_bvdepnr", "subs_city",
    "subs_status", "subs_date", "subs_toas", "subs_empl",
    "subs_clos"
)

# same condition for everybody and then we merge later using the
# incorporation year to filter only the firms active before the 2007
conditions <- rep("WHERE country = 'GERMANY'", 4)
# firm types in Amadeus
firm_types <- c("v", "l", "m", "s")
# company info, financials, bankers
tables <- c("amadeus", "financials", "bankers", "subsidiaries")
vars <- list(company_vars, financials_vars, bankers_vars, subsidiaries_vars)

# acquire data from Amadeus following the configuration
server_data <- download_firms_data(
    wrds, tables, vars, firm_types, conditions
)

# write data on disk as .fst in their directories
fst::write_fst(server_data[[1]], here::here(
    "data", "firms", "amadeus", "amadeus_companies.fst"
))
fst::write_fst(server_data[[2]], here::here(
    "data", "firms", "amadeus", "amadeus_financials.fst"
))
fst::write_fst(server_data[[3]], here::here(
    "data", "firms", "amadeus", "amadeus_bankers.fst"
))
fst::write_fst(server_data[[4]], here::here(
    "data", "firms", "amadeus", "amadeus_subsidiaries.fst"
))

server_data <- NULL