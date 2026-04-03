library(bigrquery)
library(DBI)
library(dplyr)

PROJECT <- "mapping-patents"
DATASET <- "opm_data"

# Call once at app startup to authenticate.
# Locally: uses Application Default Credentials (gcloud auth application-default login).
# On Posit Connect: set GCP_SA_KEY env var to service account JSON contents.
bq_connect <- function() {
  sa_key <- Sys.getenv("GCP_SA_KEY", unset = "")
  if (nchar(sa_key) > 0) {
    tmp <- tempfile(fileext = ".json")
    writeLines(sa_key, tmp)
    on.exit(unlink(tmp), add = TRUE)
    bq_auth(path = tmp)
  } else {
    bq_auth()  # falls back to ADC
  }
  dbConnect(bigquery(), project = PROJECT, dataset = DATASET)
}

# --- Query functions ---
# Each returns a tibble. Call with an open con from bq_connect().

get_agency_monthly <- function(con) {
  dbReadTable(con, "agency_monthly")
}

get_stem_monthly <- function(con) {
  dbReadTable(con, "stem_monthly")
}

get_geo_monthly <- function(con) {
  dbReadTable(con, "geo_monthly")
}
