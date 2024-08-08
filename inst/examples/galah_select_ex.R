
library("galah")

galah_config(email = Sys.getenv("ALA_email")
             , download_reason_id = 10 # testing
             )

example_cols <- c("eventDate", "scientificName")

# example_cols are in 'fields'
example_cols %in% show_all("fields")$id

# Initiate a query
qry <- galah_call() |>
  identify("Swainsona viridis")

# Returns a tibble with ~ 100 records
qry |>
  select(group = "basic") |>
  atlas_occurrences()

# No results when directly selecting names
qry |>
  select(c("eventDate", "scientificName")) |>
  atlas_occurrences()

# No results selecting names with all_of
qry |>
  dplyr::select(all_of(example_cols)) |>
  atlas_occurrences()

# No results via request_data
request_data(type = "occurrences") |>
  identify("Swainsona viridis") |>
  select(all_of(example_cols)) |>
  collect()

# Try swapping order as per https://github.com/AtlasOfLivingAustralia/galah-R/issues/239
example_cols <- example_cols[c(2, 1)]

# Still no results
qry |>
  dplyr::select(all_of(example_cols)) |>
  atlas_occurrences()

# Try directly providing cols - still nothing
qry |>
  select(c("scientificName", "eventDate")) |>
  atlas_occurrences()
