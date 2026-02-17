
aoi <- sf::st_read(file.path(system.file(package = "envImport"), "aoi.shp"))

obis_data <- get_obis(aoi
                      , save_dir = tempdir()
                      , data_map = envImport::data_map
                      , get_new = TRUE
                      )

obis_data |>
  dplyr::count()

# Removing certain methods
obis_data <- get_obis(aoi
                      , save_dir = tempdir()
                      , data_map = envImport::data_map
                      , removes = list(basisOfRecord = c("PreservedSpecimen"))
                      , get_new = TRUE
                      )

obis_data |>
  dplyr::count()
