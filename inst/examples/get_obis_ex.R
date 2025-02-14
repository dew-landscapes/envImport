
aoi <- sf::st_read(file.path(system.file(package = "envImport"), "aoi.shp"))

obis_data <- get_obis(aoi
                      , save_dir = "inst/examples/qry_obis"
                      , data_map = envImport::data_map
                      )
