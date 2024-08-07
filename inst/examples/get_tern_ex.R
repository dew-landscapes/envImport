
  library("envImport")

  out_dir <- file.path(system.file(package = "envImport"), "examples")

  aoi <- envImport::aoi

  qry_tern <- get_tern(aoi = aoi
                       , save_dir = out_dir
                       , data_map = data_map
                       , get_new = TRUE
                       )
