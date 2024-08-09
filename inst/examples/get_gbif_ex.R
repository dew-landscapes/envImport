
  # setup -------
  library("envImport")

  # no aoi ------
  out_dir <- file.path(system.file(package = "envImport"), "examples", "get_gbif_ex")

  gbif_data <- get_gbif(save_dir = out_dir
                        , get_new = FALSE
                        #, data_map = envImport::data_map
                        , predicates = rgbif::pred_and(rgbif::pred("taxonKey", 2474903)
                                                       , rgbif::pred("year", 2000)
                                                       )
                        , previous_key = "0057516-240626123714530"
                        )

  # 667 records 2024-08-09
  nrow(gbif_data)
  head(gbif_data)

  # with aoi
  out_dir <- file.path(system.file(package = "envImport"), "examples", "get_gbif_aoi_ex")

  gbif_data <- get_gbif(save_dir = out_dir
                        , aoi = envClean::aoi
                        , data_map = envImport::data_map
                        , get_new = FALSE
                        , predicates = rgbif::pred("year", 2000)
                        )

  # 107 records 2024-08-09
  nrow(gbif_data)

  # .bib created
  readr::read_lines(fs::path(out_dir, "gbif", "gbif.bib"))
