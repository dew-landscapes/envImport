
  library("envImport")

  aoi <- envImport::aoi

  out_dir <- file.path(system.file(package = "envImport")
                       , "examples"
                       )

  gal <- get_galah(aoi = aoi
                   , save_dir = out_dir
                   , data_map = data_map
                   , sub_dir = "bio_all"
                   )

  ter <- get_tern(aoi = aoi
                  , save_dir = out_dir
                  , data_map = data_map
                  , sub_dir = "bio_all"
                  )


  bio_all_dir <- fs::path(out_dir, "bio_all")

  bio_all <- arrow::open_dataset(fs::dir_ls(bio_all_dir
                                            , regexp = "\\.parquet"
                                            )
                                 ) %>%
    dplyr::collect()
