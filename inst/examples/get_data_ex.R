
  library("envImport")

  out_dir <- file.path(system.file(package = "envImport"), "examples")

  # galah--------

  ## config -------
  galah::galah_config(email = Sys.getenv("GBIF_email")
                      , username = Sys.getenv("GBIF_user")
                      , password = Sys.getenv("GBIF_pwd")
                      , caching = TRUE
                      , download_reason_id = 10 # testing
                      )

  old_atlas <- galah::galah_config()$atlas$region

  galah::galah_config(atlas = "GBIF")

  ## 01: atlas = gbif --------

  qry01 <- galah::galah_call() %>%
    galah::galah_identify("Ardeotis australis") %>%
    galah::galah_filter(year == 2000) %>%
    galah::atlas_occurrences() %>%
    dplyr::collect()


  ## 02: atlas = ala ----------

  galah::galah_config(atlas = "ALA")

  galah::galah_config(email = Sys.getenv("ALA_email"))

  # 'qry' used for both qry02 and qry03
  qry <- galah::galah_call() %>%
    galah::galah_identify("Ardeotis australis") %>%
    galah::galah_filter(year == 2000)

  qry02 <- qry %>%
    galah::atlas_occurrences()

  # similar (but not identical) # of records
    # difference due to predicates?
  nrow(qry01)
  nrow(qry02)


  ## 03: get_galah ---------

  qry03 <- get_galah(save_dir = fs::path(out_dir, "qry03")
                     , data_map = data_map
                     , qry = qry
                     )

  nrow(qry03) == nrow(qry02)

  # names from data_map
  names(qry02)
  names(qry03)

  ## 04: get_galah with profile -------

  qry04 <- get_galah(save_dir = fs::path(out_dir, "qry04")
                     , data_map = data_map
                     , qry = qry %>%
                       galah::apply_profile(CSDM)
                     )

  # lost some records due to the profile
  nrow(qry03) > nrow(qry04)

  # bio_all --------
  ## get_galah for aoi -------
  bio_all_galah <- get_galah(aoi = envImport::aoi
                             , save_dir = out_dir
                             , data_map = data_map
                             , sub_dir = "bio_all"
                             )

  ## get_tern for aoi --------
  bio_all_tern <- get_tern(aoi = envImport::aoi
                           , save_dir = out_dir
                           , data_map = data_map
                           , sub_dir = "bio_all"
                           )

  ## or using get_data -------
  # to get both galah and tern
  datas <- c("galah", "tern")

  purrr::map(datas
              , \(x) get_data(x
                              , save_dir = out_dir
                              , get_new = FALSE
                              , aoi = envImport::aoi
                              , data_map = data_map
                              , sub_dir = "bio_all"
                              )
              )

  ## single dataset --------
  bio_all <- arrow::open_dataset(fs::dir_ls(fs::path(out_dir, "bio_all")
                                            , regexp = "\\.parquet"
                                            )
                                 ) %>%
    dplyr::collect()

  # 'bio_all' is now the sum of its components
  nrow(bio_all) == nrow(bio_all_galah) + nrow(bio_all_tern)

  # clean up -------
  # return to original atlas
  galah::galah_config(atlas = old_atlas)
