

#' Get data using `galah::atlas_occurrences()`
#'
#'
#'
#' @param aoi Optional simple feature (sf). Used to limit the occurrences
#' returned via `galah::galah_geolocate()`
#' @param save_dir Character. Path to directory into which to save outputs. If
#' `NULL` results will be saved to `here::here("out", "ds", "galah")`. File will be
#' named `galah.parquet`
#' @param get_new Logical. If FALSE, will attempt to load from existing
#' `save_dir`.
#' @param name Character. `data_name` value in `envImport::data_map`
#' (or other `data_map`)
#' @param data_map Dataframe or NULL. Mapping of fields to retrieve. See example
#' `envImport::data_map`
#' @param node Character. Name of atlas to use (see `galah::atlas_occurrences()`).
#' Doesn't seem to work with node = "GBIF" and untested on other nodes.
#' @param qry `NULL` or an object of class data_request, created using
#' `galah::galah_call()`
#' @param check_rel_metres Logical. Ensure that `coordinateUncertaintyInMetres`
#' is no less than `generalisationInMetres`?
#' @param filter_inconsistent Logical. If `TRUE`, inconsistencies between the
#' `occurrenceStatus` column and either `organismQuantity` or `individualCount`
#' are removed. e.g. a record with `occurrenceStatus == "ABSENT"` but
#' `individualCount == 1` would be filtered.
#' @param ... Passed to `envImport::file_prep()`
#'
#' @return Dataframe of occurrences and file saved to `save_dir`. .bib created
#' when `download_reason_id != 10.`
#' @export
#'
#' @example inst/examples/get_data_ex.R
get_galah <- function(aoi = NULL
                      , save_dir = NULL
                      , get_new = FALSE
                      , name = "galah"
                      , data_map = NULL
                      , node = "ALA"
                      , qry = NULL
                      , check_rel_metres = TRUE
                      , filter_inconsistent = TRUE
                      , ...
) {

  # save file -------
  save_file <- file_prep(save_dir, name, ...)

  # run the qry ---------
  get_new <- if(!file.exists(save_file)) TRUE else get_new

  if(get_new) {

    # atlas -------
    old_atlas <- galah::galah_config()$atlas$region
    galah::galah_config(atlas = node)
    on.exit(galah::galah_config(atlas = old_atlas))

    # qry -------
    ## initiate-------
    if(is.null(qry)) {

      qry <- galah::galah_call()

    }

    ## select-------
    if(is.null(data_map)) {

      get_groups <- c("basic", "event", "taxonomy")

      qry <- qry %>%
        galah::galah_select(group = get_groups)

      data_map <- tibble::tibble(col = unique(unlist(purrr::map(get_groups, galah:::preset_groups)))) %>%
        dplyr::mutate(!!rlang::ensym(name) := col)

    } else {

      select_names <- choose_names(data_map = data_map
                                   , this_name = name
      ) %>%
        dplyr::filter(value %in% galah::show_all("fields")$id)

      qry <- qry %>%
        galah::select(recordID # inc recordID here: see https://github.com/AtlasOfLivingAustralia/galah-R/issues/239
                      , select_names$value
        )

    }

    ## aoi------
    if(!is.null(aoi)) qry1 <- qry %>% # relabel qry as qry1 to preserve the original qry for potential use with split aoi's later
      galah::galah_geolocate(aoi)


    ## check records--------
    records <- {if(!is.null(aoi)) qry1 else qry} %>%
      galah::atlas_counts() %>%
      dplyr::pull(count)

    if(records > 50000000 & !is.null(aoi)) {

      ### split aoi if records > 50M ----

      max_splits <- ceiling(records/50000000)+1

      combos <- tibble::tibble(cells_x = seq(1, max_splits)
                               , cells_y = seq(1, max_splits)
      ) %>%
        expand.grid() %>%
        dplyr::mutate(tot = cells_x + cells_y
                      , diff = abs(cells_x - cells_y)
        ) %>%
        dplyr::arrange(tot, diff)

      grd <- purrr::map2(combos$cells_x
                         , combos$cells_y
                         , \(x,y)

                         aoi %>%
                           sf::st_make_grid(n = c(x,y)) %>%
                           sf::st_sf() %>%
                           dplyr::mutate(cell = dplyr::row_number()) %>%
                           tidyr::nest(data = -(cell)) %>%
                           dplyr::mutate(rec = purrr::map_int(data, \(a) qry %>%
                                                                galah::galah_geolocate(a) %>%
                                                                galah::atlas_counts() %>%
                                                                dplyr::pull(count)
                           )
                           , combo = paste0(x, " x ", y)
                           , cells_x = x
                           , cells_y = y
                           , cells = x + y
                           , above_limit = rec > 50000000
                           ) %>%
                           tidyr::unnest(cols = c(data))

      ) %>%
        purrr::list_rbind() %>%
        dplyr::group_by(combo) %>%
        dplyr::mutate(any_above_limit = any(above_limit)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!any_above_limit) %>%
        dplyr::filter(cells == min(cells)) %>%
        dplyr::filter(cells_x == min(cells_x)) %>%
        dplyr::filter(cells_y == min(cells_y)) %>%
        sf::st_sf()

      qry <- grd %>%
        dplyr::select(cell, geometry) %>%
        tidyr::nest(data = -c(cell)) %>%
        .$data %>%
        purrr::map(\(x) qry %>%
                     galah::galah_geolocate(x)
        )


      warning("Returned records for query/aoi ("
              , records
              , ") would exceed limit of 50 million. "
              , "aoi has been split into a "
              , unique(grd$combo)
              , " cell grid to allow multiple smaller downloads."
      )

    } else if(records > 50000000 & is.null(aoi)) {

      ### stop if no aoi & records > 50M ----

      stop("Returned records for query ("
           , records
           , ") would exceed limit of 50 million"
      )

    } else {

      ### continue if records < 50M ----

      if(!is.null(aoi)) qry <- qry1

      message(records
              , " records available from galah via "
              , galah::galah_config()$atlas$acronym
              , " node"
      )

    }

    ## retrieve ------

    make_doi <- galah::galah_config()$user$download_reason_id != 10

    temp <- qry %>%
      purrr::map(\(x) x %>%
                   galah::atlas_occurrences(mint_doi = make_doi)
      )

    # doi string for use below
    if(make_doi) doi <- purrr::map_chr(temp, \(x) attr(x, "doi"))

    # combine multiple downloads
    temp <- temp %>%
      purrr::list_rbind()

    ## rel_metres-------
    if(check_rel_metres) {

      temp <- temp %>%
        dplyr::rename(cuim = coordinateUncertaintyInMeters
                      , gim = generalisationInMetres
        ) %>%
        dplyr::mutate(coordinateUncertaintyInMeters = dplyr::case_when(!is.na(gim) & gim > cuim ~ gim
                                                                       , TRUE ~ cuim
        )
        )

    }

    ## filter_inconsistent --------
    if(filter_inconsistent) {

      temp <- temp %>%
        dplyr::filter(!(occurrenceStatus == "ABSENT" &
                          !is.na(organismQuantity) &
                          organismQuantity > 0
        )
        ) %>%
        dplyr::filter(!(occurrenceStatus == "PRESENT" &
                          !is.na(organismQuantity) &
                          organismQuantity == 0
        )
        )

    }


    # bib -------
    if(make_doi) {

      if(is.character(doi)) {

        bib <- bibentry(bibtype = "MISC"
                        , key = "galah"
                        , title = "Occurrence download data"
                        , author = utils::person("Atlas Of Living Australia")
                        , publisher = "Atlas Of Living Australia"
                        , year = base::format(base::Sys.Date(), "%Y")
                        , doi = paste(fs::path(basename(dirname(doi)), basename(doi)), collapse = "; ")
        ) %>%
          utils::toBibtex()

        readr::write_lines(bib
                           , file = fs::path(dirname(save_file)
                                             , paste0(basename(dirname(save_file)), ".bib")
                           )
                           , append = TRUE
        )

      } else {

        message("problem with doi: galah entry not written to .bib")

      }

    }

    # remap ------
    temp <- remap_data_names(this_name = name
                             , df_to_remap = temp
                             , data_map = data_map
                             , out_file = save_file
                             , final_select = TRUE
                             , final_select_col = "bio_all"
                             , ...
    )

  } else {

    temp <- rio::import(save_file
                        , setclass = "tibble"
    )

  }

  return(temp)

}


