

#' Get data using `galah::atlas_occurrences()`
#'
#'
#'
#' @param aoi Optional simple feature (sf). Used to limit the occurrences
#' returned via `galah::galah_geolocate()`. Note the limitations given in
#' `galah_geolocate`, 'Shapefiles must be simple to be accepted by the ALA....If
#' type = "polygon", WKT strings longer than 10000 characters and sf objects
#' with more than 500 vertices will not be accepted by the ALA."
#' @param save_dir Character. Path to directory into which to save outputs. If
#' `NULL` results will be saved to `here::here("out", "ds", "galah")`. File will
#' be named `galah.parquet`
#' @param get_new Logical. If FALSE, will attempt to load from existing
#' `save_dir`.
#' @param name Character. `data_name` value in `envImport::data_map`
#' (or other `data_map`)
#' @param data_map Dataframe or NULL. Mapping of fields to retrieve. See example
#' `envImport::data_map`
#' @param node Character. Name of atlas to use (see `galah::atlas_occurrences()`).
#' Doesn't seem to work with node = "GBIF" and untested on other nodes.
#' @param qry `NULL` or an object of class data_request, created using
#' `galah::galah_call()`. NOTE: do not include any `galah::atlas_occurrences()`
#' in the `qry`, this is called by get_galah.
#' @param check_rel_metres Logical. Ensure that `coordinateUncertaintyInMetres`
#' is no less than `generalisationInMetres`? Only relevant if both columns are
#' returned by `qry`
#' @param filter_inconsistent Logical. If `TRUE`, inconsistencies between the
#' `occurrenceStatus` column and either `organismQuantity` are filtered
#' (removed). e.g. a record with `occurrenceStatus == "ABSENT"` but
#' `organismQuantity == 10` would be filtered. Only relevant if both columns are
#' returned by `qry`
#' @param ... Passed to `envImport::file_prep()` and
#' `envImport::remap_data_names()`
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

      make_doi <- galah::galah_config()$user$download_reason_id != 10

      ## initiate-------
      if(is.null(qry)) {

        qry <- galah::request_data(type = "occurrences"
                                   , mint_doi = make_doi
                                   )

      }

      ## aoi------
      if(!is.null(aoi)) qry <- qry %>%
          galah::galah_geolocate(aoi)


      ## check records--------
      records <- qry %>%
        galah::atlas_counts()

      if(records$count[[1]] > 50000000) {

        stop("Returned records ("
             , base::format(records$count[[1]], big.mark = ",")
             , ") would exceed limit of 50,000,000"
             )

      }


      message(base::format(records$count, big.mark = ",")
              , " records available from galah via "
              , galah::galah_config()$atlas$acronym
              , " node, based on this query"
              )

      ## select-------
      if(is.null(data_map)) {

        get_groups <- c("basic", "event", "taxonomy")

        qry <- qry %>%
          galah::galah_select(group = get_groups)

        data_map <- tibble::tibble(col = unique(unlist(purrr::map(get_groups, galah:::preset_groups)))) %>%
          dplyr::mutate(!!rlang::ensym(name) := col)

      } else {

        all_fields <- unique(galah::show_all("fields")$id)

        select_names <- data_map$galah[data_map$galah %in% all_fields]

        qry <- qry %>%
          galah::select(recordID # inc recordID here: see https://github.com/AtlasOfLivingAustralia/galah-R/issues/239
                        , tidyselect::all_of(select_names)
                        )

      }

      ## retrieve ------
      temp <- qry %>%
        dplyr::collect()

      ## rel_metres-------
      if(all(check_rel_metres, c("coordinateUncertaintyInMeters", "generalisationInMetres") %in% names(temp))) {

        temp <- temp %>%
          dplyr::mutate(cuim = coordinateUncertaintyInMeters
                        , gim = generalisationInMetres
                        ) %>%
          dplyr::mutate(coordinateUncertaintyInMeters = dplyr::case_when(!is.na(gim) & gim > cuim ~ gim
                                                                         , TRUE ~ cuim
                                                                         )
                        )

      }

      ## filter_inconsistent --------
      if(all(filter_inconsistent, c("organismQuantity", "occurrenceStatus") %in% names(temp))) {

        temp <- temp %>%
            dplyr::filter(!(occurrenceStatus == "ABSENT" &
                              isTRUE(organismQuantity > 0)
                            )
                          ) %>%
            dplyr::filter(!(occurrenceStatus == "PRESENT" &
                            isTRUE(organismQuantity == 0)
                            )
                          )

      }


      # bib -------
      if(make_doi) {

        doi <- attr(temp, "doi")

        if(is.character(doi)) {

          bib <- bibentry(bibtype = "MISC"
                          , key = "galah"
                          , title = "Occurrence download data"
                          , author = utils::person("Atlas Of Living Australia")
                          , publisher = "Atlas Of Living Australia"
                          , year = base::format(base::Sys.Date(), "%Y")
                          , doi = fs::path(basename(dirname(doi)), basename(doi))
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


