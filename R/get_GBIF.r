
#' Get an occurrence record set from GBIF
#'
#' Get new occurrence record set from GBIF and save as `.rds`. GBIF is the
#' [Global Biodiversity Information Facility](https://www.gbif.org/).
#'
#' Uses various [`rgbif`](https://docs.ropensci.org/rgbif/index.html) functions
#' to return a dataframe of occurence records. Requires
#' [gbif credentials](https://docs.ropensci.org/rgbif/articles/gbif_credentials.html).
#'
#' Any arguments to `rgbif::occ_download()` can be passed via `extra_prediates`.
#' For convenience, `aoi` can also be passed directly and internally it is
#' converted to a bounding box in appropriate lat/long and passed to
#' `rgbif::pred_within()` in WKT format.
#'
#' @param aoi sf defining area of interest.
#' @param save_dir Character. File path into which to save outputs. If `null`
#' results will be saved to `fs::path("out", "ds", "gbif")` as file
#' `gbif.rds`.
#' @param get_new Logical. If `FALSE` will attempt to load data from previously
#' saved results.
#' @param name Character or `NULL`. `data_name` value in `envImport::data_map`
#' (or other `data_map`). Required if `data_map` is not `NULL`
#' @param data_map Dataframe or `NULL.` Mapping of fields to retrieve. See example
#' `envImport::data_map`
#' @param predicates List. Any number of [gbif predicates](https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html#occ_download)
#' @param request_wait Integer. Time in seconds to wait between
#' `rgbif::occ_download_meta()` requests. Used by `rgbif::occ_download_wait()`
#' `status_ping` argument.
#' @param filter_inconsistent Logical. If `TRUE` inconsistencies between the
#' `occurrenceStatus` column and either `organismQuantity` or `individualCount`
#' are removed. e.g. a record with `occurrenceStatus == "ABSENT"` but
#' `individualCount == 1` would be filtered.
#' @param filter_NA_date Logical. Filter if `is.na(eventDate)`.
#' @param occ_char Logical. If true, occ_derivation will be coerced to character
#' (to match other data sources).
#' @param adj_spa_rel Logical. If true, an attempt will be made to check
#' `coordinateUncertaintyInMeters` against: information in `informationWithheld.` If
#' `informationWithheld` contains "Coordinate uncertainty increased to",
#' `readr::parse_number()` is used to retrieve that number, which is then used
#' to replace any value in `coordinateUncertaintyInMeters`; and if the column
#' `issue` contains `COORDINATE_UNCERTAINTY_METERS_INVALID`,
#' `coordinateUncertaintyInMeters` is limited to 10000 or greater.
#' @param previous_key Character. e.g. `0092123-240506114902167`. If provided,
#' an attempt will be made to load (or download) a previous query of occurrence
#' data.
#' @param ... Passed to `envImport::file_prep()`
#'
#' @return Dataframe of occurrences, full download (as key.zip) in `save_dir`
#' and file saved to `save_dir` as `gbif.parquet`.
#' @export
#'
#' @example inst/examples/get_gbif_ex.R
#'
  get_gbif <- function(aoi = NULL
                       , save_dir = NULL
                       , get_new = FALSE
                       , data_map = NULL
                       , predicates = NULL
                       , request_wait = 20
                       , name = "gbif"
                       , filter_inconsistent = TRUE
                       , filter_NA_date = TRUE
                       , occ_char = TRUE
                       , adj_spa_rel = TRUE
                       , previous_key = NULL
                       , ...
                       ) {

    save_file <- file_prep(save_dir, name, ...)

    get_new <- if(!file.exists(save_file)) TRUE else get_new

    if(get_new) {

      if(is.null(previous_key)) {

        # Increase the time allowed to access URLs
        RCurl::curlSetOpt(timeout = 100000)

        # occ_download ------
        if(!is.null(aoi)) {

          aoiWKT <- aoi %>%
            sf::st_transform(crs = 4326) %>%
            sf::st_bbox() %>%
            sf::st_as_sfc() %>%
            sf::st_as_text()

          next_year <- as.numeric(format(Sys.Date(), "%Y")) + 1
          dummy_pred <- rgbif::pred_lt("year", next_year)

          gbif_download <- rgbif::occ_download(
            rgbif::pred_and(rgbif::pred("HAS_GEOSPATIAL_ISSUE"
                                        , FALSE
                                        )
                            , rgbif::pred("HAS_COORDINATE"
                                          , TRUE
                                          )
                            , rgbif::pred_not(rgbif::pred_in("BASIS_OF_RECORD"
                                                             , c("FOSSIL_SPECIMEN"
                                                                 , "LIVING_SPECIMEN"
                                                                 )
                                                             )
                                              )
                            )
            , rgbif::pred_within(aoiWKT)
            , if(!is.null(predicates)) predicates else dummy_pred
            )

        } else {

          gbif_download <- rgbif::occ_download(
            rgbif::pred_and(rgbif::pred("HAS_GEOSPATIAL_ISSUE"
                                        , FALSE
                                        )
                            , rgbif::pred("HAS_COORDINATE"
                                          , TRUE
                                          )
                             , rgbif::pred_not(rgbif::pred_in("BASIS_OF_RECORD"
                                                             , c("FOSSIL_SPECIMEN"
                                                                 , "LIVING_SPECIMEN"
                                                                 )
                                                             )
                                              )
                            )
            , if(!is.null(predicates)) predicates else dummy_pred
            )

        }

        # wait ------
        rgbif::occ_download_wait(gbif_download
                                 , status_ping = request_wait
                                 )


      } else {

        gbif_download <- previous_key

      }

      # meta-------
      meta <- rgbif::occ_download_meta(gbif_download)

      # get -------
      gbif_download <- rgbif::occ_download_get(gbif_download
                                               , path = dirname(save_file)
                                               , overwrite = FALSE
                                               )

      # build output -------

      temp <- rgbif::occ_download_import(gbif_download) %>%
        {if(filter_NA_date) (.) %>%
            dplyr::filter(!is.na(eventDate)) else (.)
          } %>%
        {if(filter_inconsistent) (.) %>%
            dplyr::filter(!(occurrenceStatus == "ABSENT" &
                              !is.na(organismQuantity) &
                              organismQuantity > 0
                            )
                          ) %>%
            dplyr::filter(!(occurrenceStatus == "PRESENT" &
                            !is.na(organismQuantity) &
                            organismQuantity == 0
                            )
                          ) %>%
            dplyr::filter(!(occurrenceStatus == "PRESENT" &
                              !is.na(individualCount) &
                              individualCount == 0
                            )
                          ) %>%
            dplyr::filter(!(occurrenceStatus == "ABSENT" &
                              !is.na(individualCount) &
                              individualCount > 0
                            )
                          ) else (.)
          } %>%
        {if(occ_char) (.) %>%
            dplyr::mutate(organismQuantity = as.character(organismQuantity)) else (.)
          } %>%
        {if(adj_spa_rel) (.) %>%
            dplyr::mutate(coordinateUncertaintyInMeters_adj = dplyr::case_when(grepl("Coordinate uncertainty increased to"
                                                                                     , as.character(informationWithheld)
                                                                                     ) ~ readr::parse_number(as.character(informationWithheld))
                                                                               , grepl("COORDINATE_UNCERTAINTY_METERS_INVALID"
                                                                                       , issue
                                                                                       ) & coordinateUncertaintyInMeters < 10000 ~ 10000
                                                                           , TRUE ~ coordinateUncertaintyInMeters
                                                                           )
            ) else (.)
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

      # .bib -------
      bib_file <- fs::path(fs::path(dirname(save_file))
                           , "gbif.bib"
                           )


      bib <- RefManageR::GetBibEntryWithDOI(meta$doi
                                           , temp.file = bib_file
                                           , delete.file = TRUE
                                           ) %>%
        RefManageR::toBiblatex()

      bib[1] <- paste0("@misc{gbif,")

      readr::write_lines(bib
                         , file = fs::path(dirname(save_file)
                                           , paste0(basename(dirname(save_file)), ".bib")
                                           )
                         , append = TRUE
                         )

    } else {

      temp <- rio::import(save_file
                          , setclass = "tibble"
                          )

    }

    return(temp)

  }

