

#' Get data using `robis::occurrence()`
#'
#' Requires an aoi. i.e. aoi is not optional for this implementation of get_obis
#'
#' @param aoi Optional simple feature (sf). Used to limit the occurrences
#' returned via `obis::occurence()`
#' @param save_dir Character. Path to directory into which to save outputs. If
#' `NULL` results will be saved to `here::here("out", "ds", "obis")`. File will
#' be named `obis.parquet`
#' @param get_new Logical. If FALSE, will attempt to load from existing
#' `save_dir`.
#' @param name Character. `data_name` value in `envImport::data_map`
#' (or other `data_map`)
#' @param data_map Dataframe or NULL. Mapping of fields to retrieve. See example
#' `envImport::data_map`
#' @param filter_inconsistent Logical. If `TRUE`, inconsistencies between the
#' `occurrenceStatus` column and either `organismQuantity` or `individualCount`
#' are removed. e.g. a record with `occurrenceStatus == "ABSENT"` but
#' `individualCount == 1` would be filtered.
#' @param removes Named list. Names need to match column names in the result of
#' a call to `robis::occurrence()` (i.e. generally
#' [Darwin core](https://dwc.tdwg.org/) column names). Levels within each name
#' are matched and, if present, removed. Note that this filtering occurs _after_
#' download so it does not save on download time and can also be done after a
#' call to `get_obis()`. It does enable filtering on columns that are not passed
#' through to the return value as they are not in the `data_map`.
#' @param ... Passed to `envImport::file_prep()`
#'
#' @return Dataframe of occurrences and file saved to `save_dir`.
#' @export
#'
#' @example inst/examples/get_obis_ex.R
get_obis <- function(aoi = NULL
                     , save_dir = NULL
                     , get_new = FALSE
                     , name = "obis"
                     , data_map = NULL
                     , filter_inconsistent = TRUE
                     , removes = list(basisOfRecord = c("LIVING_SPECIMEN", "FOSSIL_SPECIMEN", "MATERIAL_CITATION"))
                     , ...
                     ) {

  # save file -------
  save_file <- file_prep(save_dir, name, ...)

  # run the qry ---------
  get_new <- if(!file.exists(save_file)) TRUE else get_new

  if(get_new) {

    if(!is.null(aoi)) {

      aoi_wkt <- aoi |>
        sf::st_transform(crs = 4326) |>
        sf::st_bbox() |>
        sf::st_as_sfc() |>
        sf::st_as_text()

    } else {

      stop("aoi argument cannot be NULL in get_obis")

    }

    if(nrow(temp)) {

      temp <- robis::occurrence(geometry = aoi_wkt)

      # removes ------
      if(!is.null(removes)) {

        for(i in seq_along(removes)) {

          col <- names(removes)[[i]]

          if(col %in% names(temp)) {

            temp <- temp[which(!temp[[col]] %in% removes[[i]]),]

          } else {

            warning(col
                    , " is not a column so will not be filtered of values: "
                    , envFunc::vec_to_sentence(removes[[i]])
                    )

          }

        }

      }

      temp <- envClean::filter_geo_range(temp %>%
                                           dplyr::filter(!is.na(decimalLongitude)
                                                         , !is.na(decimalLatitude)
                                                         )
                                         , use_aoi = aoi
                                         , x = "decimalLongitude"
                                         , y = "decimalLatitude"
                                         , crs_df = 4326
                                         )


      ## filter_inconsistent --------
      if(filter_inconsistent) {

        temp <- temp |>
          dplyr::filter(!(occurrenceStatus == "ABSENT" &
                            !is.na(organismQuantity) &
                            organismQuantity > 0
                          )
                        ) |>
          dplyr::filter(!(occurrenceStatus == "PRESENT" &
                            !is.na(organismQuantity) &
                            organismQuantity == 0
                          )
                        )

      }

      # remap ------
      temp <- remap_data_names(this_name = name
                               , df_to_remap = temp
                               , data_map = data_map
                               , out_file = save_file
                               , final_select = TRUE
                               , final_select_col = "bio_all"
                               , ...
                               ) |>
        dplyr::mutate(rel_metres = as.numeric(rel_metres))

    } else {

      message("No results for ", name)

      temp <- NULL

    }

  } else {

    temp <- rio::import(save_file
                        , setclass = "tibble"
    )

  }

  return(temp)

}


