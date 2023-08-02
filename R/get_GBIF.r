
#' Get new occurrence record set from GBIF and save as `.rds`
#'
#' GBIF is the
#' [Global Biodiversity Information Facility](https://www.gbif.org/).
#'
#' Uses various [`rgbif`](https://docs.ropensci.org/rgbif/index.html) functions
#' to return a dataframe of occurence records. Requires
#' [gbif credentials](https://docs.ropensci.org/rgbif/articles/gbif_credentials.html).
#'
#'
#' @param taxon_key Numeric The primary id number used in GBIF to id a species
#' (or some higher group). These are the id numbers found in the GBIF backbone
#' taxonomy. See
#' [understanding GBIF taxonomic keys](https://discourse.gbif.org/t/understanding-gbif-taxonomic-keys-usagekey-taxonkey-specieskey/3045)
#' @param aoi sf defining area of interest. Will be converted to bounding box.
#' @param min_year Numeric. gbif data will be filtered to >= min_year
#' @param ... Other arguments passed to `rgbif::occ_download`
#' @param save_file Character. File path into which to save outputs. If `null`
#' results will be saved to `fs::path("out", "ds", "gbif", paste0(t"gbif_raw.rds")`.
#' @param get_new Logical. If `FALSE` will attempt to load data from previously
#' saved results.
#' @param request_wait Integer. Time in seconds to wait between
#' `rgbif::occ_download_meta()` requests. Used by `rgbif::occ_download_wait()`
#' `status_ping` argument.
#' @param return_results Logical. If true, results will be returned as a tibble
#' (as well as written to file). If false, results are only written to file.
#'
#' @return Dataframe, `save_file` with occurence records, `gbif_data_ref.bib`
#' (in the same directory as `save_file`) and full GBIF download.
#' @export
#'
#' @examples
#' \dontrun{
#' # Australian Bustard Ardeotis australis with year == 1901. Returns 7 records
#' # but can take a while to run depending on GBIF waiting times.
#' get_GBIF(taxon_key = 2474903, rgbif::pred("year", 1901))
#'}
  get_GBIF <- function(taxon_key
                       , aoi = NULL
                       , min_year = NULL
                       , ...
                       , save_file = NULL
                       , get_new = FALSE
                       , request_wait = 20
                       ) {

    # Increase the time allowed to access URLs
    RCurl::curlSetOpt(timeout = 100000)

    aoi_name <- deparse(substitute(aoi))

    if(is.null(save_file)) {

      save_file <- fs::path("out"
                            , "ds"
                            , "GBIF"
                            , taxon_key
                            , paste0(taxon_key
                                     , if(!is.null(aoi)) paste0("_"
                                                                , aoi_name
                                                                )
                                     , "_raw.rds"
                                     )
                            )

    }

    save_dir <- dirname(save_file)

    fs::dir_create(save_dir)

    get_new <- if(!file.exists(save_file)) TRUE else get_new

    if(get_new) {

      if(!is.null(aoi)) {

        aoiWKT <- aoi %>%
          sf::st_bbox() %>%
          sf::st_as_sfc() %>%
          sf::st_geometry() %>%
          sf::st_transform(crs = 4326) %>%
          sf::st_as_text()

        gbif_download <- rgbif::occ_download(
          rgbif::pred_and(rgbif::pred("HAS_GEOSPATIAL_ISSUE"
                                      , FALSE
                                      )
                          , rgbif::pred("HAS_COORDINATE"
                                        , TRUE
                                        )
                          , rgbif::pred("OCCURRENCE_STATUS"
                                        , "PRESENT"
                                        )
                          , rgbif::pred_not(rgbif::pred_in("BASIS_OF_RECORD"
                                                           , c("FOSSIL_SPECIMEN"
                                                               , "LIVING_SPECIMEN"
                                                               )
                                                           )
                                            )
                          )
          , rgbif::pred_in("taxonKey"
                           , taxon_key
                           )
          , rgbif::pred_gte("year"
                            , min_year
                            )
          , rgbif::pred_within(aoiWKT)
          , ...
          )

      } else {

        gbif_download <- rgbif::occ_download(
          rgbif::pred_and(rgbif::pred("HAS_GEOSPATIAL_ISSUE"
                                      , FALSE
                                      )
                          , rgbif::pred("HAS_COORDINATE"
                                        , TRUE
                                        )
                          , rgbif::pred("OCCURRENCE_STATUS"
                                        , "PRESENT"
                                        )
                          , rgbif::pred_not(rgbif::pred_in("BASIS_OF_RECORD"
                                                           , c("FOSSIL_SPECIMEN"
                                                               , "LIVING_SPECIMEN"
                                                               )
                                                           )
                                            )
                          )
          , rgbif::pred_in("taxonKey"
                           , taxon_key
                           )
          , rgbif::pred_gte("year"
                            , min_year
                            )
          , ...
          )

      }

      rgbif::occ_download_wait(gbif_download
                               , status_ping = request_wait
                               )

      meta <- rgbif::occ_download_meta(gbif_download)

      gbif_download <- rgbif::occ_download_get(gbif_download
                                               , path = save_dir
                                               , overwrite = TRUE
                                               )

      temp <- rgbif::occ_download_import(gbif_download) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(doi = meta$doi)

      rio::export(temp
                  , save_file
                  )

      # Make a reference for the download

      bib_file <- fs::path(save_dir
                           , paste0(meta$key
                                    , ".bib"
                                    )
                           )


      ref <- RefManageR::GetBibEntryWithDOI(meta$doi
                                           , temp.file = bib_file
                                           , delete.file = FALSE
                                           )


      ref <- readr::read_lines(bib_file)

      ref[1] <- paste0("@misc{"
                       , meta$key
                       , "}"
                       )

      readr::write_lines(ref, bib_file)

    } else {

      temp <- rio::import(save_file)

    }

    return(temp)

  }

