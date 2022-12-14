
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
#' @param ... Other arguments passed to `rgbif::occ_download`
#' @param save_file Character. File path into which to save outputs. If `null`
#' results will be saved to `fs::path("out", "ds", "name", "name_raw.rds")`
#' where `name` is the data source name.
#' @param poly sf. Polygon defining area of interest for retrieving data.
#' Acutally turned into `sf::st_bbox(poly)` before any `poly_buf` and converting
#' to WKT using
#' @param poly_buf Numeric. Distance to buffer `poly` via `sf::st_buffer` `dist`
#' argument.
#' @param get_new Logical. If `FALSE` will attempt to load data from previously
#' saved results.
#' @param wait_time Time to wait between running `rgbif::occ_download_meta` to
#' check status. Will keep trying while status == "RUNNING".
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
                       , ...
                       , poly = NULL
                       , poly_buf = 100000
                       , save_file = NULL
                       , wait_time = 10
                       , get_new = FALSE
                       ) {

    name <- "GBIF"

    if(is.null(save_file)) {

      save_file <- fs::path("out"
                            , "ds"
                            , name
                            , paste0(name
                                     , "_"
                                     , taxon_key
                                     , "_raw.rds"
                                     )
                            )

    }

    save_loc <- dirname(save_file)

    get_new <- if(!file.exists(save_file)) TRUE else get_new

    if(get_new) {

      if(wait_time < 10) wait_time = 10

      if(!is.null(poly)) {

        aoiWKT <- poly %>%
          sf::st_buffer(poly_buf) %>%
          sf::st_bbox() %>%
          sf::st_as_sfc() %>%
          sf::st_geometry() %>%
          sf::st_transform(crs = 4326) %>%
          sf::st_as_text()

      }

      temp <- rgbif::occ_download(rgbif::pred("taxonKey"
                                              , taxon_key
                                              )
                                  , rgbif::pred("hasCoordinate"
                                                , TRUE
                                                )
                                  , if(!is.null(poly)) rgbif::pred_within(aoiWKT)
                                  , ...
                                  )

      meta <- rgbif::occ_download_meta(temp)

      attempt <- 1

      start_time <- Sys.time()

      while(meta$status %in% c("PREPARING", "RUNNING")) {

        cat(paste0("Status = ", meta$status, "\n"
                   , "Attempt = ", attempt, "\n"
                   , "Each wait time = ", wait_time, " seconds\n"
                   , "Total time = ", Sys.time() - start_time, "\n"
                   )
            )

        Sys.sleep(wait_time)

        meta <- rgbif::occ_download_meta(temp)

        attempt <- attempt + 1

      }

      if(meta$status != "SUCCEEDED") {

        print(meta)

        stop("Status not equal to 'SUCCEEDED'")

      }

      fs::dir_create(save_loc)

      rgbif::occ_download_get(temp
                              , path = save_loc
                              , overwrite = TRUE
                              )

      utils::unzip(fs::path(save_loc
                            , paste0(meta$key[[1]]
                                     , ".zip"
                                     )
                            )
                   , exdir = fs::path(save_loc
                                      , meta$key[[1]]
                                      )
                   )

      temp <- data.table::fread(fs::path(save_loc
                                         , meta$key[[1]]
                                         , "occurrence.txt"
                                         )
                                ) %>%
        dtplyr::lazy_dt() %>%
        dplyr::filter(is.na(occurrenceStatus) | occurrenceStatus != "ABSENT") %>%
        dplyr::filter(is.na(individualCount) | individualCount > 0) %>%
        dplyr::filter(is.na(organismQuantity) | organismQuantity > 0) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(doi = meta$doi[[1]])

      rio::export(temp
                  , save_file
                  )

      # Make a reference for the download

      bib_file <- fs::path(save_loc
                           , "gbif_data_ref.bib"
                           )


      ref <- RefManageR::GetBibEntryWithDOI(meta$doi
                                           , temp.file = bib_file
                                           , delete.file = FALSE
                                           )


      ref <- readr::read_lines(bib_file)
      ref[1] <- paste0("@misc{GBIFRef,")
      readr::write_lines(ref, bib_file)

    } else {

      temp <- rio::import(save_file)

    }

    return(temp)

  }

