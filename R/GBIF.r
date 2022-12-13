
#' Get new occurrence record set from GBIF
#'
#' GBIF is the
#' [Global Biodiversity Information Facility](https://www.gbif.org/).
#'
#' Uses various [`rgbif`](https://docs.ropensci.org/rgbif/index.html) functions
#' to return a dataframe of occurence records. Requires
#' [gbif credentials](https://docs.ropensci.org/rgbif/articles/gbif_credentials.html).
#' If`out_file` is provided will save the dataframe there with the full results
#' in `fs::path(dirname(out_file), "GBIF")`. If a data_map is provided, only
#' fields specified in the data_map are returned. Otherwise all fields are
#' returned.
#'
#' @param taxon_key Numeric The primary id number used in GBIF to id a species
#' (or some higher group). These are the id numbers found in the GBIF backbone
#' taxonomy. See
#' [understanding GBIF taxonomic keys](https://discourse.gbif.org/t/understanding-gbif-taxonomic-keys-usagekey-taxonkey-specieskey/3045)
#' @param ... Other arguments passed to `rgbif::occ_download`
#' @param poly sf. Polygon defining area of interest for retrieving data.
#' Acutally turned into `sf::st_bbox(poly)` before any `poly_buf` and converting
#' to WKT using
#' @param poly_buf Numeric. Distance to buffer `poly` via `sf::st_buffer` `dist`
#' argument.
#' @param out_file Character. Path to save output data.
#' @param data_map Dataframe. Mapping of GBIF fields to retrieve and their new
#' names
#' @param wait_time Time to wait between running `rgbif::occ_download_meta` to
#' check status. Will keep trying while status == "RUNNING".
#'
#'
#' @return If `out_file` is supplied: dataframe, `rio::export(results, out_file)`,
#' and `gbif_data_ref.bib` (in the same directory as `out_file`); else dataframe.
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
                       , out_file = NULL
                       , data_map = NULL
                       , wait_time = 10
                       ) {

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
                                #, ...
                                )

    meta <- rgbif::occ_download_meta(temp)

    attempt <- 1

    start_time <- Sys.time()

    while(meta$status %in% c("PREPARING", "RUNNING")) {

      cat(paste0("Status = ", meta$status, "\n"
                 , "Attempt = ", attempt, "\n"
                 , "Each wait time = ", wait_time, " seconds\n"
                 , "Total time = ", Sys.time() - start_time
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



    if(!is.null(out_file)) {

      save_loc <- fs::path(dirname(out_file)
                           , "GBIF"
                           )

    } else {

      save_loc <- fs::path(tempdir()
                           , "GBIF"
                           )

    }

    fs::dir_create(save_loc)

    getDownload <- rgbif::occ_download_get(temp
                                           , path = save_loc
                                           , overwrite = TRUE
                                           )


    # MOVE THIS SECTION TO NEW FUNCTION TO RETRIEVE A DOWNLOAD VIA KEY
    if(FALSE) {

      #------get gbif meta data---------

      info <- rgbif::occ_download_list(Sys.getenv("GBIF_user")
                                       , Sys.getenv("GBIF_pwd")
                                       )$results %>%
        dplyr::mutate(created = lubridate::ymd_hms(created
                                                   , tz = Sys.timezone()
                                                   )
                      , taxon_key = purrr::map(request.predicate.predicates
                                               , c("value")
                                               )
                      , taxon_key = purrr::map(taxon_key
                                               , 1
                                               )
                      ) %>%
        tidyr::unnest(cols = c(taxon_key)) %>%
        dplyr::filter(taxon_key == taxon_key
                      , created == max(created)
                      )

    }

    #------bib-------

    bib_file <- fs::path(save_loc, "gbif_data_ref.bib")

    ref <- RefManageR::GetBibEntryWithDOI(meta$doi
                                          , temp.file = bib_file
                                          , delete.file = FALSE
                                          )

    # Make a reference for the download
    ref <- readr::read_lines(bib_file)
    ref[1] <- paste0("@misc{GBIFRef,")
    readr::write_lines(ref, bib_file)



    #-------unzip gbif data--------

    utils::unzip(fs::path(save_loc
                          , paste0(meta$key[[1]]
                                   , ".zip"
                                   )
                          )
                 , exdir = fs::path(save_loc
                                    , meta$key[[1]]
                                    )
                 )

    rawGBIF <- data.table::fread(fs::path(save_loc
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

    # What names to grab before writing results?
    if(is.null(data_map)) {

      data_map <- data.frame(t(c("GBIF", names(rawGBIF)))) %>%
        stats::setNames(c("data_name", names(rawGBIF)))

    }

    selectNames <- data_map %>%
      dplyr::filter(data_name == "GBIF") %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit()

    dat <- rawGBIF %>%
      dplyr::select(tidyselect::any_of(selectNames)) %>%
      dplyr::filter(!is.na(eventDate)
                    , !is.na(decimalLatitude)
                    , !is.na(decimalLongitude)
                    , !is.na(species)
                    , species != ""
                    )

    if(!is.null(out_file)) {

      rio::export(dat
                  , out_file
                  )

    }

    return(dat)

  }

