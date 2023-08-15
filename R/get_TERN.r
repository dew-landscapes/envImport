

#' Get occurrence records from TERN
#'
#' TERN is the
#' [Terrestrial Ecosystem Research Network](https://www.tern.org.au/). Built on
#' the `ausplotsR::get_ausplots` function.
#'
#' @param save_file Character. File path into which to save outputs. If `null`
#' results will be saved to `fs::path("out", "ds", "name", "name_raw.rds")`
#' where `name` is the data source name.
#' @param aoi sf. Polygon defining area of interest for retrieving data.
#' Used as `sf::st_bbox(aoi)`.
#' @param get_new Logical. If FALSE, will attempt to load from existing
#' `save_file`.
#' @param ... Passed to `ausplotsR::get_ausplots()`.
#'
#' @return Object and `save_file`
#' @export
#'
#' @examples
  get_TERN <- function(save_file = NULL
                       , aoi
                       , get_new = FALSE
                       , ...
                       ) {

    name <- "TERN"

    if(is.null(save_file)) {

      save_file <- fs::path("out"
                           , "ds"
                           , name
                           , paste0(name
                                    , "_raw.rds"
                                    )
                           )

    }

    # run query
    get_new <- if(!file.exists(save_file)) TRUE else get_new

    if(get_new) {

      fs::dir_create(dirname(save_file))

      # Define area to query
      bb <- aoi %>%
        sf::st_transform(crs = 4326) %>%
        sf::st_bbox()

      temp <- ausplotsR::get_ausplots(bounding_box = bb[c("xmin"
                                                           , "xmax"
                                                           , "ymin"
                                                           , "ymax"
                                                           )
                                                         ]
                                      , ...
                                       )

      rio::export(temp
                  , save_file
                  )

    } else {

      temp <- rio::import(save_file)

    }

    return(temp)

  }
