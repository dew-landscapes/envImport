

#' Get occurrence records from TERN
#'
#' TERN is the
#' [Terrestrial Ecosystem Research Network](https://www.tern.org.au/). Built on
#' the `ausplotsR::get_ausplots` function.
#'
#' @param aoi sf. Polygon defining area of interest for retrieving data.
#' Used as `sf::st_bbox(aoi)`.
#' @param save_dir Character. Path to directory into which to save outputs. If
#' `null` results will be saved to `fs::path("out", "ds", "name")`. File will be
#' named `name_raw.rds`. `name` is the data source name.
#' @param get_new Logical. If FALSE, will attempt to load from existing
#' `save_file`.
#' @param ... Passed to `ausplotsR::get_ausplots()`.
#'
#' @return Object and tern_raw.rds in `save_dir`
#' @export
#'
#' @examples
  get_tern <- function(aoi
                       , save_dir = NULL
                       , get_new = FALSE
                       , name = "tern"
                       , ...
                       ) {

    save_file <- file_prep(save_dir, name)

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
