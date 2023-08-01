

  get_sat_sits <- function(...
                           , roi_name = NULL
                           , force_new = FALSE
                           ) {

    dots <- list(...)

    satellite <- tolower(stringr::str_extract(dots$collection
                                              , "^.*?(?=\\-)"
                                              )
                         )

    roi_name <- if(is.null(roi_name)) {

      match.call(expand.dots = FALSE)$...$roi

    } else roi_name

    dots$data_dir <- fs::path(dots$data_dir
                              , satellite
                              , paste(roi_name
                                      , dots$period
                                      , dots$res
                                      , sep = "-"
                                      )
                              )

    fs::dir_create(dots$data_dir)

    dots$output_dir <- dots$data_dir # output_dir needed in sits_regularize


    # aoi-------

    if(exists("roi", dots)) {

      if(inherits(dots$roi, "SpatRaster")) {

        dots$roi <- sf::st_bbox(dots$roi) %>%
          sf::st_as_sfc() %>%
          sf::st_transform(crs = 4326)

      } else if(inherits(dots$roi, "sf")) {

        dots$roi <- dots$roi %>%
          sf::st_transform(crs = 4326)

      }

    }


    # irregular cube -----

    # This just finds images that match the roi and time span (no image downloads)

    cube_file <- fs::path(ras_dir
                          , "cube.rds"
                          )

    get_new <- if(!file.exists(cube_file)) TRUE else force_new

    if(get_new) {

      cube <- R.utils::doCall(sits::sits_cube
                              , args = dots
                              )

      rio::export(cube
                  , cube_file
                  )

    } else {

      cube <- rio::import(cube_file)

    }

    # regular cube-------

    # This is where downloads happen

    reg_cube <- R.utils::doCall(sits::sits_regularize
                                , cube = cube
                                , args = dots
                                )


  }

