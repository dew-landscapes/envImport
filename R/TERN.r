

#' Get occurrence records from TERN
#'
#' TERN is the
#' [Terrestrial Ecosystem Research Network](https://www.tern.org.au/).
#'
#' Built on the `ausplotsR::get_ausplots` function. Summarises taxa cover data
#' within a visit. Optionally adds
#' [Muir](https://museum.wa.gov.au/sites/default/files/2.%20Muir_5.pdf) codes.
#'
#' @param out_file Character. Full path to save output data.
#' @param data_map Dataframe. Mapping of GBIF fields to retrieve and their new
#' names
#' @param poly sf. Polygon defining area of interest for retrieving data.
#' Acutally turned into `sf::st_bbox(poly)` before any `poly_buf`.
#' @param poly_buf Numeric. Distance to buffer `poly` via `sf::st_buffer` `dist`
#' argument.
#' @param make_muir Logical. If true Muir codes a column `MUIRCODE` is added to
#' the dataframe.
#'
#' @return Dataframe.
#' @export
#'
#' @examples
  get_TERN <- function(out_file = NULL
                       , data_map = NULL
                       , poly
                       , poly_buf
                       , make_muir = TRUE
                       ) {


    # Define area to query

    bb <- poly %>%
      sf::st_buffer(poly_buf) %>%
      sf::st_transform(crs = 4326) %>%
      sf::st_bbox()

    # run query

    plots <- ausplotsR::get_ausplots(bounding_box = bb[c("xmin"
                                                         , "xmax"
                                                         , "ymin"
                                                         , "ymax"
                                                         )
                                                       ]
                                     )

    # initial data mung

    temp <- plots$site.info %>%
      tibble::as_tibble() %>%
      dplyr::inner_join(tibble::as_tibble(plots$veg.PI) %>%
                          dplyr::filter(!is.na(herbarium_determination)) %>%
                          dplyr::add_count(site_unique, name = "points")
                        )

    # What names to grab before writing results?

    if(is.null(data_map)) {

      data_map <- data.frame(t(c("TERN"
                                 , names(temp)[grepl("site|date|herbarium"
                                                     , names(temp)
                                                     )
                                               ]
                                 )
                               )
                             ) %>%
        stats::setNames(c("data_name"
                          , names(temp)[grepl("site|date|herbarium"
                                              , names(temp)
                                              )
                                        ]
                          )
                        )

    }

    selectNames <- data_map %>%
      dplyr::filter(data_name == "TERN") %>%
      unlist(., use.names = FALSE) %>%
      stats::na.omit()


    # pull results together
    res <- temp %>%
      tidyr::nest(data = -c(tidyselect::any_of(na.omit(selectNames))
                            , points
                            , plot_dimensions
                            )
                  ) %>%
      dplyr::mutate(growth_form = purrr::map_chr(data
                                                 , ~names(which.max(table(.$growth_form)))
                                                 )
                    , ht = purrr::map_dbl(data
                                          , ~ mean(.$height
                                                   , na.rm = TRUE
                                                   )
                                          )
                    , COVER = purrr::map2_dbl(data
                                              , points
                                              , ~ 100 * nrow(.x) / .y
                                              )
                    , visit_start_date = as.POSIXct(visit_start_date
                                                    , format = "%Y-%m-%dT%H:%M:%S"
                                                    )
                    , quadX = as.numeric(gsub("\\s"
                                              , ""
                                              , stringr::str_extract(plot_dimensions
                                                            , "\\d{2,4} "
                                                            )
                                              )
                                         )
                    , quadY = as.numeric(gsub("\\s"
                                              , ""
                                              , stringr::str_extract(plot_dimensions
                                                            , " \\d{2,4}"
                                                            )
                                              )
                                         )
                    ) %>%
      dplyr::select(where(Negate(is.list)))

    if(make_muir) {

      luGF <- tibble::tribble(
        ~growth_form, ~LifeForm_Type
        , "Bryophyte", "MO"
        , "Chenopod", "S"
        , "Epiphyte", "MI"
        , "Fern", "X"
        , "Forb", "J"
        , "Grass-tree", "S"
        , "Heath-shrub", "S"
        , "Hummock grass", "H"
        , "Rush", "G"
        , "Sedge", "Sedge"
        , "Shrub", "S"
        , "Shrub Mallee", "K"
        , "Tree Mallee", "K"
        , "Tree/Palm", "T"
        , "Tussock grass", "G"
        , "Vine", "V"
      )

      res <- res %>%
        dplyr::left_join(luGF) %>%
        dplyr::mutate( MUIRCODE = LifeForm_Type
                      , MUIRCODE = dplyr::if_else(MUIRCODE == "S"
                                         , dplyr::if_else(ht > 2
                                                   , "S"
                                                   , dplyr::if_else(ht > 1.5
                                                             , "SA"
                                                             , dplyr::if_else(ht > 1
                                                                       , "SB"
                                                                       , dplyr::if_else(ht > 0.5
                                                                                 , "SC"
                                                                                 , "SD"
                                                                                 )
                                                                       )
                                                             )
                                                   )
                                         , MUIRCODE
                                         )
                      , MUIRCODE = dplyr::if_else(MUIRCODE == "T"
                                           , dplyr::if_else(ht > 30
                                                     , "T"
                                                     , dplyr::if_else(ht > 15
                                                               , "M"
                                                               , dplyr::if_else(ht > 5
                                                                         , "LA"
                                                                         , "LB"
                                                                         )
                                                               )
                                                     )
                                           , MUIRCODE
                                           )
                      , MUIRCODE = dplyr::if_else(MUIRCODE == "K"
                                           , dplyr::if_else(ht > 3
                                                     , "KT"
                                                     , "KS"
                                                     )
                                           , MUIRCODE
                                           )
                      , MUIRCODE = dplyr::if_else(MUIRCODE == "G"
                                           , dplyr::if_else(ht > 0.5
                                                     , "GT"
                                                     , "GL"
                                                     )
                                           , MUIRCODE
                                           )
                      , MUIRCODE = dplyr::if_else(MUIRCODE == "Sedge"
                                           , dplyr::if_else(ht > 0.5
                                                     , "VT"
                                                     , "VL"
                                                     )
                                           , MUIRCODE
                                           )
                      )

    }

    if(!is.null(out_file)) {

      rio::export(temp
                  , out_file
                  )

    }

    return(temp)

  }
