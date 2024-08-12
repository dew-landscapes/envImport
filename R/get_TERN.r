

#' Get occurrence records from TERN
#'
#' TERN is the
#' [Terrestrial Ecosystem Research Network](https://www.tern.org.au/). Built on
#' the `ausplotsR::get_ausplots` function.
#'
#' @param aoi sf. Polygon defining area of interest for retrieving data.
#' Used as `sf::st_bbox(aoi)`.
#' @param save_dir Character. Path to directory into which to save outputs. If
#' `null` results will be saved to `here::here("out", "ds", "tern")`. File will be
#' named `tern_raw.parquet`
#' @param get_new Logical. If FALSE, will attempt to load from existing
#' `save_dir`.
#' @param name Character. `data_name` value in `envImport::data_map`
#' (or other `data_map`)
#' @param data_map Dataframe or NULL. Mapping of fields to retrieve. See example
#' `envImport::data_map`
#' @param m_kind,cover_type,species_name,strip_bryophytes Arguments required by
#' `ausplotsR::species_table()`
#' @param make_lifeform Logical. If true, the columns `growth_form` and
#' `height` in `obj$veg.PI` are used to estimate a lifeform for each taxa within
#' each unique site.
#' @param ... Passed to `envImport::file_prep()`
#'
#' @return Dataframe of occurrences and file saved to `save_dir`
#' @export
#'
#' @example inst/examples/get_tern_ex.R
  get_tern <- function(aoi
                       , save_dir = NULL
                       , get_new = FALSE
                       , name = "tern"
                       , data_map = NULL
                       , m_kind = "percent_cover"
                       , cover_type = "PFC"
                       , species_name = "SN"
                       , strip_bryophytes = FALSE
                       , make_lifeform = TRUE
                       , ...
                       ) {

    save_file <- file_prep(save_dir, name, ...)

    # run query
    get_new <- if(!file.exists(save_file)) TRUE else get_new

    if(get_new) {

      # Define area to query
      bb <- aoi %>%
        sf::st_transform(crs = 4326) %>%
        sf::st_bbox()

      tern_data <- ausplotsR::get_ausplots(bounding_box = bb[c("xmin"
                                                               , "xmax"
                                                               , "ymin"
                                                               , "ymax"
                                                               )
                                                             ]
                                           , veg.PI = TRUE
                                           )

      if(nrow(tern_data$veg.PI) > 0) {

        select_names <- choose_names(data_map = data_map
                                     , this_name = name
                                     )

        species_col <- if(species_name == "SN") {

          "standardised_name"

        } else if(species_name == "HD") {

          "herbarium_determination"

        } else if (species_name == "GS") {

          "genus_species"

        }

        all_names <- c(select_names$value
                      , species_col
                      ) %>%
          unique()

        temp <- ausplotsR::species_table(tern_data$veg.PI
                                         , m_kind = m_kind
                                         , cover_type = cover_type
                                         , species_name = species_name
                                         , strip_bryophytes = strip_bryophytes
                                         ) %>%
          tibble::as_tibble(rownames = "site_unique") %>%
          stats::setNames(gsub("\\.", " ", names(.))) %>%
          stats::setNames(stringr::str_squish(names(.))) %>%
          tidyr::pivot_longer(2:ncol(.)
                              , names_to = species_col
                              , values_to = "cover"
                              ) %>%
          dplyr::filter(cover > 0) %>%
          dplyr::left_join(tern_data$site.info) %>%
          dplyr::mutate(cover = cover / 100
                        , visit_start_date = as.POSIXct(visit_start_date
                                                  , format = "%Y-%m-%d"
                                                  )
                        , quadX = readr::parse_number(gsub("x.*|"
                                                           , ""
                                                           , plot_dimensions
                                                           )
                                                      )
                        , quadY = readr::parse_number(gsub(".*x"
                                                           , ""
                                                           , plot_dimensions
                                                           )
                                                      )
                        , quad_metres = quadX * quadY
                        , observer_veg = as.character(observer_veg)
                        )

        if(make_lifeform) {

          luGF <- tibble::tribble(
            ~growth_form, ~lifeform
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

          lf <- tern_data$veg.PI %>%
            dplyr::filter(!is.na(!!rlang::ensym(species_col))
                          , !grepl("NA|Na", !!rlang::ensym(species_col))
                          ) %>%
            tibble::as_tibble() %>%
            dplyr::select(growth_form
                          , height
                          , tidyselect::any_of(all_names)
                          ) %>%
            dplyr::group_by(site_unique, !!rlang::ensym(species_col)) %>%
            dplyr::summarise(growth_form = names(which.max(table(growth_form)))
                             , height = median(height)
                             ) %>%
            dplyr::ungroup() %>%
            dplyr::left_join(luGF) %>%
            dplyr::mutate(lifeform = dplyr::if_else(lifeform == "S"
                                                       , dplyr::if_else(height > 2
                                                                        , "S"
                                                                        , dplyr::if_else(height > 1.5
                                                                                         , "SA"
                                                                                         , dplyr::if_else(height > 1
                                                                                                          , "SB"
                                                                                                          , dplyr::if_else(height > 0.5
                                                                                                                           , "SC"
                                                                                                                           , "SD"
                                                                                                                           )
                                                                                                          )
                                                                                         )
                                                                        )
                                                       , lifeform
                                                       )
                           , lifeform = dplyr::if_else(lifeform == "T"
                                                       , dplyr::if_else(height > 30
                                                                        , "T"
                                                                        , dplyr::if_else(height > 15
                                                                                         , "M"
                                                                                         , dplyr::if_else(height > 5
                                                                                                          , "LA"
                                                                                                          , "LB"
                                                                                                          )
                                                                                         )
                                                                        )
                                                       , lifeform
                                                       )
                           , lifeform = dplyr::if_else(lifeform == "K"
                                                       , dplyr::if_else(height > 3
                                                                        , "KT"
                                                                        , "KS"
                                                                        )
                                                       , lifeform
                                                       )
                           , lifeform = dplyr::if_else(lifeform == "G"
                                                       , dplyr::if_else(height > 0.5
                                                                        , "GT"
                                                                        , "GL"
                                                                        )
                                                       , lifeform
                                                       )
                           , lifeform = dplyr::if_else(lifeform == "Sedge"
                                                       , dplyr::if_else(height > 0.5
                                                                        , "VT"
                                                                        , "VL"
                                                                        )
                                                       , lifeform
                                                       )
                           ) %>%
            dplyr::select(tidyselect::any_of(all_names)
                          , lifeform
                          )

          temp <- temp %>%
            dplyr::left_join(lf)

        }

        temp <- temp %>%
          dplyr::rename(species = !!rlang::ensym(species_col)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(kingdom = "Plantae")

        temp <- remap_data_names(this_name = name
                                 , df_to_remap = temp
                                 , data_map = data_map
                                 , out_file = save_file
                                 , previous = "move"
                                 )

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
