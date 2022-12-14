

#' Mung raw TERN data
#'
#' Summarises within a visit. Optionally adds
#' [Muir](https://museum.wa.gov.au/sites/default/files/2.%20Muir_5.pdf) codes.
#'
#' @param obj Result from `get_TERN`.
#' @param save_file Character or NULL. Path to save output data. If NULL, no
#' file saved.
#' @param data_map Dataframe or NULL. Mapping of TERN fields to retrieve. If
#' NULL, all columns returned.
#' @param summarise_visits Logical. If true, the most frequent `growth_form` and
#' mean `ht` are returned, along with the percentage of visit `point`s, for each
#' `herbarium_determination` and visit. Otherwise all site `point`s are
#' returned.
#' @param make_muir Logical. If true muir codes are appended to the returned
#' data frame.
#' @export
#'
#' @return dataframe and if save_file is not null, `TERN.rds`.
  make_TERN <- function(obj
                        , save_file = NULL
                        , data_map = NULL
                        , make_muir = FALSE
                        , summarise_visits = FALSE
                        ) {

    name <- "TERN"

    # initial data mung

    if(summarise_visits) {

      vis <- tibble::as_tibble(obj$veg.PI) %>%
        dplyr::filter(!is.na(herbarium_determination)) %>%
        dplyr::add_count(site_unique, name = "vis_points") %>%
        dplyr::add_count(site_location_visit_id
                         , herbarium_determination
                         , name = "points"
                         ) %>%
        dplyr::group_by(dplyr::across(tidyselect::matches("site"))
                        , dplyr::across(tidyselect::matches("visit"))
                        , herbarium_determination
                        ) %>%
        dplyr::summarise(growth_form = names(which.max(table(growth_form)))
                         , ht = mean(height, na.rm = TRUE)
                         , COVER = 100 * points / vis_points
                         ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()

    } else obj$veg.PI

    qry <- obj$site.info %>%
      tibble::as_tibble() %>%
      dplyr::inner_join(vis)


    # pull results together

    temp <- qry %>%
      dplyr::mutate(visit_start_date = as.POSIXct(visit_start_date
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

      temp <- temp %>%
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

    # What names to grab before returning results?
    if(is.null(data_map)) {

      data_map <- data.frame(t(c(name, names(temp)))) %>%
        stats::setNames(c("data_name", names(temp)))

    }

    select_names <- data_map %>%
      dplyr::filter(data_name == name) %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit()

    temp <- temp %>%
      dplyr::select(tidyselect::any_of(select_names))

    if(!is.null(save_file)) {

      rio::export(temp
                  , save_file
                  )

    }

    return(temp)

  }
