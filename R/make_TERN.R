

#' Mung raw TERN data
#'
#' Now uses `ausplotsR::species_table()` to mung the raw TERN data.
#'
#' @param obj Result from `get_TERN`.
#' @param save_file Character or NULL. Path to save output data. If NULL, no
#' file saved.
#' @param data_map Dataframe or NULL. Mapping of TERN fields to retrieve. If
#' NULL, all columns returned.
#'
#' @export
#'
#' @return dataframe and if save_file is not null, `tern.rds`.
  make_TERN <- function(obj
                        , save_file = NULL
                        , data_map = NULL
                        , make_muir = FALSE
                        , summarise_visits = FALSE
                        ) {

    name <- "tern"

    # initial data mung

    qry <- ausplotsR::species_table(obj$veg.PI
                                    , ...
                                    )




    # pull results together

    temp <- qry %>%
      dplyr::mutate(visit_start_date = as.POSIXct(visit_start_date
                                                  , format = "%Y-%m-%dT%H:%M:%S"
                                                  )
                    , quadX = as.numeric(gsub("\\s"
                                              , ""
                                              , stringr::str_extract(plot_dimensions
                                                                     , "\\d{1,4} "
                                                                     )
                                              )
                                         )
                    , quadY = as.numeric(gsub("\\s"
                                              , ""
                                              , stringr::str_extract(plot_dimensions
                                                                     , " \\d{1,4}"
                                                                     )
                                              )
                                         )
                    )

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
