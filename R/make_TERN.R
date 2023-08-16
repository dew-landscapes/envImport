
#' Mung raw TERN data
#'
#' Now uses `ausplotsR::species_table()` to mung the raw TERN data.
#'
#' @param obj Result from `get_tern`.
#' @param save_file Character or NULL. Path to save output data. If NULL, no
#' file saved.
#' @param data_map Dataframe or NULL. Mapping of TERN fields to retrieve. If
#' NULL, all columns returned.
#' @param m_kind,cover_type,species_name,strip_bryophytes Arguments required by
#' `ausplotsR::species_table()`
#' @param make_lifeform Logical. If true, the columns `growth_form` and
#' `height` in `obj$veg.PI` are used to estimate a lifeform for each taxa within
#' each unique site.
#'
#' @export
#'
#' @return dataframe and if save_file is not null, `tern.rds`.
  make_tern <- function(obj
                        , save_file = NULL
                        , data_map = NULL
                        , m_kind = "percent_cover"
                        , cover_type = "PFC"
                        , species_name = "SN"
                        , strip_bryophytes = FALSE
                        , make_lifeform = TRUE
                        ) {

    name <- "tern"

    # What names to grab before returning results?
    if(is.null(data_map)) {

      data_map <- data.frame(t(c(name, names(temp)))) %>%
        stats::setNames(c("data_name", names(temp)))

    }

    select_names <- data_map %>%
      dplyr::filter(data_name == name) %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit()

    species_col <- if(species_name == "SN") {

      "standardised_name"

    } else if(species_name == "HD") {

      "herbarium_determination"

    } else if (species_name == "GS") {

      "genus_species"

    }

    all_names <- c(select_names
                      , species_col
                      ) %>%
      unique()


    temp <- ausplotsR::species_table(obj$veg.PI
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
      dplyr::left_join(obj$site.info %>%
                         dplyr::select(tidyselect::any_of(all_names)
                                       , plot_dimensions
                                       )
                       ) %>%
      dplyr::mutate(cover = cover / 100
                    , visit_start_date = as.POSIXct(visit_date
                                                  , format = "%Y-%m-%d"
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

    if(make_lifeform) {

      # not used any more. left here in case

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

      lf <- obj$veg.PI %>%
        dplyr::filter(!is.na(!!rlang::ensym(species_col))
                      , !grepl("NA|Na", !!rlang::ensym(species_col))
                      ) %>%
        tibble::as_tibble() %>%
        dplyr::select(growth_form
                      , height
                      , tidyselect::any_of(all_names)
                      ) %>%
        dplyr::group_by(dplyr::across(tidyselect::any_of(all_names))) %>%
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
      dplyr::select(tidyselect::any_of(select_names)) %>%
      dplyr::distinct()

    if(!is.null(save_file)) {

      rio::export(temp
                  , save_file
                  )

    }

    return(temp)

  }
