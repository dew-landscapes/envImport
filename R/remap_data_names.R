#' Use a data map to combine several data sources into one data frame
#'
#' Includes code from the [stack exchange network](https://stackoverflow.com/)
#' [post](https://stackoverflow.com/a/48186249)
#' by [Dan](https://stackoverflow.com/users/4777122/dan).
#'
#' @param this_name Character. Name of the data source.
#' @param df_to_remap Dataframe containing the columns to select and (potentially) rename
#' @param names_map Dataframe mapping old names to new names
#' @param exclude_names Character. column names in namesmap to exclude from the
#' combined data
#'
#' @return Tibble with select and renamed columns
#' @family Help with combining data sources
#' @export
#'
#' @examples
  remap_data_names <- function(this_name
                               , df_to_remap
                               , names_map
                               , exclude_names = c("data_name"
                                                   , "order"
                                                   , "days"
                                                   , "desc"
                                                   , "epsg"
                                                   )
                               ) {

    these_names <- names_map %>%
      dplyr::filter(data_name == this_name) %>%
      dplyr::select(grep(paste0(exclude_names
                                , collapse = "|"
                                )
                         , names(.)
                         , invert = TRUE
                         , value = TRUE
                         )
                    )

    new_old_names <- tibble::tibble(old = these_names %>%
                                  unlist(., use.names=FALSE) %>%
                                  stats::na.omit() %>%
                                  unname()
                                , new = these_names %>%
                                  janitor::remove_empty("cols") %>%
                                  names()
                                )

    # call out the column names that don't exist
    not_nms <- setdiff(new_old_names$old, names(df_to_remap))

    if(length(not_nms) > 0) {

      msg <- paste(this_name
                   , ": "
                   , paste(not_nms
                         , collapse = ", "
                         )
                   , "are not columns in the dataframe, so won't be renamed."
                   )

      warning(msg)

    }

    # rename
    rdf_to_remap <- df_to_remap %>%
      dplyr::select(tidyselect::any_of(new_old_names$old)) %>%
      stats::setNames(new_old_names$new[match(names(.), new_old_names$old)])

    # dates
    if(any(grepl("date", names(rdf_to_remap), ignore.case = TRUE))) {

      rdf_to_remap <- rdf_to_remap %>%
        dplyr::mutate(dplyr::across(tidyselect::matches("date")
                                    , ~if(is.character(.x)) {lubridate::parse_date_time(.x
                                                                                        , orders = c("dmy"
                                                                                                     , "dmy HMS"
                                                                                                     , "dmy HM"
                                                                                                     , "ymd HMS"
                                                                                                     , "ymd"
                                                                                                     , "ym"
                                                                                                     , "y"
                                                                                                     )
                                                                                        )

                                    } else {

                                      .x

                                      }
                                    )
                      , dplyr::across(tidyselect::matches("date")
                                      , lubridate::as_date
                                      )
                      )

      rdf_to_remap <- rdf_to_remap %>%
        dplyr::filter(dplyr::if_any(tidyselect::matches("date")
                                    , ~!is.na(.x)
                                    )
                      ) %>%
        dplyr::filter(dplyr::if_any(tidyselect::matches("date")
                                    , ~ .x > "1600-01-01"
                                    )
                      )

    }

    if(any(grepl("site", names(rdf_to_remap), ignore.case = TRUE))) {

      rdf_to_remap <- rdf_to_remap %>%
        dplyr::mutate(dplyr::across(tidyselect::matches("site")
                                    , as.character
                                    )
                      )

    }

    if(any(grepl("ind", names(rdf_to_remap), ignore.case = TRUE))) {

      rdf_to_remap <- rdf_to_remap %>%
        dplyr::mutate(dplyr::across(tidyselect::matches("ind")
                                    , ~dplyr::case_when(grepl("\\*|^N$|^n$|introduced|Introduced", .x) ~ "N"
                                                        , grepl("^Y$|^y$|native|Native", .x) ~ "Y"
                                                        , TRUE ~ "U"
                                                        )
                                    )
                      )

    }

    if(any(grepl("lat|long", names(rdf_to_remap), ignore.case = TRUE))) {

      rdf_to_remap <- rdf_to_remap %>%
        dplyr::mutate(dplyr::across(tidyselect::matches("lat")
                                    , as.numeric
                                    )
                      ) %>%
        dplyr::mutate(dplyr::across(tidyselect::matches("long")
                                    , as.numeric
                                    )
                      )

      rdf_to_remap <- rdf_to_remap  %>%
        dplyr::filter(dplyr::if_any(tidyselect::matches("lat")
                                    , ~!is.na(.x)
                                    )
                      ) %>%
        dplyr::filter(dplyr::if_any(tidyselect::matches("long")
                                    , ~!is.na(.x)
                                    )
                      )

    }

    return(rdf_to_remap)

  }
