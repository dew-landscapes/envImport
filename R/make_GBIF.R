
#' Mung raw GBIF data
#'
#'
#' @param obj Result from `get_gbif`.
#' @param save_file Character or NULL. Path to save output data. If NULL, no
#' file saved.
#' @param data_map Dataframe or NULL. Mapping of GBIF fields to retrieve. If
#' NULL, all columns returned.
#' @filter_inconsistent Logical. If `TRUE` inconsistencies between the
#' `occurrenceStatus` column and either `organismQuantity` or `individualCount`
#' are removed. e.g. a record with `occurrenceStatus == "ABSENT"` but
#' `individualCount == 1` would be filtered.
#' @param filter_NA_date Logical. Filter if `is.na(eventDate)`.
#' @param occ_char Logical. If true, occ_derivation will be coerced to character
#' (to match other data sources).
#' @param adj_spa_rel Logical. If true, an attempt will be made to check
#' `coordinateUncertaintyInMetres` against information in `informationWithheld.` If
#' `informationWithheld` contains "Coordinate uncertainty increased to",
#' `readr::parse_number()` is used to retrieve that number, which is then used
#' to replace any value in `coordinateUncertaintyInMetres.`
#'
#'
#' @retrun Dataframe. If `save_file` is not `NULL` dataframe is saved there.
#' @export
  make_gbif <- function(obj
                        , name = "gbif"
                        , data_map = NULL
                        , save_file = NULL
                        , filter_inconsistent = TRUE
                        , filter_NA_date = TRUE
                        , occ_char = TRUE
                        , adj_spa_rel = TRUE
                        ) {

    # What names to grab before returning results?
    if(is.null(data_map)) {

      data_map <- data.frame(t(c(name, names(obj)))) %>%
        stats::setNames(c("data_name", names(obj)))

    }

    select_names <- data_map %>%
      dplyr::filter(data_name == name) %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit() %>%
      unique() %>%
      c(., "individualCount")

    temp <- obj %>%
      dplyr::select(tidyselect::any_of(select_names)) %>%
      {if(filter_NA_date) (.) %>%
          dplyr::filter(!is.na(eventDate)) else (.)
        } %>%
      {if(filter_inconsistent) (.) %>%
          dplyr::filter(!(occurrenceStatus == "ABSENT" &
                            !is.na(organismQuantity) &
                            organismQuantity > 0
                          )
                        ) %>%
          dplyr::filter(!(occurrenceStatus == "PRESENT" &
                          !is.na(organismQuantity) &
                          organismQuantity == 0
                          )
                        ) %>%
          dplyr::filter(!(occurrenceStatus == "PRESENT" &
                            !is.na(individualCount) &
                            individualCount == 0
                          )
                        ) %>%
          dplyr::filter(!(occurrenceStatus == "ABSENT" &
                            !is.na(individualCount) &
                            individualCount > 0
                          )
                        ) else (.)
        } %>%
      {if(occ_char) (.) %>%
          dplyr::mutate(organismQuantity = as.character(organismQuantity)) else (.)
        } %>%
      {if(adj_spa_rel) (.) %>%
          dplyr::mutate(coordinateUncertaintyInMeters = dplyr::case_when(grepl("Coordinate uncertainty increased to"
                                                                               , informationWithheld
                                                                               ) ~ readr::parse_number(informationWithheld)
                                                                         , TRUE ~ coordinateUncertaintyInMeters
                                                                         )
          ) else (.)
      }

    if(!is.null(save_file)) {

      rio::export(temp
                  , save_file
                  )

    }

    return(temp)

  }
