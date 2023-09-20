

#' Unite (assemble) disparate data sources
#'
#' Looks for any object with the names given by `data_map$data_name` and attempts
#' to unite them into one data frame (tibble).
#'
#' @param data_map Dataframe or NULL. Mapping of fields to retrieve. See example
#' `envImport::data_map`
#' @param out_file Path to save results to
#' @param get_new Logical. If FALSE, will attempt to load from existing `out_file`
#' @param add_month,add_year Logical. Add a year and/or month column to returned
#' data frame (requires a `date` field to be specified by `data_map`)
#' @param make_occ Logical. Make an `occ` column (occurrence) of 1 = detected, 0
#' = not detected? Due to the plethora of ways original data sets record numbers
#' and absences this should not be considered 100% reliable.
#' @param absences Character. If `make_occ` what values are considered absences?
#'
#' @return Dataframe of united objects with columns named as per the columns of
#' `data_map`, optionally with `month` and `year` columns too. `out_file` is
#' also created.
#' @export
#'
#' @examples
  unite_data <- function(data_map
                       , out_file
                       , get_new = FALSE
                       , add_month = TRUE
                       , add_year = TRUE
                       , make_occ = TRUE
                       , absences = c("0"
                                      , "none detected"
                                      , "none observed"
                                      , "None detected"
                                      , "ABSENT"
                                      )
                       ) {

    get_new = if(!file.exists(out_file)) TRUE else get_new

    if(get_new) {

      combine <- mget(as.character(data_map$data_name)
                      , ifnotfound = rep(NA
                                         , nrow(data_map)
                                         )
                      , inherits = TRUE
                      ) %>%
        tibble::enframe(name = "data_name") %>%
        dplyr::filter(purrr::map_lgl(value
                                     , ~ ! is.logical(.)
                                     )
                      ) %>%
        dplyr::mutate(value = purrr::map2(data_name
                                         , value
                                         , remap_data_names
                                         , names_map = data_map
                                         )
                      ) %>%
        tidyr::unnest(cols = c(value))

      if(any(add_year, add_month)) {

        dates <- combine %>%
          dplyr::distinct(date)

        if(add_year) {

          years <- dates %>%
            dplyr::mutate(year = lubridate::year(date))

        }

        if(add_month) {

          months <- dates %>%
            dplyr::mutate(month = lubridate::month(date))

        }

        combine <- combine %>%
          {if(add_year) (.) %>% dplyr::left_join(years) else (.)} %>%
          {if(add_month) (.) %>% dplyr::left_join(months) else (.)}

      }

      if(exists("quad_x", combine)) {

        combine <- combine %>%
          dplyr::mutate(quad_metres = quad_x * quad_y) %>%
          dplyr::select(-quad_x, -quad_y)

      }

      if(make_occ) {

        combine <- combine %>%
          dplyr::mutate(occ = dplyr::if_else(occ_derivation %in% absences, 0, 1))


      }

      rio::export(combine
                  , out_file
                  )

    } else {

      combine <- rio::import(out_file)

    }

    return(tibble::as_tibble(combine))

  }
