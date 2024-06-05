

#' Unite (assemble) disparate data sources
#'
#' Looks for any object with the names given by `data_map$data_name` and attempts
#' to unite them into one data frame (tibble).
#'
#' @param data_map Dataframe or NULL. Mapping of fields to retrieve. See example
#' `envImport::data_map`
#' @param out_file Path to save results to. Will always save .parquet
#' irrespective of any file type implied by out_file.
#' @param get_new Logical. If FALSE, will attempt to load from existing `out_file`
#' @param add_month,add_year Logical. Add a year and/or month column to returned
#' data frame (requires a `date` field to be specified by `data_map`)
#' @param make_occ Logical. Make an `occ` column (occurrence) of 1 = detected, 0
#' = not detected? Due to the plethora of ways original data sets record numbers
#' and absences this should not be considered 100% reliable.
#' @param absences Character. If `make_occ` what values are considered absences?
#' @param previous Character. What to do with any previous `out_file`.
#' Default is 'delete'. Alternative 'move' will rename to the same location as
#' gsub("\\.parquet", paste0("moved__", format(now(), "%Y%m%d_%H%M%S"), ".parquet"), out_file)
#' @param compare_previous Logical. If `TRUE` a comparison of records per
#' `compare_cols` will be made between the new and previous out_file. Ignored
#' unless `previous == "move`
#' @param compare_cols If `compare_previous` which columns to comapare. Default
#' is `data_name` and `survey`.
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
                       , previous = c("delete", "move")
                       , compare_previous = TRUE
                       , compare_cols = c("data_name", "survey")
                       ) {

    out_file <- gsub("\\..*", ".parquet", out_file)

    get_new = if(!file.exists(out_file)) TRUE else get_new

    if(get_new) {

      previous <- previous[1]

      fs::dir_create(dirname(out_file))

      # combine -------
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

      # year and month ------
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

      # quad_metres-------
      if(exists("quad_x", combine)) {

        combine <- combine %>%
          dplyr::mutate(quad_metres = quad_x * quad_y) %>%
          dplyr::select(-quad_x, -quad_y)

      }

      # occ -------
      if(make_occ) {

        combine <- combine %>%
          dplyr::mutate(occ = dplyr::if_else(occ_derivation %in% absences, 0, 1))


      }

      # previous------

      if(file.exists(out_file)) {

        if(previous[1] == "delete") {

          fs::file_delete(out_file)

        } else if(previous[1] == "move") {

          moved_out_file <- gsub("\\.parquet"
                                 , paste0("__moved__", format(now(), "%Y%m%d_%H%M%S"), ".parquet")
                                 , out_file
                                 )

          fs::file_move(out_file
                        , moved_out_file
                        )

          if(compare_previous) {

            previous <- arrow::open_dataset(moved_out_file) %>%
              dplyr::count(dplyr::across(tidyselect::any_of(compare_cols))
                           , name = "old_n"
                           ) %>%
              dplyr::collect()

            new <- combine %>%
              dplyr::count(dplyr::across(tidyselect::any_of(compare_cols))
                           , name = "new_n"
                           )

            stats <- previous %>%
              dplyr::full_join(new) %>%
              dplyr::mutate(diff = new_n - old_n) %>%
              dplyr::arrange(desc(diff))

            rio::export(stats
                        , fs::path(gsub("parquet", "csv", moved_out_file))
                        )

          }

        } else {

          warning("previous 'out_file' exists but no valid 'previous' argument provided. Will attempt to overwrite.")

        }

      }

      # save ------
      arrow::write_dataset(dataset = combine %>%
                             dplyr::group_by(data_name) # for parquet partitions
                           , existing_data_behavior = "overwrite"
                           , path = out_file
                           )

    } else {

      combine <- arrow::open_dataset(out_file) %>%
        dplyr::collect()

    }

    return(tibble::as_tibble(combine) %>% dplyr::ungroup())

  }
