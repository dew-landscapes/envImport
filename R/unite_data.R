

#' Unite (assemble) disparate data sources
#'
#' Looks for any object with the names given by `data_map$data_name` and attempts
#' to unite them into one data frame (tibble).
#'
#' @param df_to_unite Dataframe with (at least) columns `data_name` and a list column of
#' results from `make_data_name` named `obj`
#' @param data_map Dataframe or NULL. Mapping of fields to retrieve. See example
#' `envImport::data_map`
#' @param out_file Path to save results to. Will always save .parquet
#' irrespective of any file type implied by out_file.
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
unite_data <- function(df_to_unite
                       , data_map
                       , out_file
                       , add_month = TRUE
                       , add_year = TRUE
                       , make_occ = TRUE
                       , occ_cols = c("occ_derivation", "quantity")
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

    out_file <- gsub("\\..*", "", out_file)
    out_file <- paste0(out_file, ".parquet")

    previous <- previous[1]

    fs::dir_create(dirname(out_file))

    # united -------
    united <- df_to_unite %>%
      dplyr::mutate(obj = purrr::map2(data_name
                                       , obj
                                       , \(x, y) remap_data_names(this_name = x
                                                                  , df = y
                                                                  , names_map = data_map
                                                                  )
                                       )
                    ) %>%
      tidyr::unnest(cols = c(obj))

    # year and month ------
    if(any(add_year, add_month)) {

      dates <- united %>%
        dplyr::distinct(date)

      if(add_year) {

        years <- dates %>%
          dplyr::mutate(year = lubridate::year(date))

      }

      if(add_month) {

        months <- dates %>%
          dplyr::mutate(month = lubridate::month(date))

      }

      united <- united %>%
        {if(add_year) (.) %>% dplyr::left_join(years) else (.)} %>%
        {if(add_month) (.) %>% dplyr::left_join(months) else (.)}

    }

    # quad_metres-------
    if(exists("quad_x", united)) {

      united <- united %>%
        dplyr::mutate(quad_metres = quad_x * quad_y) %>%
        dplyr::select(-quad_x, -quad_y)

    }

    # occ -------
    if(make_occ) {

      united$occ <- 1L

      for(i in 1:length(occ_cols)) {

        if(occ_cols[i] %in% names(united)) {

          this_col <- occ_cols[i]

          united <- united %>%
            dplyr::mutate(occ = dplyr::if_else(!!rlang::ensym(this_col) %in% absences
                                               , 0
                                               , occ
                                               )
                          )

        }

      }

    }

    keep_cols <- c(names(data_map)
                   , "quad_metres"
                   , if(make_occ) "occ"
                   , if(add_year) "year"
                   , if(add_month) "month"
                   )

    # clean up -----
    united <- united %>%
      dplyr::select(tidyselect::any_of(keep_cols))

    # previous------

    if(file.exists(out_file)) {

      if(previous[1] == "delete") {

        fs::file_delete(out_file)

      } else if(previous[1] == "move") {

        moved_out_file <- gsub("\\.parquet"
                               , paste0("__moved__", format(now(), "%Y%m%d_%H%M%S"), ".parquet")
                               , out_file
                               )

        fs::dir_copy(out_file
                     , moved_out_file
                     )

        fs::dir_delete(out_file)

        if(compare_previous) {

          previous <- arrow::open_dataset(moved_out_file) %>%
            dplyr::count(dplyr::across(tidyselect::any_of(compare_cols))
                         , name = "old_n"
                         ) %>%
            dplyr::collect()

          new <- united %>%
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
    arrow::write_dataset(dataset = united %>%
                           dplyr::group_by(data_name) # for parquet partitions
                         , existing_data_behavior = "overwrite"
                         , path = out_file
                         )

    return(invisible(NULL))

  }
