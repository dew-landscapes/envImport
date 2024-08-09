#' Use a data map to select, rename, adjust and align columns
#'
#' Useful to prepare data from several different data sources into a common
#' structure that can be read collectively via `arrow::open_dataset()`
#'
#' Includes code from the [stack exchange network](https://stackoverflow.com/)
#' [post](https://stackoverflow.com/a/48186249)
#' by [Dan](https://stackoverflow.com/users/4777122/dan).
#'
#' @param this_name Character. Name of the data source.
#' @param df_to_remap Dataframe containing the columns to select and (potentially) rename
#' @param data_map Dataframe or NULL. Mapping of fields to retrieve. See example
#' `envImport::data_map`
#' @param out_file Character. Name of file to save. If `NULL`, this will be
#' `here::here("ds", this_name, "this_name.parquet")`
#' @param exclude_names Character. column names in namesmap to exclude from the
#' combined data
#' @param add_month,add_year Logical. Add a year and/or month column to returned
#' data frame (requires a `date` field to be specified by `data_map`)
#' @param make_occ Logical. Make an `occ` column (occurrence) of 1 = detected, 0
#' = not detected? Due to the plethora of ways original data sets record numbers
#' and absences this should not be considered 100% reliable.
#' @param absences Character. If `make_occ` what values are considered absences?
#' @param previous Character. What to do with any previous `out_file`.
#' Default is 'delete'. Alternative 'move' will rename to the same location as
#' gsub("\\.parquet", paste0("moved__", format(now(), "%Y%m%d_%H%M%S"), ".parquet"), `out_file`)
#' @param compare_previous Logical. If `TRUE` a comparison of records per
#' `compare_cols` will be made between the new and previous `out_file.` Ignored
#' unless `previous == "move`
#' @param compare_cols If `compare_previous` which columns to comapare. Default
#' is `survey`.
#' @param ... Not used
#'
#' @keywords internal
#' @return Tibble with selected, renamed, adjusted and aligned columns
#' @family Help with combining data sources
#' @export
#'
#' @examples
  remap_data_names <- function(this_name
                               , df_to_remap
                               , data_map = NULL
                               , out_file = NULL
                               , exclude_names = c("order"
                                                   , "epsg"
                                                   , "desc"
                                                   , "data_name_use"
                                                   )
                               , add_month = !is.null(data_map)
                               , add_year = !is.null(data_map)
                               , make_occ = !is.null(data_map)
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
                               , ...
                               ) {

    if(is.null(out_file)) out_file <- here::here("ds", this_name, paste0(this_name, ".parquet"))

    if(is.null(data_map)) {

      select_names <- tibble::tibble(name = names(df_to_remap)
                                     , value = names(df_to_remap)
                                     ) %>%
        dplyr::bind_rows(tibble::tibble(name = this_name, value = this_name))

    } else {

      select_names <- data_map %>%
        dplyr::filter(data_name == this_name) %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(), \(x) as.character(x))) %>%
        tidyr::pivot_longer(everything()) %>%
        dplyr::filter(!name %in% exclude_names) %>%
        stats::na.omit()

    }

    # call out the column names that don't exist
    not_nms <- setdiff(select_names$value[select_names$name != "data_name"], names(df_to_remap))

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

    # rename------
    rdf <- df_to_remap %>%
      dplyr::mutate(data_name := this_name) %>%
      dplyr::select(data_name, tidyselect::any_of(unlist(tidyr::pivot_wider(select_names))))

    # dates------
    if(any(grepl("date", names(rdf), ignore.case = TRUE))) {

      rdf <- rdf %>%
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

      rdf <- rdf %>%
        dplyr::filter(dplyr::if_any(tidyselect::matches("date")
                                    , ~!is.na(.x)
                                    )
                      ) %>%
        dplyr::filter(dplyr::if_any(tidyselect::matches("date")
                                    , ~ .x > "1600-01-01"
                                    )
                      )

    }

    if(any(grepl("site", names(rdf), ignore.case = TRUE))) {

      rdf <- rdf %>%
        dplyr::mutate(dplyr::across(tidyselect::matches("site")
                                    , as.character
                                    )
                      )

    }

    # ind -------
    if(any(grepl("ind", names(rdf), ignore.case = TRUE))) {

      rdf <- rdf %>%
        dplyr::mutate(dplyr::across(tidyselect::matches("ind")
                                    , ~dplyr::case_when(grepl("\\*|^N$|^n$|introduced|Introduced", .x) ~ "N"
                                                        , grepl("^Y$|^y$|native|Native", .x) ~ "Y"
                                                        , TRUE ~ "U"
                                                        )
                                    )
                      )

    }

    # lat/long --------
    if(any(grepl("lat|long", names(rdf), ignore.case = TRUE))) {

      rdf <- rdf %>%
        dplyr::mutate(dplyr::across(tidyselect::matches("lat")
                                    , as.numeric
                                    )
                      ) %>%
        dplyr::mutate(dplyr::across(tidyselect::matches("long")
                                    , as.numeric
                                    )
                      )

      rdf <- rdf  %>%
        dplyr::filter(dplyr::if_any(tidyselect::matches("lat")
                                    , ~!is.na(.x)
                                    )
                      ) %>%
        dplyr::filter(dplyr::if_any(tidyselect::matches("long")
                                    , ~!is.na(.x)
                                    )
                      )

    }


    # year and month ------
    if(any(add_year, add_month)) {

      dates <- rdf %>%
        dplyr::distinct(date)

      if(add_year) {

        years <- dates %>%
          dplyr::mutate(year = lubridate::year(date))

      }

      if(add_month) {

        months <- dates %>%
          dplyr::mutate(month = lubridate::month(date))

      }

      rdf <- rdf %>%
        {if(add_year) (.) %>% dplyr::left_join(years) else (.)} %>%
        {if(add_month) (.) %>% dplyr::left_join(months) else (.)}

    }

    # quad_metres-------
    if(exists("quad_x", rdf)) {

      rdf <- rdf %>%
        dplyr::mutate(quad_metres_calc = quad_x * quad_y) %>%
        {if("quad_metres" %in% names(.)) (.) else (.) %>% dplyr::mutate(quad_metres = NA)} %>%
        dplyr::mutate(quad_metres = dplyr::case_when(!is.na(quad_metres) ~ quad_metres
                                                     , is.na(quad_metres) ~ quad_metres_calc
                                                     )
                      ) %>%
        dplyr::select(-quad_x, -quad_y, -quad_metres_calc)

    }

    # occ -------
    if(make_occ) {

      rdf$occ <- 1L

      for(i in 1:length(occ_cols)) {

        if(occ_cols[i] %in% names(rdf)) {

          this_col <- occ_cols[i]

          rdf <- rdf %>%
            dplyr::mutate(occ = dplyr::if_else(!!rlang::ensym(this_col) %in% absences
                                               , 0
                                               , occ
                                               )
                          )

        }

      }

    }

    # previous------

    if(file.exists(out_file)) {

      if(previous[1] == "delete") {

        fs::file_delete(out_file)

      } else if(previous[1] == "move") {

        moved_out_file <- fs::path(dirname(out_file)
                                   , "moved"
                                   , gsub("\\.parquet"
                                          , base::paste0("__moved__", base::format(base::Sys.time(), "%Y%m%d_%H%M%S"), ".parquet")
                                          , basename(out_file)
                                          )
                                   )

        fs::dir_create(dirname(moved_out_file))

        fs::file_copy(out_file
                     , moved_out_file
                     )

        fs::file_delete(out_file)

        if(compare_previous) {

          previous <- rio::import(moved_out_file
                                  , setclass = "tibble"
                                  ) %>%
            dplyr::count(dplyr::across(tidyselect::any_of(compare_cols))
                         , name = "old_n"
                         ) %>%
            dplyr::collect()

          new <- rdf %>%
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

    # clean up -----
    ## select cols -------
    bio_all_cols <- data_map_class %>%
      dplyr::filter(bio_all)

    keep_cols <- bio_all_cols$col

    rdf <- rdf %>%
      dplyr::select(tidyselect::any_of(keep_cols))

    # save ------
    rio::export(rdf
                , out_file
                )

    return(rdf)

  }
