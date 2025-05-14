
#' Choose columns to keep from a dataframe, based on a data map
#'
#' The behaviour when `df` provided and `is.null(data_map)` is essentially
#' `df <- df` (although any colnames matching `excludes` will be removed)
#'
#' @param df Dataframe to select columns from. Only needed if `is.null(data_map)`
#' @param data_map Dataframe or `NULL.` Mapping of fields to retrieve. See example
#' `envImport::data_map` or `envImport::data_map_old`. If `NULL` all columns are
#' returned. Optional if `df` provided.
#' @param this_name Character. `data_name` value in `data_map`. Required if
#' `data_map` is not `NULL`
#' @param final_select Logical. Is this the final select prior to writing to
#' disk?
#' @param final_select_col Character. If `final_select`, which column contains
#' the logical values to filter on?
#'
#' @return
#' @export
#' @keywords internal
#'
#' @examples
  choose_names <- function(df = NULL
                           , data_map = NULL
                           , this_name
                           , excludes = NULL
                           , final_select = FALSE
                           , final_select_col = "bio_all"
                           ) {

    # Deal with null data_map
    if(is.null(data_map)) {

      data_map <- tibble::tibble(col = "data_name"
                                 , !!rlang::ensym(this_name) := this_name
                                 ) |>
        dplyr::bind_rows(tibble::tibble(col = names(df)
                                        , !!rlang::ensym(this_name) := names(df)
                                        )
                         )
    }

    if(! final_select_col %in% names(data_map)) {

      data_map[final_select_col] <- TRUE

    }

    # Select cols from old or new data map
    if(!all(c("col", this_name) %in% names(data_map))) {

      select_names <- data_map %>%
        dplyr::filter(data_name == this_name) %>%
        dplyr::mutate(dplyr::across(tidyselect::everything(), \(x) as.character(x))) %>%
        dplyr::select(tidyselect::any_of(data_map$col)) %>%
        tidyr::pivot_longer(tidyselect::any_of(data_map$col)
                            , names_to = "col"
                            ) %>%
        na.omit()


    } else {

      select_names <- data_map %>%
        {if(final_select) (.) %>% dplyr::filter(!!rlang::ensym(final_select_col)) else (.)} %>%
        dplyr::select(col, value = tidyselect::any_of(this_name)) %>%
        na.omit()

    }

    if(!is.null(excludes)) {

      select_names <- select_names %>%
        dplyr::filter(!col %in% excludes)

    }

    return(select_names)

  }
