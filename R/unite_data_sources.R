
#' Unite multiple data sources into one data frame
#'
#' Failed (not currently working anyway) experiment
#'
#' @param data_map Dataframe. Needs to contain columns `data_name` and `days`.
#' Other columns are the column names in the unified data frame. Values
#' against each `data_name` contain the name of the column in the original data
#' source that should map to the current column name.
#' @param override_days Logical over-ride of `get_new` output.
#' @param exclude Passed to `envImport::remap_data_names` argument
#' `exclude_names`.
#' @param ... Passed to `envImport::get_data`
#'
#' @return single data frame unifying the data from the input `data_name`s
#' @family functions to help with combining data sources
#' @export
#'
#' @examples
  unite_data_sources <- function(data_map
                                 , override_days = NULL
                                 , exclude = c("data_name"
                                               , "order"
                                               , "days"
                                               , "desc"
                                               )
                                 , ...
                                 ) {

    .data_map = data_map

    data_map %>%
      dplyr::select(data_name,days) %>%
      dplyr::mutate(get_new = purrr::map_dbl(data_name,envImport::days_since_update)
                    , get_new = get_new > days
                    , get_new = if(!is.null(override_days)) override_days else get_new
                    , dat = purrr::map2(data_name
                                        , get_new
                                        , function(x
                                                   , y
                                                   , ...
                                                   ) {

                                          get_data(x
                                                   , y
                                                   , data_map = .data_map
                                                   , ...
                                                   )

                                          }
                                        , ...
                                        )
                    , dat = purrr::map2(data_name
                                        , dat
                                        , envImport::remap_data_names
                                        , names_map = data_map
                                        , exclude_names = exclude
                                        )
                    ) %>%
      tidyr::unnest(cols = c(dat)) %>%
      dplyr::select(!tidyselect::matches("^n$"))

  }
