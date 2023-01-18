

#' Make spatial reliability column
#'
#' Combines spatial reliability codes and distances into one column.
#'
#' @param df Dataframe.
#' @param rel_nr_col Column with reliabilities corresponding to
#' `envImport::lurelBDBSA` `RELIABNR`
#' @param rel_dist_col Column with reliabilities in distance
#' @param out_col Name of single output column
#'
#' @return
#' @export
#'
#' @examples
  make_rel <- function(df
                       , rel_nr_col = "rel_nr"
                       , rel_dist_col = "rel_dist"
                       , out_col = "rel_metres"
                       ) {

    df %>%
      dplyr::rename(RELIABNR = !!rlang::ensym(rel_nr_col)) %>%
      dplyr::left_join(envImport::lurelBDBSA) %>%
      dplyr::mutate(new_dist = purrr::map2(max_dist
                                           , !!rlang::ensym(rel_dist_col)
                                           , max
                                           , na.rm = TRUE
                                           )
                    ) %>%
      dplyr::select(grep(paste0(rel_nr_col
                                , "|"
                                , rel_dist_col
                                , "|"
                                , "max_dist"
                                , "|"
                                , "RELIABNR"
                                )
                         , names(.)
                         , invert = TRUE
                         )
                    ) %>%
      dplyr::rename(!!rlang::ensym(out_col) := new_dist)

  }
