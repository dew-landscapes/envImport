#' Use a data map to combine several data sources into one data frame
#'
#' @param this_name Character. Name of the data source.
#' @param df Dataframe containing the columns to select and (potentially) rename
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
                               , df
                               , names_map
                               , exclude_names = c("data_name"
                                                   , "order"
                                                   , "days"
                                                   , "desc"
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

    old_names <- these_names %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit() %>%
      unname()

    new_names <- these_names %>%
      janitor::remove_empty("cols") %>%
      names()

    res <- df %>%
      dplyr::select(all_of(old_names)) %>%
      tibble::as_tibble() %>%
      stats::setNames(new_names)

    has_date <- "date" %in% names(res)

    if(has_date) {

      date_done <- lubridate::is.Date(res$date)

      if(!date_done) {

        res$date <- lubridate::as_date(res$date)

      }

      res <- res %>%
        dplyr::filter(!is.na(date))

    }

    if("site" %in% names(res)) {

      res$site = as.character(res$site)

    }

    res <- res  %>%
      dplyr::filter(!is.na(lat)
                    , !is.na(long)
                    )

  }
