#' Use a data map to combine several data sources into one data frame
#'
#' Includes code from the [stack exchange network](https://stackoverflow.com/)
#' [post](https://stackoverflow.com/a/48186249)
#' by [Dan](https://stackoverflow.com/users/4777122/dan).
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

    # SO code...
    # https://stackoverflow.com/a/48186249
    stopifnot(length(old_names) == length(new_names))

    # pull out the names that are actually in df
    old_nms <- old_names[old_names %in% names(df)]
    new_nms <- new_names[old_names %in% names(df)]

    # call out the column names that don't exist
    not_nms <- setdiff(old_names, old_nms)

    if(length(not_nms) > 0) {

      msg <- paste(paste(not_nms
                         , collapse = ", "
                         )
                   , "are not columns in the dataframe, so won't be renamed."
                   )

      warning(msg)

    }

    # rename
    names(df)[names(df) %in% old_nms] <- new_nms
    # end SO code

    # dates
    has_date <- "date" %in% names(df)

    if(has_date) {

      date_done <- lubridate::is.Date(df$date)

      if(!date_done) {

        df$date <- lubridate::as_date(df$date)

      }

      df <- df %>%
        dplyr::filter(!is.na(date))

    }

    if("site" %in% names(df)) {

      df$site = as.character(df$site)

    }

    df <- df  %>%
      dplyr::filter(!is.na(lat)
                    , !is.na(long)
                    )

  }
