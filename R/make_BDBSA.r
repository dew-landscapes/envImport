
#' Mung raw BDBSA data
#'
#'
#' @param obj Result from `get_TERN`.
#' @param save_file Character or NULL. Path to save output data. If NULL, no
#' file saved.
#' @param data_map Dataframe or NULL. Mapping of TERN fields to retrieve. If
#' NULL, all columns returned.
#' @param flora Logical. Return flora or fauna records.
#'
#' @return dataframe and if save_file is not null, `save_file` is written.
#' @export
#'
#' @examples
  make_BDBSA <- function(obj
                         , save_file = NULL
                         , data_map = NULL
                         ) {

    name <- "BDBSA"

    # What names to grab before returning results?
    if(is.null(data_map)) {

      data_map <- data.frame(t(c(name, names(temp)))) %>%
        stats::setNames(c("data_name", names(temp)))

    }

    select_names <- data_map %>%
      dplyr::filter(data_name == name) %>%
      unlist(.
             , use.names = FALSE
             ) %>%
      stats::na.omit()

    temp <- obj %>%
      dplyr::select(tidyselect::any_of(select_names))

    if(!is.null(save_file)) {

      rio::export(temp
                  , save_file
                  )

    }

    return(temp)

  }
