
#' Mung raw GBIF data
#'
#'
#' @param obj Result from `get_gbif`.
#' @param save_file Character or NULL. Path to save output data. If NULL, no
#' file saved.
#' @param data_map Dataframe or NULL. Mapping of GBIF fields to retrieve. If
#' NULL, all columns returned.
#' @param occ_char Logical. If true, occ_derivation will be coerced to character
#' (to match other data sources).
#'
#' @retrun Dataframe. If `save_file` is not `NULL` dataframe is saved there.
#' @export
  make_gbif <- function(obj
                        , save_file = NULL
                        , data_map = NULL
                        , occ_char = TRUE
                        ) {

    name <- "gbif"

    # What names to grab before returning results?
    if(is.null(data_map)) {

      data_map <- data.frame(t(c(name, names(temp)))) %>%
        stats::setNames(c("data_name", names(temp)))

    }

    select_names <- data_map %>%
      dplyr::filter(data_name == name) %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit() %>%
      unique()

    temp <- obj %>%
      dplyr::select(tidyselect::any_of(select_names)) %>%
      {if(occ_char) (.) %>% dplyr::mutate(organismQuantity = as.character(organismQuantity)) else (.)}

    if(!is.null(save_file)) {

      rio::export(temp
                  , save_file
                  )

    }

    return(temp)

  }
