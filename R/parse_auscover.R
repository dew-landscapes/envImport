

#' Parse an AusCover file name.
#'
#' See for example AusCover [persistent green](https://object-store.rc.nectar.org.au/v1/AUTH_05bca33fce34447ba7033b9305947f11/data_submission_tool_attachments/e60f5125-ed2f-47cb-99a7-c9a201e44d2f/seasonal_persistent_green_landsat_filenaming_conven_h5HG2vG.txt).
#' filenaming convention.
#'
#' @param path Character. Path(s) to AusCover raster file(s).
#'
#' @return Tibble with names
#' \describe{
#'   \item{satellite}{satellite category.}
#'   \item{instrument}{tm: thematic.}
#'   \item{product}{re: reflective.}
#'   \item{where}{sa: South Australia.}
#'   \item{when}{Season start and season end dates as "m"YYYYmmYYYYmm.}
#'   \item{process}{e.g. dja is persistent green cover.}
#'   \item{months}{Month component of `when` as mmmm.}
#'   \item{year}{Year component of `when` as YYYY. For `months` = `1202`, `year`
#'   = `year + 1`.}
#' }
#' @export
#'
#' @examples
parse_auscover <- function(path) {

  luseasons <- tibble::tribble(
    ~season, ~months,
    "summer", "1202",
    "autumn", "0305",
    "winter", "0608",
    "spring", "0911",
    ) %>%
    dplyr::mutate(season = forcats::fct_inorder(season))

  tibble::tibble(path = path) %>%
    dplyr::mutate(NULL
                  , tif = fs::path_file(path)
                  , satellite = substr(tif, 1, 2)
                  , instrument = substr(tif, 3, 4)
                  , product = substr(tif, 5, 6)
                  , where = stringr::str_match(tif, "_([[:alpha:]]{2,3})_")[,2]
                  , when = stringr::str_match(tif, "_[[:alpha:]]{1}([[:digit:]]+)_")[,2]
                  , process = stringr::str_match(tif, "_([[:alnum:]]+)\\.")[,2]
                  , process = gsub("a2", "", process)
                  , months = paste0(substr(when,5,6),substr(when,11,12))
                  , year = as.integer(substr(when, 1, 4))
                  , year = dplyr::if_else(months == "1202"
                                          , year + 1L
                                          , year
                                          ) # sets summer to next year
                  ) %>%
    dplyr::left_join(luseasons) %>%
    dplyr::select(-path, -tif)
}
