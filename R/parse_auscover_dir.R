

#' Parse AusCover file names found in a directory.
#'
#' See for example AusCover [persistent green](https://object-store.rc.nectar.org.au/v1/AUTH_05bca33fce34447ba7033b9305947f11/data_submission_tool_attachments/e60f5125-ed2f-47cb-99a7-c9a201e44d2f/seasonal_persistent_green_landsat_filenaming_conven_h5HG2vG.txt).
#' filenaming convention.
#'
#' @param dir Character. Directory to search for rasters to parse.
#' @param ... Passed to [fs::dir_info()].
#'
#' @return Dataframe with columns
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
#'   \item{tif}{Filename.}
#'   \item{path}{Full (relative) path including `tif`.}
#' }
#' @export
#'
#' @examples
parse_auscover_dir <- function(dir
                          , ...
                          ) {

  luseasons <- tibble::tribble(
    ~season, ~months,
    "summer", "1202",
    "autumn", "0305",
    "winter", "0608",
    "spring", "0911",
    ) %>%
    dplyr::mutate(season = forcats::fct_inorder(season))

  fs::dir_info(path
               , ...
               ) %>%
    dplyr::filter(type == "file") %>%
    envImport::parse_auscover()

}
