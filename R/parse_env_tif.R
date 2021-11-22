

#' Parse tif saved using raster file naming convention among `env` packages.
#'
#'
#' @param path Character. Path to search for rasters to parse.
#' @param ... Passed to [fs::dir_info()].
#'
#' @return Dataframe with columns
#' \describe{
#'   \item{process}{e.g. dja is persistent green cover.}
#'   \item{layer}{band or layer that was summarised.}
#'   \item{method}{Method that was used to summarise.}
#'   \item{season}{Season into which summary was made.}
#'   \item{epoch}{Date range (years) into which summary was made.}
#'   \item{type}{File type of `path`.}
#'   \item{path}{Full (relative) path including `tif`.}
#' }
#' @export
#'
#' @examples
parse_env_tif <- function(path
                           , ...
                           ) {

  fs::dir_info(path
               , ...
               ) %>%
    dplyr::filter(type == "file") %>%
    dplyr::mutate(tif = fs::path_file(path)) %>%
    dplyr::select(tif, path) %>%
    tidyr::separate(tif
                    , into = c("name", "type")
                    , sep = "\\."
                    ) %>%
    tidyr::separate(name
                    , into = c("process", "layer", "method", "season", "epoch")
                    , sep = "_"
                    )

}
