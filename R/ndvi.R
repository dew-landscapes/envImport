#' Calculate normalised difference vegetation index (NDVI).
#'
#' @param nir Numeric. Near infra-red value.
#' @param red Numeric. Red value.
#'
#' @return Numeric. NDVI value.
#' @export
#'
#' @examples
ndvi <- function(nir, red) {

  (nir - red) / (nir + red)

}
