#' Calculate normalised difference moisture index (NDMI).
#'
#' @param nir Numeric. Near infra-red value.
#' @param swir1 Numeric. Short-wave infra-red band 1 value.
#'
#' @return Numeric. NDMI value.
#' @export
#'
#' @examples
ndmi <- function(nir, swir1) {

  (nir - swir1) / (nir + swir1)

}
