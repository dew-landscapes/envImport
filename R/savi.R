#' Calculate soil adjusted vegetation index (SAVI).
#'
#' SAVI is used to correct Normalized Difference Vegetation Index (NDVI) for the
#' influence of soil brightness in areas where vegetative cover is low
#' [USGS.GOV](https://www.usgs.gov/core-science-systems/nli/landsat/landsat-soil-adjusted-vegetation-index).
#'
#' @param nir Numeric. Near infra-red value.
#' @param red Numeric. Red value.
#' @param L Numeric. Soil brightness correction factor. 0.5 for most land cover
#' types (see [USGS.GOV](https://www.usgs.gov/core-science-systems/nli/landsat/landsat-soil-adjusted-vegetation-index)).
#'
#' @return Numeric. SAVI value.
#' @export
#'
#' @examples
savi <- function(nir, red, L = 0.5) {

  ((nir - red) / (nir + red + L)) * (1 + L)

}
