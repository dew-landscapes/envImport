#' Calculate modified soil adjusted vegetation index (MSAVI).
#'
#' Soil adjusted vegetation index (SAVI) is used to correct Normalized
#' Difference Vegetation Index (NDVI) for the influence of soil brightness in
#' areas where vegetative cover is low. MSAVI minimizes the effect of bare soil
#' on the Soil Adjusted Vegetation Index (SAVI). See
#' [USGS.GOV](https://www.usgs.gov/core-science-systems/nli/landsat/landsat-soil-adjusted-vegetation-index).
#'
#' @param nir Numeric. Near infra-red value.
#' @param red Numeric. Red value.
#' types (see [USGS.GOV](https://www.usgs.gov/core-science-systems/nli/landsat/landsat-soil-adjusted-vegetation-index)).
#'
#' @return Numeric. MSAVI value.
#' @export
#'
#' @examples
msavi <- function(nir, red) {

  (2 * nir + 1 - sqrt((2 * nir + 1)^2 - 8 * (nir -red))) / 2

}
