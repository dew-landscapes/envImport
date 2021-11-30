#' Calculate normalised burn ratio 2 (NBR2).
#'
#' NBR2 modifies the Normalized Burn Ratio (NBR) to highlight water sensitivity
#' in vegetation and may be useful in post-fire recovery studies [USGS.GOV](https://www.usgs.gov/core-science-systems/nli/landsat/landsat-normalized-burn-ratio-2).
#'
#' @param swir1 Numeric. Short-wave infra-red band 1 value.
#' @param swir2 Numeric. Short-wave infra-red band 2 value.
#'
#' @return Numeric. NBR2 value.
#' @export
#'
#' @examples
nbr2 <- function(swir1, swir2) {

  (swir1 - swir2) / (swir1 + swir2)

}
