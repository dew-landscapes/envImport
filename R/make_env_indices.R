#' Make NDVI, SAVI, NDMI and NBR2
#'
#' @param process Which functions to calculate? e.g c("ndvi", "savi")
#' @param sum_func Named list. If there are more than one layer over which
#' `process` are calculated, how to summarise them?
#' @param sum_func_na_rm `na.rm` argument passed to `sum_func`
#'
#' @param ... nir, red, swir1, swir2 values
#'
#' @return tibble with columns
#' \itemize{
#'   \item{func}{Name of the functions applied}
#'   \item{summarise_func}{A column for each summary function, each named for
#'   `summarise_func`.}
#' }
#' @export
#'
#' @examples
make_env_indices <- function(process = c("ndvi"
                                         , "savi"
                                         , "msavi"
                                         , "ndmi"
                                         , "nbr2"
                                       )
                             , sum_func = "mean"
                             , sum_func_na_rm = TRUE
                             , ...
                             ) {

  tibble::tibble(process = process) %>%
    dplyr::mutate(value = purrr::map(process
                                     , function(x, ...) R.utils::doCall(x
                                                                        , args = ...
                                                                        )
                                     , ...
                                     )
                  , value = purrr::map_dbl(value
                                           , ~R.utils::doCall(sum_func
                                                              , .
                                                              , args = list(na.rm = sum_func_na_rm)
                                                              )
                                           )
                  ) %>%
    dplyr::pull(value)

}
