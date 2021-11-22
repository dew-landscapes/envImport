

#' Generate lists of paths in preparation for [summarise_rast_paths()].
#'
#' Using the file naming conventions in the [AusCover data](http://data.auscover.org.au/xwiki/bin/view/Field+Sites/AusCover+Filenaming+Convention)
#' , collate paths of previously downloaded raster files in preparation for
#' running [envImport::summarise_rast_paths()].
#'
#' @param dir_local Character. Path to search for rasters to summarise.
#' @param epoch_step Numeric. How many years in an epoch?
#' @param epoch_overlap Logical. Should epochs overlap by one year? i.e.
#' `epoch_overlap = TRUE` gives, say, 2000-2010 and 2010-2020 whereas
#' `epoch_overalp = FALSE` gives, say, 2000-2009 and 2010-2019.
#' @param epoch_which Numeric. Which epochs to summarise? Zero for current
#' epoch, -1 for immediate previous epoch etc. -2:-1 for the last two full
#' epochs.
#' @param filt_n Numeric. Filter prepared stacks with less than `filt_n`
#' rasters.
#'
#' @return Dataframe with columns
#' \describe{
#'   \item{process}{Raster product in `data`. See [envEcosystems::env].}
#'   \item{epoch}{Last two digits of initial year and final year for rasters
#'   in `data`. e.g. 10-19.}
#'   \item{season}{Season for rasters in `data`.}
#'   \item{data}{List column of full path of raster files to summarise.}
#' }
#' @export
#'
#' @examples
prep_auscover <- function(dir_local = "../../data/raster/dynamic/AusCover/landsat/"
                          , epoch_step = 10
                          , epoch_overlap = FALSE
                          , epoch_which = -1
                          , filt_n = 9
                          ) {

  luseasons <- tibble::tribble(
    ~season, ~months,
    "summer", "1202",
    "autumn", "0305",
    "winter", "0608",
    "spring", "0911",
    ) %>%
    dplyr::mutate(season = forcats::fct_inorder(season))

  starts <- 1990 + epoch_step*0:10
  ends <- starts + (epoch_step - 1 + epoch_overlap)

  now <- as.numeric(format(Sys.Date(), "%Y"))

  eps <- tibble::tibble(start = starts
                        , end = ends
                        ) %>%
    dplyr::mutate(year = purrr::map2(start, end, ~.x:.y)
                  , epoch = paste0(substr(start,3,4), "-", substr(end,3,4))
                  , epoch = forcats::fct_inorder(epoch, ordered = TRUE)
                  , epoch_min = purrr::map_int(year, min)
                  , epoch_max = purrr::map_int(year, max)
                  , epoch_now = purrr::map_lgl(year, ~ ! now %in% .)
                  , epoch_index = dplyr::row_number() - max(dplyr::row_number() * !epoch_now)
                  ) %>%
    dplyr::filter(epoch_index < 1) %>%
    dplyr::filter(epoch_index %in% epoch_which)

  luepoch <- eps %>%
    tidyr::unnest(cols = c(year))

  fs::dir_info(dir_local
               , recurse = TRUE
               ) %>%
    dplyr::filter(type == "file") %>%
    dplyr::mutate(dir = fs::path_dir(path)
                  , tif = fs::path_file(path)
                  ) %>%
    dplyr::select(dir, tif, type, path) %>%
    dplyr::add_count(dir) %>%
    dplyr::mutate(NULL
                  , file_dates = stringr::str_match(tif, "_[[:alpha:]]{1}([[:digit:]]+)_")[,2]
                  , process = stringr::str_match(tif, "_([[:alnum:]]+)\\.")[,2]
                  , process = gsub("a2", "", process)
                  , months = paste0(substr(file_dates,5,6),substr(file_dates,11,12))
                  , year = as.integer(substr(file_dates, 1, 4))
                  , year = dplyr::if_else(months == "1202"
                                          , year + 1L
                                          , year
                                          ) # sets summer to next year
                  ) %>%
    dplyr::select(process, year, months, path, -file_dates) %>%
    dplyr::inner_join(luepoch %>%
                       dplyr::select(year, epoch)
                     ) %>%
    dplyr::left_join(luseasons) %>%
    tidyr::nest(data = -matches("process|epoch|season")) %>%
    dplyr::mutate(n_layers = purrr::map_dbl(data, nrow)) %>%
    dplyr::filter(n_layers >= filt_n) %>%
    dplyr::mutate(NULL
                  , data = purrr::map(data, "path")
                  )

}
