

#' Generate lists of paths in preparation for `summarise_rast_paths`.
#'
#' Using the file naming conventions in the [AusCover data](), collate paths of
#' previously downloaded raster files in preparation for running
#' [envImport::summarise_rast_paths()].
#'
#' @param dir_local Character. Path to search for rasters to summarise.
#' @param dir_out Character. Path to save summarised rasters.
#' @param epoch_step Numeric. How many years in an epoch?
#' @param epoch_overlap Logical. Should epochs overlap by one year? i.e.
#' `epoch_overlap = TRUE` gives, say, 2000-2010 and 2010-2020 whereas
#' `epoch_overalp = FALSE` gives, say, 2000-2009 and 2010-2019.
#'
#' @return Dataframe with columns
#' \itemize{
#'   \item{out_file}{Base name for summary files to be created. These will be
#'   appended with function name and `lyr1:n` by
#'   [envImport::summarise_rast_paths()]}
#'   \item{data}{List column of full path of files to summarise.}
#' }
#' @export
#'
#' @examples
prep_auscover <- function(dir_local = "../../data/raster/AusCover/landsat"
                          , dir_out = "../../data/raster/Auscover"
                          , epoch_step = 10
                          , epoch_overlap = FALSE
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

  eps <- tibble::tibble(start = starts
                        , end = ends
                        ) %>%
    dplyr::mutate(year = purrr::map2(start, end, ~.x:.y)
                  , epoch = paste0(substr(start,3,4), "-", substr(end,3,4))
                  , epoch = forcats::fct_inorder(epoch, ordered = TRUE)
                  , epoch_id = dplyr::row_number()
                  , epoch_min = purrr::map_int(year, min)
                  , epoch_max = purrr::map_int(year, max)
                  )

  luepoch <- eps %>%
    tidyr::unnest(cols = c(year))

  raster_summaries <- fs::dir_info(dir_local
                                   , recurse = TRUE
                                   ) %>%
    dplyr::filter(type == "file") %>%
    dplyr::mutate(dir = fs::path_dir(path)
                  , tif = fs::path_file(path)
                  ) %>%
    dplyr::select(dir, tif, type, path) %>%
    dplyr::add_count(dir) %>%
    dplyr::filter(n > 50) %>%
    dplyr::mutate(NULL
                  #, file_sat = substr(tif, 1, 2)
                  #, file_inst = substr(tif, 3, 4)
                  #, file_prod = substr(tif, 5, 6)
                  #, file_loc = stringr::str_match(tif, "_([[:alpha:]]{2,3})_")[,2]
                  , file_dates = stringr::str_match(tif, "_[[:alpha:]]{1}([[:digit:]]+)_")[,2]
                  , file_pro = stringr::str_match(tif, "_([[:alnum:]]+)\\.")[,2]
                  , file_pro = gsub("a2", "", file_pro)
                  , months = paste0(substr(file_dates,5,6),substr(file_dates,11,12))
                  , year = as.integer(substr(file_dates, 1, 4))
                  , year = dplyr::if_else(months == "1202"
                                          , year + 1L
                                          , year
                                          ) # sets summer to next year
                  ) %>%
    dplyr::filter(year >= 1990) %>%
    dplyr::select(contains("file"), year, months, path, -file_dates) %>%
    dplyr::left_join(luepoch %>%
                       dplyr::select(year, epoch)
                     ) %>%
    dplyr::left_join(luseasons) %>%
    tidyr::nest(data = -matches("file|epoch|season")) %>%
    tidyr::unite("out_file", matches("file|epoch|season")) %>%
    dplyr::mutate(NULL
                  , data = purrr::map(data, "path")
                  , out_file = fs::path(dir_local,out_file)
                  )

}
