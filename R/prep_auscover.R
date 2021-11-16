

#' Generate lists of paths and run `summarise_rast_paths`.
#'
#' Using the file naming conventions in the [AusCover data](), collate paths of
#' previously downloaded raster files and run
#' [envImport::summarise_raster_paths()], creating summary rasters for epochs.
#'
#' @param dir_local Character. Path to search for rasters to summarise.
#' @param dir_out Character. Path to save summarised rasters.
#' @param epoch_step Numeric. How many years in an epoch?
#' @param epoch_overlap Logical. Should epochs overlap by one year? i.e.
#' `epoch_overlap = TRUE` gives, say, 2000-2010 and 2010-2020 whereas
#' `epoch_overalp = FALSE` gives, say, 2000-2009 and 2010-2019.
#' @param force_new Logical. If true, existing summarised file(s) will be
#' overwritten.
#' @param ... Passed to [envImport::summarise_ftp_paths()].
#'
#' @return Side effect of creating raster summaries per epoch and func (see
#' `func` argument in [envImport::summarise_ftp_paths()]).
#' @export
#'
#' @examples
prep_auscover <- function(dir_local = "../../data/raster/AusCover/landsat"
                          , dir_out = "../../data/raster/Auscover"
                          , epoch_step = 10
                          , epoch_overlap = FALSE
                          , force_new = FALSE
                          , ...
                          ) {

  done_file <- fs::path(dir_out,"paths_done.csv")

  paths_done <- if(file.exists(done_file)) {

    rio::import(done_file)

  } else tibble::tibble(data = NULL)

  if(force_new) paths_done <- tibble::tibble(data = NULL)

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

  raster_summaries$done <- purrr::map_lgl(raster_summaries$data
                                          , ~all(. %in% paths_done$data)
                                          )

  # This just will not work with furrr::future_walk
  # many, many suggestions, iterations attempted.
  # The passing of paths, creating the stack within the function part of those iterations.
  # Also works with a single function that does all the calcs in one pass, but took 7.5 hours per epoch
  # (even with multiple cores passed to terra::app) compared with 20 minutes per function per epoch.

  purrr::walk2(raster_summaries$data[!raster_summaries$done]
               , raster_summaries$out_file[!raster_summaries$done]
               , summarise_rast_paths
               , ...
               )

  rio::export(raster_summaries %>%
                dplyr::select(Negate(where(is.logical))) %>%
                tidyr::unnest(cols = c(data))
              , done_file
              )

}
