
#' Download ftp files
#'
#' @param paths Paths to download. Possibly from [search_ftp_files()].
#' @param path_from,path_to Change `path_from` to `path_to` in the downloaded
#' file.
#' @param check_fun A function. Used to check if any previously downloaded
#' files can be read. If `check_fun` produces an error, those files will be
#' deleted before running `download.file`.
#' @param force_new Logical. If true, new files will be downloaded even if they
#' already exist in `path_to`.
#'
#' @return Called for side effect of saving ftp files to local directory
#' `path_to`.
#' @export
#'
#' @examples
get_ftp_files <- function(paths
                          , path_from = "ftp://qld.auscover.org.au"
                          , path_to = "../../data/raster/dynamic/AusCover"
                          , check_fun = NULL
                          , force_new = FALSE
                          ){

  copy_layers <- tibble::tibble(path = paths) %>%
    #dplyr::sample_n(10) %>% # TESTING
    dplyr::mutate(to_file = purrr::map_chr(path
                                           , ~fs::path(gsub(path_from
                                                            , path_to
                                                            , .
                                                            )
                                                       )
                                           )
                  , done = file.exists(to_file)
                  )

  if(force_new) copy_layers$done <- FALSE

  if(isTRUE(!is.null(check_fun))) {

    failing_layers <- copy_layers %>%
      dplyr::filter(done) %>%
      dplyr::mutate(is_file = purrr::map_lgl(to_file, fs::is_file)) %>%
      dplyr::filter(is_file) %>%
      dplyr::mutate(test = purrr::map(to_file, purrr::safely(check_fun))) %>%
      dplyr::mutate(pass = purrr::map_lgl(test, ~is.null(.$error))) %>%
      dplyr::filter(!pass)

    if(nrow(failing_layers) > 0) {

      purrr::walk(failing_layers$to_file
                  , fs::file_delete
                  )

    }

  }

  todo_layers <- copy_layers %>%
    dplyr::mutate(done = file.exists(to_file))

  copy_layers %>%
    dplyr::pull(to_file) %>%
    dirname(.) %>%
    unique() %>%
    purrr::walk(fs::dir_create)

  purrr::walk2(todo_layers$path[!todo_layers$done]
               , todo_layers$to_file[!todo_layers$done]
               , download.file
               , mode = "wb"
               )

}
