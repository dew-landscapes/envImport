
#' Search for files on an ftp site
#'
#' Defaults to [TERN](http://qld.auscover.org.au/public/html/index.html)
#' AusCover landsat.
#'
#' @param dir_ftp Character. URL (directory on ftp site to search).
#' @param dir_regex Character. Search string to limit results from within `dir_ftp`.
#' @param recurse Logical. Recursively search `dir_ftp`?
#'
#' @return Tibble with columns
#' \describe{
#'   \item{file}{If path is a file, this will contain `basename(path)`.}
#'   \item{path}{Full path to file for use in [get_ftp_files()].}
#' }
#' @export
#'
#' @examples
#' search_ftp_files()
  search_ftp_files <- function(dir_ftp = "ftp://qld.auscover.org.au/landsat"
                               , dir_regex = NULL
                               , recurse = FALSE
                               ) {

    todo <- tibble::tibble(path = dir_ftp)

    done <- tibble::tibble()

    while(nrow(todo) > 0) {

      res <- todo %>%
        dplyr::mutate(files = purrr::map(path, threadr::list_files_ftp)) %>%
        tidyr::unnest(cols = c(files)) %>%
        dplyr::mutate(files = gsub("\\/{2}","/"
                                   , files
                                   )
                      , files = gsub("\\/$","",files)
                      , files = gsub(":/","://",files)
                      , done = path == files | grepl("\\.[[:alpha:]]{2,5}$", files)
                      , is_file = grepl("\\.[[:alpha:]]{2,5}$", files)
                      , path = dplyr::if_else(!grepl("\\.[[:alpha:]]{2,5}$"
                                                       , files
                                                       )
                                                , files
                                                , path
                                                )
                      )

      done <- done %>%
        dplyr::bind_rows(res %>%
                           dplyr::filter(done)
                         )

      if(recurse) {

        todo <- res %>%
          dplyr::filter(done == FALSE)

      } else {

        todo <- tibble::tibble()

        done <- res

      }

    }

    done %>%
      {if(isTRUE(!is.null(dir_regex))) (.) %>%
          dplyr::filter(grepl(paste0(dir_regex
                                     , collapse = "|"
                                     )
                              , files
                              )
                        ) else (.)
        } %>%
      dplyr::mutate(file = ifelse(is_file
                                  , basename(files)
                                  , NA
                                  )
                    ) %>%
      dplyr::select(file
                    , path = files
                    ) %>%
      dplyr::distinct()

  }
