
#' Prepare saving location in get functions
#'
#' @param save_dir
#' @param name
#'
#' @return
#' @export
#'
#' @examples
  file_prep <- function(save_dir
                        , name
                        , out_type = ".parquet"
                        ) {

    if(is.null(save_dir)) {

      save_dir <- here::here("out"
                             , "ds"
                             )

    }

    save_file <- fs::path(save_dir
                          , name
                          , paste0(name
                                   , "_raw"
                                   , out_type
                                   )
                          )

    fs::dir_create(dirname(save_file))

    return(save_file)

  }
