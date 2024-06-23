
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
                        ) {

    if(is.null(save_dir)) {

      save_dir <- here::here("out"
                             , "ds"
                             )

    }

    save_file <- fs::path(save_dir
                          , name
                          , paste0(name
                                   , "_raw.rds"
                                   )
                          )

    fs::dir_create(dirname(save_dir))

    return(save_file)

  }
