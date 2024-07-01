#' Get `data_name` data
#'
#' Import data, running `get_data_name` to requery original data source,
#' if `get_new`. Data is saved to (and imported from)
#' `file.path(save_dir, data_name, "paste0(data_name,"_raw.rds"))`. `save_dir`
#' and `get_new` can be passed via `...` to `get_data_name`, otherwise, default
#' values from `get_data_name` are used
#' (respectively: `FALSE` and
#' `here::here("out", "ds", data_name, paste0(data_name, "_raw.rds"))`)
#'
#' @param data_name Character. Name of data source. e.g. 'tern' or 'gbif'.
#' @param ... Passed to `get_data_name`.
#'
#' @return Dataframe, either loaded from `save_dir` or from a new query to
#' `data_name`. If new data is queried, .rds results file will be created,
#' overwriting if necessary. Timing and number of records log in `save_dir`.
#' @family Help with combining data sources
#' @export
#'
#' @examples
  get_data <- function(data_name
                       , ...
                       ) {

    dots <- list(...)
    get_new <- dots$get_new

    if(is.null(dots$save_dir)) {

      dots$save_dir <- here::here("out", "ds")

      save_dir <- dots$save_dir

    }

    start_time <- Sys.time()

    temp <- R.utils::doCall(paste0("get_",data_name)
                            , name = data_name
                            , args = dots
                            )

    readr::write_lines(paste0(Sys.time()
                              , ": "
                              , data_name
                              , if(get_new) " download" else " collection"
                              , " took "
                              , round(as.numeric(difftime(Sys.time()
                                                          , start_time
                                                          , units = c("mins")
                                                          )
                                                 )
                                      , 3
                                      )
                              , " minutes to return "
                              , format(nrow(temp), big.mark = ",")
                              , " records."
                              )
                       , file = fs::path(save_dir, "log.log")
                       , append = TRUE
                       )

    return(temp)

  }
