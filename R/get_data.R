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
#' @param data_name Character. Name of data source. e.g. 'tern' or 'galah'.
#' @param ... Passed to `get_data_name`
#'
#' @return Dataframe, either loaded from `save_dir` or from a new query to
#' `data_name`. If new data is queried, .rds results file will be created,
#' overwriting if necessary. Timing and number of records log in `save_dir`.
#' @family Help with combining data sources
#' @export
#'
#' @example inst/examples/get_data_ex.R
  get_data <- function(data_name
                       , ...
                       ) {

    start_time <- Sys.time()

    func <- get(paste0("get_", data_name))

    temp <- func(name = data_name
                 , ...
                 )

    return(temp)

  }


