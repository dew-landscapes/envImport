
#' How many days since the data source was queried?
#'
#' If file exits, time since file was created, else `Inf`.
#'
#' @param data_name Character. name of data source. e.g. 'BDBSA' or 'GBIF'.
#' This is used to generate file location.
#'
#' @return Numeric. Days since last update.
#' @family Help with combining data sources
#' @export
#'
#' @examples
#' days_since_update("BDBSA")
#'
  days_since_update <- function(data_name) {

    ds_file <- base::file.path("out"
                               , "ds"
                               , paste0(data_name,".csv")
                               )

    if(file.exists(ds_file)) {

      base::difftime(base::Sys.time()
                     , base::file.mtime(ds_file)
                     , units = "days"
                     ) %>%
        as.numeric() %>%
        round(1)

    } else Inf

  }


