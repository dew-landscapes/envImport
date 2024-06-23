#' Prepare `data_name` for `unite_data`
#'
#' Select appropriate columns from already loaded obj. Some make
#' functions also do further work to convert, adjust and/or ensure data types
#' are aligned for `envImport::unite_data()`.
#'
#' @param data_name Character. Name of data source. e.g. 'tern' or 'gbif'.
#' @param ... Passed to `make_data_name`.
#'
#' @return Dataframe, adjusted from previous dataframe `data_name_raw`
#' @family Help with combining data sources
#' @export
#'
#' @examples
  make_data <- function(obj
                        , data_name
                        , ...
                        ) {

    temp <- R.utils::doCall(paste0("make_",data_name)
                            , obj = obj
                            , name = data_name
                            , args = ...
                            )

    return(temp)

  }
