

#' Connect to access database via DBI function
#'
#' to enable use with dbplyr package. From icj's answer on
#' https://stackoverflow.com/questions/46458092/connection-from-access-to-r-via-dbi/48390093#48390093
#'
#' @param db_file_path
#'
#' @return
#' @export
#'
#' @examples
connect_to_access_dbi <- function(db_file_path)  {
  require(DBI)
  # make sure that the file exists before attempting to connect
  if (!file.exists(db_file_path)) {
    stop("DB file does not exist at ", db_file_path)
  }
  # Assemble connection strings
  dbq_string <- paste0("DBQ=", db_file_path)
  driver_string <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
  db_connect_string <- paste0(driver_string, dbq_string)

  myconn <- dbConnect(odbc::odbc(),
                      .connection_string = db_connect_string)
  return(myconn)
}
