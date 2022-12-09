
#' Get data from MS Access
#'
#' Generates an instance of 32-bit R, queries the database 'db_path' to
#' retrieve the table 'db_table' and makes the table available as the object
#' 'table_out'. Modified from the Stack Exchange Network
#' [post](https://stackoverflow.com/questions/13070706/how-to-connect-r-with-access-database-in-64-bit-window)
#'  by [manotheshark](https://stackoverflow.com/users/3242130/manotheshark).
#'
#' @param db_path Character. Path to MS Access database.
#' @param db_table Character. Name of table within database.
#' @param table_out Character. Name of object to which the table is assigned.
#'
#' @return Makes the table available as the object 'table_out'.
#' @export
#'
#' @examples
#'  access_query_32(db_path = "path/to/site.accdb", db_table = "sites", table_out = "sites")
#'
  access_query_32 <- function(db_path
                              , db_table = "qryData_RM"
                              , table_out = "data_access"
                              ) {

    # variables to make values uniform
    sock_port <- 8642L
    sock_con <- "sv_con"
    ODBC_con <- "a32_con"

    if (file.exists(db_path)) {

      # build ODBC string
      ODBC_str <- local({
        s <- list()
        s$path <- paste0("DBQ=", gsub("(/|\\\\)+", "/", path.expand(db_path)))
        s$driver <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)}"
        s$threads <- "Threads=4"
        s$buffer <- "MaxBufferSize=4096"
        s$timeout <- "PageTimeout=5"
        paste(s, collapse=";")
      })

      # start socket server to transfer data to 32 bit session
      svSocket::startSocketServer(port=sock_port, server_name="access_query_32", local=TRUE)

      # build expression to pass to 32 bit R session
      expr <- "library(svSocket)"
      expr <- c(expr, "library(RODBC)")
      expr <- c(expr, sprintf("%s <- odbcDriverConnect('%s')", ODBC_con, ODBC_str))
      expr <- c(expr, sprintf("if('%1$s' %%in%% sqlTables(%2$s)$TABLE_NAME) {%1$s <- sqlFetch(%2$s, '%1$s')} else {%1$s <- 'table %1$s not found'}", db_table, ODBC_con))
      expr <- c(expr, sprintf("%s <- socketConnection(port=%i)", sock_con, sock_port))
      expr <- c(expr, sprintf("evalServer(%s, %s, %s)", sock_con, table_out, db_table))
      expr <- c(expr, "odbcCloseAll()")
      expr <- c(expr, sprintf("close(%s)", sock_con))
      expr <- paste(expr, collapse=";")

      # launch 32 bit R session and run expressions
      prog <- file.path(R.home(), "bin", "i386", "Rscript.exe")
      system2(prog, args=c("-e", shQuote(expr)), stdout=NULL, wait=TRUE, invisible=TRUE)

      # stop socket server
      svSocket::stopSocketServer(port=sock_port)

      # display table fields
      message("retrieved: ", table_out, " - ", paste(colnames(get(table_out)), collapse=", "))

      } else {

        warning("database not found: ", db_path)

      }
  }
