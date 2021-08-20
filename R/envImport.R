#' Reprojects/resamples and aligns a raster
#'
#' Modified from https://github.com/ailich/mytools/blob/1c910f77e4d36e0965528975b13a02e77dcabe25/R/reproject_align_raster.R
#'
#' Reprojects/resamples and aligns a raster by matching a raster a raster to a specified origin, resolution, and coordinate reference system, or that of a reference raster. Useful for preparing adjacent areas before using raster::merge or raster::mosaic. Also, see documentation for mytools::combine_rasters.
#' @param rast raster to be reprojected or resampled
#' @param ref_rast reference raster with desired properties (Alternatively can supply desired_origin, desired_res, and desired_crs)
#' @param desired_origin desired origin of output raster as a vector with length 2 (x,y)
#' @param desired_res  desired resolution of output raster. Either an integer or a vector of length 2 (x,y)
#' @param desired_crs desired coordinate reference system of output raster (CRS class)
#' @param method resampling method. Either "bilinear" for bilinear interpolation (the default), or "ngb" for using the nearest neighbor
#' @param outfile name of file to create
#' @param ... passed to writeRaster
#' @importFrom raster crs
#' @importFrom raster extent
#' @importFrom raster origin
#' @importFrom raster projectExtent
#' @importFrom raster raster
#' @importFrom raster resample
#' @importFrom raster projectRaster
#' @export
  reproject_align_raster<- function(rast
                                    , ref_rast = NULL
                                    , desired_origin
                                    , desired_res
                                    , desired_crs
                                    , method = "bilinear"
                                    , outfile = NULL
                                    , ...
                                    ){

    #Set parameters based on ref rast if it was supplied
    if (!is.null(ref_rast)) {

      desired_origin <- origin(ref_rast) # Desired origin
      desired_res <- res(ref_rast) # Desired resolution
      desired_crs <- crs(ref_rast) # Desired crs
      desired_extent <- raster::extent(ref_rast) # Desired extent

    }

    # Ensure desired resolution is length 2
    if(length(desired_res)==1){

      desired_res <- rep(desired_res,2)

      }

    # Test if already identical crs, origin, resolution and extent (i.e. Raster already aligned)
    if(identical(crs(rast), desired_crs) &
       isTRUE(all.equal(origin(rast), desired_origin)) &
       identical(desired_res, res(rast)) &
       isTRUE(all.equal(extent(rast),desired_extent))
       ){

      message("raster was already aligned")

      return(rast)

    }


    # reproject desired extent crs

    rast_orig_extent <- extent(projectExtent(object = ref_rast, crs = desired_crs))

    var1 <- floor((rast_orig_extent@xmin - desired_origin[1])/desired_res[1])

    new_xmin <- desired_origin[1] + desired_res[1]*var1 #Calculate new minimum x value for extent

    var2 <- floor((rast_orig_extent@ymin - desired_origin[2])/desired_res[2])

    new_ymin <- desired_origin[2]+ desired_res[2]*var2 #Calculate new minimum y value for extent

    n_cols <- ceiling((rast_orig_extent@xmax-new_xmin)/desired_res[1]) #number of cols to be in output raster

    n_rows <- ceiling((rast_orig_extent@ymax-new_ymin)/desired_res[2]) #number of rows to be in output raster

    new_xmax <- new_xmin+(n_cols*desired_res[1]) #Calculate new max x value for extent

    new_ymax <- new_ymin+(n_rows*desired_res[2]) #Calculate new max y value for extent

    #Create a blank template raster to fill with desired properties
    rast_new_template <- raster(xmn = new_xmin
                               , xmx = new_xmax
                               , ymn = new_ymin
                               , ymx = new_ymax
                               , res = desired_res
                               , crs = desired_crs
                               )

    # Throw error if origin doesn't match
    if(!isTRUE(all.equal(desired_origin,origin(rast_new_template)))) {

      message("desired origin does not match output origin")

      stop()

      }

    # Use projectRaster if crs doesn't match and resample if they do
    if(identical(crs(rast),desired_crs)){

      rast_new <- resample(x = rast
                           , y = rast_new_template
                           , method = method
                           , filename = outfile
                           , ...
                           )

    } else {

      rast_new <- projectRaster(from = rast
                                , to = rast_new_template
                                , method = method
                                , filename = outfile
                                , ...
                                )

    }


    if(!isTRUE(all.equal(desired_origin,origin(rast_new)))){

      message("desired origin does not match output origin")

      stop()

      } #Throw error if origin doesn't match

    return(rast_new)

  }


#' Create an 'area of interest'
#'
#' @param polygons sf.
#' @param filterpolys character. Used to filter filterpolyscol.
#' @param filterpolyscol character. Which column to filter on.
#' @param buffer numeric. Create a buffer around the area of interest of
#' buffer metres.
#' @param domask logical. If FALSE, just use extent of polyBuffer.
#' @param usecrs numeric. [EPSG](https://epsg.io/) code giving coordinate
#' system to use in output sf.
#'
#' @return sf.
#' @export
#'
#' @examples
  make_aoi <- function(polygons
                       , filterpolys = FALSE
                       , filterpolyscol = NULL
                       , buffer
                       , domask = TRUE
                       , usecrs = 3577
                       ) {

    if(!isFALSE(filterpolys)) {

      keepRows <- polygons %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::pull(!!ensym(filterpolyscol)) %>%
        grep(paste0(filterpolys,collapse = "|"),.)

      polygons <- polygons %>%
        dplyr::slice(keepRows)

      }

    polygons <- if(domask) {

      polygons %>%
        dplyr::mutate(dissolve = 1) %>%
        dplyr::summarise(Include = n()) %>%
        sf::st_cast() %>%
        sf::st_buffer(buffer)

    } else {

      polygons %>%
        dplyr::mutate(dissolve = 1) %>%
        dplyr::summarise(Include = n()) %>%
        sf::st_cast() %>%
        sf::st_buffer(buffer) %>%
        sf::st_bbox() %>%
        sf::st_as_sfc() %>%
        sf::st_sf() %>%
        dplyr::mutate(Include = 1)

    }

    polygons <- polygons %>%
      sf::st_transform(crs = usecrs)

  }

#' Collect rasters from raster folder into named list
#'
#' @param rasterfolder Character. Path to folder containing rasters
#' @param greptype Character. What type of rasters (e.g. 'tif')
#' @param pattern Character. Optional. Further pattern to filter resulting list
#' of rasters.
#'
#' @return List. Named list of rasters (name is from file name)
#' @export
#'
#' @examples
  raster_prep <- function(rasterfolder, greptype = c("tif","grd"), pattern = NULL) {

    rastersall <- tibble::tibble(path = fs::dir_ls(rasterfolder)) %>%
      dplyr::filter(grepl(paste0(greptype,"$",collapse = "|"),path)) %>%
      {if(!is.null(pattern)) (.) %>% dplyr::filter(grepl(paste0(pattern,collapse = "|"),path)) else (.)} %>%
      dplyr::mutate(file = purrr::map_chr(path,fs::path_file)
                    , name = purrr::map_chr(file,~gsub("\\.tif","",.))
                    , ras = purrr::map(path,raster::raster)
                    )

    rastersall$ras %>%
      stats::setNames(rastersall$name)

  }


#' Use a data map to combine several data sources into one data frame
#'
#' @param this_name Character. name of the data source.
#' @param df Dataframe containing the columns to select and (potentially) rename
#' @param names_map Dataframe mapping old names to new names
#' @param exclude_names Character. column names in namesmap to exclude from the
#' combined data
#'
#' @return Tibble with select and renamed columns
#' @family Help with combining data sources
#' @export
#'
#' @examples
  remap_data_names <- function(this_name, df, names_map, exclude_names = c("data_name","order","days")) {

    these_names <- names_map %>%
      dplyr::filter(data_name == this_name) %>%
      dplyr::select(grep(paste0(exclude_names,collapse = "|"),names(.),invert = TRUE, value = TRUE))

    old_names <- these_names %>%
      unlist(., use.names=FALSE) %>%
      stats::na.omit() %>%
      unname()

    new_names <- these_names %>%
      janitor::remove_empty("cols") %>%
      names()

    res <- df %>%
      dplyr::select(all_of(old_names)) %>%
      stats::setNames(new_names) %>%
      tibble::as_tibble()

    has_date <- "date" %in% names(res)

    if(has_date) {

      date_done <- lubridate::is.Date(res$date)

      if(!date_done) {

        res$date <- lubridate::as_date(res$date)

      }

      res <- res %>%
        dplyr::filter(!is.na(date))

    }

    if("site" %in% names(res)){

      res$site = as.character(res$site)

    }

    res <- res  %>%
      dplyr::filter(!is.na(lat)
                    , !is.na(long)
                    )

  }

#' How many days since the data source was queried?
#'
#' If file exits, time since file was created, else NA
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

    ds_file <- base::file.path("out","ds",paste0(data_name,".feather"))

    if(file.exists(ds_file)) {

      base::difftime(base::Sys.time()
                   , base::file.mtime(ds_file)
                   , units = "days"
                   ) %>%
        as.numeric() %>%
        round(1)

      } else Inf

  }


#' Get data
#'
#' Import data, running 'get_dataname' to requery original data source,
#' if 'lgl'. Optionally times the query, if timeR exists and is specified. Data
#' is saved to (and imported from)
#' file.path("out","ds","paste0(dataname,".feather"))
#'
#' @param data_name Character. Name of home data source. e.g. 'BDBSA' or 'GBIF'.
#' This is used to generate file location.
#' @param lgl Logical. Usually generated by get_new_data
#' @param timer Character. timeR object name, if one exists.
#'
#' @return Data from out/ds/dataname.feather. If new data is queried,
#' out/ds/dataname.feather will be created, overwriting if necessary.
#' @family Help with combining data sources
#' @export
#'
#' @examples
  get_data <- function(data_name,lgl,timer = "extractTimer") {

    ds_file <- base::file.path("out","ds",paste0(data_name,".feather"))

    if(lgl) {

      data_funtion <- paste0("get_",data_name)

      if(exists(timer)) get(timer)$start(data_name)

      temp <- do.call(data_funtion
                      , list(ds_file)
                      )

      if(exists(timer)) get(timer)$stop(data_name,comment = paste0(nrow(temp)," records"))

    } else {

      temp <- rio::import(ds_file)

    }

    return(temp)

  }


#' Get data from MS Access
#'
#' Generates an instance of 32-bit R, queries the database 'db_path' to
#' retrieve the table 'db_table' and makes the table available as the object
#' 'table_out'. Sourced from the [Stack Exchange Network](https://stackoverflow.com/questions/13070706/how-to-connect-r-with-access-database-in-64-bit-window)
#' post by [manotheshark](https://stackoverflow.com/users/3242130/manotheshark).
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
  access_query_32 <- function(db_path, db_table = "qryData_RM", table_out = "data_access") {

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
      svSocket::startSocketServer(port=sock_port, server.name="access_query_32", local=TRUE)

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


#' Create unified data source
#'
#' @param data_map Dataframe. Needs to contain columns `data_name` and `days`.
#' Other columns are the column names in the unified data source. Values
#' against each `data_name` contain the name of the column in the original data
#' source that should map to the current column name.
#' @param override_days Logical over-ride of `get_new` output.
#'
#' @return single data frame unifying the data from the input `data_name`s
#' @family functions to help with combining data sources
#' @export
#'
#' @examples
  unify_data_sources <- function(data_map, override_days = NULL) {

    data_map %>%
      dplyr::select(data_name,days) %>%
      dplyr::mutate(get_new = purrr::map_dbl(data_name,envImport::days_since_update)
                    , get_new = get_new > days
                    , get_new = if(!is.null(override_days)) override_days else get_new
                    , dat = purrr::map2(data_name,get_new,envImport::get_data)
                    , dat = purrr::map2(data_name,dat,envImport::remap_data_names,names_map = data_map)
                    ) %>%
      tidyr::unnest(cols = c(dat)) %>%
      dplyr::select(!tidyselect::matches("^n$"))

  }
