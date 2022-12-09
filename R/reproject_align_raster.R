

#' Reprojects/resamples and aligns a raster
#'
#' Modified from a [function](https://github.com/ailich/mytools/blob/1c910f77e4d36e0965528975b13a02e77dcabe25/R/reproject_align_raster.R)
#' found on Github by [Alex](https://github.com/ailich).
#'
#' Deprecated. Use [terra::project()].
#'
#' Reprojects/resamples and aligns a raster by matching a raster a raster to a specified origin, resolution, and coordinate reference system, or that of a reference raster. Useful for preparing adjacent areas before using raster::merge or raster::mosaic. Also, see documentation for mytools::combine_rasters.
#' @param rast raster to be reprojected or resampled
#' @param ref_rast reference raster with desired properties (Alternatively can supply desired_origin, desired_res, and desired_crs)
#' @param desired_origin desired origin of output raster as a vector with length 2 (x,y)
#' @param desired_res  desired resolution of output raster. Either an integer or a vector of length 2 (x,y)
#' @param desired_crs desired coordinate reference system of output raster (CRS class)
#' @param method resampling method. Either "bilinear" for bilinear interpolation (the default), or "ngb" for using the nearest neighbor
#' @param outfile name of file to create
#' @param ... passed to [raster::writeRaster()].
#' @importFrom raster res
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
    desired_extent <- extent(ref_rast) # Desired extent

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

