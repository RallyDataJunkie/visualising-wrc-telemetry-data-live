#' Get Raster Elevation
#' 
#' Several web services provide access to raster elevation. Currently, this 
#' function provides access to the Amazon Web Services Terrain Tiles and the 
#' Open Topography global datasets API. The function accepts a \code{data.frame} 
#' of x (long) and y (lat), an \code{sf}, or \code{terra} object as input.  A 
#' \code{RasterLayer} object is returned. In subsequent versions, a \code{SpatRaster}
#' will be returned.
#' 
#' @param locations Either a \code{data.frame} of x (long) and y (lat), an 
#'                   \code{sf}, or \code{terra} object as input. 
#' @param z  The zoom level to return.  The zoom ranges from 1 to 14.  Resolution
#'           of the resultant raster is determined by the zoom and latitude.  For 
#'           details on zoom and resolution see the documentation from Mapzen at 
#'           \url{https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution}.
#'           The z is not required for the OpenTopography data sources. 
#' @param prj A valid input to \code{\link{st_crs}} If a \code{sf} 
#'            object or a \code{terra} object is provided as the \code{locations}, 
#'            the prj is optional and will be taken from \code{locations}.  This 
#'            argument is required for a \code{data.frame} of locations.
#' @param src A character indicating which API to use.  Currently supports "aws" 
#'            and "gl3", "gl1", "alos", or "srtm15plus" from the OpenTopography API global 
#'            datasets. "aws" is the default.
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can 
#'               be used for features that fall close to the edge of a tile or 
#'               for retrieving additional area around the feature. If the 
#'               feature is a single point, the area it returns will be small if 
#'               clip is set to "bbox". Default is NULL.
#' @param clip A character value used to determine clipping of returned DEM.  
#'             The default value is "tile" which returns the full tiles.  Other 
#'             options are "bbox" which returns the DEM clipped to the bounding 
#'             box of the original locations (or expanded bounding box if used), 
#'             or "locations" if the spatial data (e.g. polygons) in the input 
#'             locations should be used to clip the DEM.  Locations are not used 
#'             to clip input point datasets.  Instead the bounding box is used.
#' @param verbose Toggles on and off the note about units and coordinate 
#'                reference system.
#' @param neg_to_na Some of the data sources return large negative numbers as 
#'                  missing data.  When the end result is a projected those 
#'                  large negative numbers can vary.  When set to TRUE, only 
#'                  zero and positive values are returned.  Default is FALSE.
#' @param override_size_check Boolean to override size checks.  Any download 
#'                            between 100 Mb and 500Mb report a message but
#'                            continue.  Between 500Mb and 3000Mb requires 
#'                            interaction and greater than 3000Mb fails.  These
#'                            can be overriden with this argument set to TRUE.  
#' @param tmp_dir The location to store downloaded raster files.  Defaults to a 
#'                temporary location.  Alternatively, the user may supply an 
#'                existing path for these raster files.  New folders are not 
#'                created by \code{get_elev_raster}.
#' @param ncpu Number of CPU's to use when downloading aws tiles. Defaults to 2 
#'             if more than two available, 1 otherwise.                                              
#' @param ... Extra arguments to pass to \code{httr::GET} via a named vector, 
#'            \code{config}.   See
#'            \code{\link{get_aws_terrain}} for more details. 
#' @return Function returns a \code{RasterLayer} in the projection 
#'         specified by the \code{prj} argument or in the projection of the 
#'         provided locations.  In subsequent versions, a \code{SpatRaster}
#'         will be returned.
#' @details Currently, the \code{get_elev_raster} function utilizes the 
#'          Amazon Web Services 
#'          (\url{https://registry.opendata.aws/terrain-tiles/}) terrain 
#'          tiles and the Open Topography Global Datasets API 
#'          (\url{https://opentopography.org/developers}).  
#'          
#'          The AWS Terrain Tiles data is provided via x, y, and z tiles (see 
#'          \url{https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames} for 
#'          details.) The x and y are determined from the bounding box of the 
#'          object submitted for \code{locations} argument, and the z argument 
#'          must be specified by the user.   
#' @export
#' @examples 
#' \dontrun{
#' library(elevatr)
#' library(sf)
#' data(lake)
#' lake_buff  <- st_buffer(lake, 1000)
#' loc_df <- data.frame(x = runif(6,min=sf::st_bbox(lake)$xmin, 
#'                                max=sf::st_bbox(lake)$xmax),
#'                      y = runif(6,min=sf::st_bbox(lake)$ymin, 
#'                                max=sf::st_bbox(lake)$ymax))
#'                                
#' x <- get_elev_raster(locations = loc_df, prj = st_crs(lake) , z=10)
#' x <- get_elev_raster(lake, z = 14)
#' x <- get_elev_raster(lake, src = "gl3", expand = 5000)
#' x <- get_elev_raster(lake_buff, z = 10, clip = "locations")
#' }

get_elev_raster <- function(locations, z, prj = NULL, 
                            src = c("aws", "gl3", "gl1", "alos", "srtm15plus"),
                            expand = NULL, clip = c("tile", "bbox", "locations"), 
                            verbose = TRUE, neg_to_na = FALSE, 
                            override_size_check = FALSE, tmp_dir = tempdir(),
                            ncpu = ifelse(future::availableCores() > 2, 2, 1),
                            ...){
  
  tmp_dir <- normalizePath(tmp_dir, mustWork = TRUE)
  src  <- match.arg(src)
  clip <- match.arg(clip) 
  
  # Check location type and if sf, set prj.  If no prj (for either) then error
  locations <- loc_check(locations,prj)
  
  if(is.null(prj)){
    prj <- sf::st_crs(locations)
  }
   #need to check what is going on with PRJ when no prj passed.
  # Check download size and provide feedback, stop if too big!
  dl_size <- estimate_raster_size(locations, prj, src, z)
  if(dl_size > 500 & dl_size < 1000){
    message(paste0("Note: Your request will download approximately ", 
                   round(dl_size, 1), "Mb."))
  } else if(dl_size > 1000 & dl_size <= 3000){
    message(paste0("Your request will download approximately ",
                   round(dl_size, 1), "Mb."))
    if(!override_size_check){
      y <- readline(prompt = "Press [y] to continue with this request.")
      if(tolower(y) != "y"){return()}
    }
  } else if(!override_size_check & dl_size > 3000){
    stop(paste0("Your request will download approximately ",
                   round(dl_size, 1), "Mb. That's probably too big. If you 
                   really want to do this, set override_size_check = TRUE. Note
                   that the OpenTopography API Limit will likely be exceeded."))
  }
  
  
  # Pass of locations to APIs to get data as raster
  if(src == "aws") {
    raster_elev <- get_aws_terrain(locations, z, prj = prj, expand = expand, 
                                   tmp_dir = tmp_dir, ncpu = ncpu, ...)
  } else if(src %in% c("gl3", "gl1", "alos", "srtm15plus")){
    raster_elev <- get_opentopo(locations, src, prj = prj, expand = expand, 
                                tmp_dir = tmp_dir, ...)
  }
  sources <- attr(raster_elev, "sources")
  if(is.null(sources)){sources <- src}
  
  if(clip != "tile"){
    message(paste("Clipping DEM to", clip))
    
    raster_elev <- clip_it(raster_elev, locations, expand, clip)
  }
 
  if(verbose){
    message(paste("Note: Elevation units are in meters."))
  }
  
  
  if(neg_to_na){
    raster_elev[raster_elev < 0] <- NA
  }
  
  attr(raster_elev, "sources") <- sources
  #Returning raster for now
  #Switch to SpatRaster in near future.
  raster::raster(raster_elev)
  
}


#' Merge Rasters
#' 
#' Merge multiple downloaded raster files into a single file. The input `target_prj` 
#' describes the projection for the new grid.
#' 
#' @param raster_list a list of raster file paths to be mosaiced
#' @param target_prj the target projection of the output raster
#' @param method the method for resampling/reprojecting. Default is 'bilinear'. 
#' Options can be found [here](https://gdal.org/programs/gdalwarp.html#cmdoption-gdalwarp-r)
#' @param returnRaster if TRUE, return a raster object (default), else, return the file path to the object
#' @param tmp_dir The location to store downloaded raster files.  Defaults to a 
#'                temporary location.  Alternatively, the user may supply an 
#'                existing path for these raster files.  New folders are not 
#'                created by \code{get_elev_raster}.
#' @export
#' @keywords internal
          
merge_rasters <- function(raster_list,  target_prj, method = "bilinear", 
                          returnRaster = TRUE, tmp_dir = tempdir()){
  
  message(paste("Mosaicing & Projecting"))
  
  destfile <- tempfile(tmpdir = tmp_dir, fileext = ".tif")
  files    <- unlist(raster_list)
 
  if(is.null(target_prj)){
    r <- terra::rast(files[1])
    target_prj <- terra::crs(r)
  }
  
  sf::gdal_utils(util = "warp", 
                 source = files, 
                 destination = destfile,
                 options = c("-r", method)
             )
  # Using two steps now as gdal with one step introduced NA's along seams
  # Slower but more accurate!
  destfile2 <- tempfile(tmpdir = tmp_dir, fileext = ".tif")
  sf::gdal_utils(util = "warp", 
                 source = destfile, 
                 destination = destfile2,
                 options = c("-r", method,
                   "-t_srs", sf::st_crs(target_prj)$wkt)
  )
  
  if(returnRaster){
    terra::rast(destfile2)
  } else {
    destfile2
  }
}

