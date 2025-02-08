ll_geo <- sf::st_crs(4326)

# CORS proxy function for webR environment
make_cors_url <- function(url, params = NULL) {
  if (!is.null(params)) {
    param_string <- paste(
      mapply(
        function(k, v) paste0(URLencode(k), "=", URLencode(v)),
        names(params),
        params
      ),
      collapse = "&"
    )
    url <- paste0(url, "?", param_string)
  }
  paste0("https://corsproxy.io/", URLencode(url))
}


#' Get a digital elevation model from the AWS Terrain Tiles
#' 
#' This function uses the AWS Terrain Tile service to retrieve an elevation
#' raster from the geotiff service.  It accepts a \code{sf::st_bbox} object as 
#' input and returns a single raster object covering that extent.   
#' 
#' @source Attribution: Mapzen terrain tiles contain 3DEP, SRTM, and GMTED2010 
#'         content courtesy of the U.S. Geological Survey and ETOPO1 content 
#'         courtesy of U.S. National Oceanic and Atmospheric Administration. 
#'         \url{https://github.com/tilezen/joerd/tree/master/docs} 
#' 
#' @param locations Either a \code{data.frame} of x (long) and y (lat), an 
#'                  \code{sp}, \code{sf}, or \code{raster} object as input.
#' @param z The zoom level to return.  The zoom ranges from 1 to 14.  Resolution
#'          of the resultant raster is determined by the zoom and latitude.  For 
#'          details on zoom and resolution see the documentation from Mapzen at 
#'          \url{https://github.com/tilezen/joerd/blob/master/docs/data-sources.md#what-is-the-ground-resolution}
#' @param prj A valid input to \code{\link{st_crs}} If a \code{sf} 
#'            object or a \code{terra} object is provided as the \code{locations}, 
#'            the prj is optional and will be taken from \code{locations}.  This 
#'            argument is required for a \code{data.frame} of locations.
#' @param expand A numeric value of a distance, in map units, used to expand the
#'               bounding box that is used to fetch the terrain tiles. This can 
#'               be used for features that fall close to the edge of a tile and 
#'               additional area around the feature is desired. Default is NULL.
#' @param ncpu Number of CPU's to use when downloading aws tiles. Defaults to 2 
#'             if more than two available, 1 otherwise.  
#' @param serial Logical to determine if API should be hit in serial or in 
#'               parallel.  TRUE will use purrr, FALSE will use furrr. 
#' @param tmp_dir The location to store downloaded raster files.  Defaults to a 
#'                temporary location.  Alternatively, the user may supply an 
#'                existing path for these raster files.  New folders are not 
#'                created by \code{get_elev_raster}.
#' @param ... Extra configuration parameters to be passed to httr::GET.  Common 
#'            usage is to adjust timeout.  This is done as 
#'            \code{config=timeout(x)} where \code{x} is a numeric value in 
#'            seconds.  Multiple configuration functions may be passed as a 
#'            vector.              
#' @export
#' @importFrom
#' @keywords internal

get_aws_terrain <- function(locations, z, prj, expand=NULL, 
                            ncpu = ifelse(future::availableCores() > 2, 2, 1),
                            serial = NULL, tmp_dir = tempdir(),  use_cors=F, ...){
  # Expand (if needed) and re-project bbx to dd
  
  bbx <- proj_expand(locations,prj,expand)
  
  base_url <- "https://s3.amazonaws.com/elevation-tiles-prod/geotiff"
  
  
  tiles <- get_tilexy(bbx,z)
  
  urls  <-  sprintf("%s/%s/%s/%s.tif", base_url, z, tiles[,1], tiles[,2])
  
  dir <- tempdir()
  
  nurls <- length(urls)
  if(is.null(serial)){
    if(nurls < 175){
      serial <- TRUE
    } else {
      serial <- FALSE
    }
  }
  


  if(serial){
    
      dem_list <- lapply(urls, function(x) {
    
    tmpfile <- tempfile(tmpdir = tmp_dir, fileext = ".tif")
    
    # Apply CORS proxy if needed
   
    download_url <- if(use_cors) make_cors_url(x) else x
    
    # Use download.file with method="internal" for webR compatibility
    tryCatch({
      download.file(
        download_url, tmpfile
        #url = download_url,
        #destfile = tmpfile,
        #mode = "wb",
        #method = "internal",
        #quiet = TRUE
      )
      
      # Basic check if file exists and has content
      if (!file.exists(tmpfile) || file.size(tmpfile) == 0) {
        stop(paste("Failed to download from:", x))
      }
      
      # Note: We can't get headers like x-amz-meta-x-imagery-sources with base R
      # So we'll just return the tmpfile without the source attribute
      tmpfile
      
    }, error = function(e) {
      stop(paste("Error downloading from:", x, "\nError:", e$message))
    })
  })
  } 

  
  merged_elevation_grid <- merge_rasters(dem_list, target_prj = prj, tmp_dir = tmp_dir)
  sources <- unlist(lapply(dem_list, function(x) attr(x, "source")))
  if(!is.null(sources)){
    sources <- trimws(unlist(strsplit(sources, ",")))
    sources <- strsplit(sources, "/")
    sources <- unlist(unique(lapply(sources, function(x) x[1])))
  }
  attr(merged_elevation_grid, "sources") <- 
    paste(sources, collapse = ",")
  
  if(serial==FALSE){future::plan(future::sequential)}
  
  merged_elevation_grid 
}