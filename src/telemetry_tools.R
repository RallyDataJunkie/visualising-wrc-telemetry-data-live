library(sf)
library(stringr)

lonlat2UTM_hemisphere <- function(lonlat) {
  ifelse(lonlat[1] > 0, "north", "south")
}

lonlat2UTMzone = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}


get_utm_proj_string = function(route) {
  
  # Find the UTM zone for a sample a point on the route
  crs_zone = lonlat2UTMzone(c(st_coordinates(route)[1,1],
                              st_coordinates(route)[1,2]))
  
  # Create the projection string
  utm_proj4_string = st_crs(crs_zone)$proj4string
  #"+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs"
  # units in meters e.g. https://epsg.io/32632
  
  utm_proj4_string
  
}

to_utm = function(route) {
  route %>% st_transform(crs = st_crs(utm_proj4_string))
}


get_route = function(fn, ss, utm=TRUE) {
  
  geojson_sf = st_read(geojson_filename)
  
  # Give a stage code, try to extract it from the geojson object
  # Preview all routes: geojson_sf[[1]]
  # stages list: geojson_sf[[1]]
  # get stages from: geojson_sf[[1]] as nested list
  ss = str_replace(toString(ss),"SS", "")
  xx = str_split(str_replace(str_extract( geojson_sf[[1]], 
                                          "SS[0-9]+/?([0-9]?)+" ), "SS", ""),"/")
  contain_stage = function (x,y) y %in% x
  ss_index = unlist(lapply(xx, contain_stage, y=ss))
  
  stage_route  = geojson_sf[ss_index,]  %>% st_zm()
  
  # Grab a copy of the original projection
  original_crs = st_crs(stage_route)

  utm_proj4_string = get_utm_proj_string(stage_route)

  # Transform the route projection
  if (utm) {
    stage_route = to_utm(stage_route)
  }
  
  stage_route
}


# Get minimal telemetry file
get_min_telem = function(driver_name, stagenum, path, utm=FALSE) {
  fn = paste0("df_telemetrydata_SS",
              str_replace(toString(stagenum), "SS", ""),
              "_", driver_name, ".csv")
  telem_df_min = read.csv(file.path(path, fn) )  %>%
    map_df(rev)

  telem_df_min = telem_df_min %>%
    select(-c(accx, accy, altitude, brk, driverid,
              gear, rpm, throttle, track)) %>%
    select(-c(X_rally_stageid, X_carentryid, X_telemetryID))
  
  telem_df_min %>% drop_na(lon, lat) %>%
    st_as_sf(coords = c("lon","lat")) %>%
    st_set_crs(latlon_crs) %>%
    # Set datetime and delta_s
    mutate(delta_s=cumsum(c(0,diff(utx)))/1000)%>%
    mutate(utc = as.POSIXct(utx/1000,
                            origin = "1970-01-01",
                            tz = "Europe/Helsinki"))
}


get_buffered_route = function(stage_route_utm, buffer_width=50,
                              crs=NULL,  utm=TRUE) {
  # Generate buffered routes in UTM and longlat projections
  buffer_margin_100m = units::set_units(buffer_width, m)
  
  buffered_route = st_buffer(stage_route_utm, buffer_margin_100m)
  if (!is.null(crs)) {
    buffered_route =  st_transform(buffered_route_utm, crs)
  }
  
  buffered_route
}

# Get interection of telemetry and buffered route
get_route_telem = function(stage_route_utm, min_telem, min_telem_utm,
                           buffer_width=50, buffered_route=FALSE){
  
  if (!buffered_route) {
    buffered_route_utm = get_buffered_route(stage_route_utm, buffer_width)
  } else {
    buffered_route_utm = stage_route_utm
  }
  
  
  #Find the intersecting points
  # Could add some logic to trasnform CRS is they mismatch?
  route_telem_intersect = st_intersects(buffered_route_utm, min_telem_utm)
  
  # And then filter on those points
  # Also nullify the Z dimension
  min_telem[route_telem_intersect[[1]],]  %>% st_zm()
}



# Add distance along route measure to route
get_telem_dists = function(route_telem_utm, stage_route_utm) {
  
  min_pois_utm = st_sfc(st_multipoint(st_coordinates(route_telem_utm)),
                        crs=st_crs(route_telem_utm))
  
  # Handle the conversion from sf to sp objects
  # Generate a list of points from a multipoint
  # Via: https://github.com/r-spatial/sf/issues/114
  min_pois_points_utm = st_cast(x = min_pois_utm, to = "POINT")
  min_pois_points_utm_sp = as(min_pois_points_utm, 'Spatial')
  
  # Find the distance along the route of the point on the route
  # nearest to each telemetry sample
  dist_points = rgeos::gProject(as(stage_route_utm, "Spatial"),
                                min_pois_points_utm_sp, normalized = FALSE)
  
  zz = as.data.frame(dist_points)
  
  # Add the distance into stage for each point
  route_telem_utm$dist =  dist_points
  
  first_time = route_telem_utm[1,]$utc
  
  first_time_rounded = round_date(first_time, unit="1 minutes") 
  
  # Get stage time as a time object...
  route_telem_utm$roundeddelta_t = route_telem_utm$utc - first_time_rounded
  # And in seconds
  route_telem_utm$roundeddelta_s = as.double(route_telem_utm$roundeddelta_t)
  
  route_telem_utm
}

