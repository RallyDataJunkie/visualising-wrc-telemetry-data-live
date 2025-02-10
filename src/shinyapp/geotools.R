# Load required package for XML parsing
library(xml2)
library(tidyr)
library(sf)
library(leaflet)
library(trajr)
# library(amt)

download_kml <- function(kml_stub) {
    kml_url <- paste0("https://webapps2.wrc.com/2020/web/live/kml/", kml_stub, ".xml")
    # Download the KML file
    kml_file <- paste0(kml_stub, ".xml")
    if (file.exists(kml_file)) {
        print(paste("Reusing", kml_file))
    } else {
        print(paste("Downloading", kml_url, "to", kml_file))
        download.file(kml_url, kml_file, mode = "wb")
    }

    kml_file
}

kml_to_geojson_rally <- function(kml_file, kml_stub) {
    geojson_filename <- paste0(kml_stub, ".geojson")
    if (file.exists(geojson_filename)) {
        print(paste("Reusing", geojson_filename))
    } else {
        print(paste("Converting KML file ", kml_file, "to geojson file", geojson_filename))
        kml_string <- readLines(kml_file, warn = FALSE) %>% paste(collapse = "\n")


        # Parse KML string
        doc <- read_xml(kml_string)

        # Get the namespaces
        ns <- xml_ns(doc)

        # Debug: Print found namespaces
        print("Namespaces found:")
        print(ns)

        # Get all Placemarks with namespace
        placemarks <- xml_find_all(doc, "//kml:Placemark", ns)

        # Debug: Print number of placemarks

        # Initialize features list
        features <- list()

        # Process each placemark
        for (i in seq_along(placemarks)) {
            placemark <- placemarks[[i]]

            # Debug: Print current placemark processing
            # print(paste("Processing placemark", i))

            # Get name with namespace
            name <- xml_text(xml_find_first(placemark, ".//kml:name", ns))
            # print(paste("Name:", name))

            # Get coordinates with namespace
            coords_node <- xml_find_first(placemark, ".//kml:coordinates", ns)

            if (!is.na(coords_node)) {
                coords_str <- xml_text(coords_node)

                # Clean and parse coordinates
                coords_clean <- gsub("\\s+", " ", trimws(coords_str))
                coords_split <- strsplit(coords_clean, " ")[[1]]

                # Convert coordinates to matrix
                coords_matrix <- do.call(rbind, lapply(coords_split, function(x) {
                    parts <- as.numeric(strsplit(x, ",")[[1]])
                    if (length(parts) >= 2) {
                        c(parts[1], parts[2]) # Only take lon,lat
                    }
                }))

                # Remove any NULL rows from invalid coordinates
                coords_matrix <- coords_matrix[!is.na(coords_matrix[, 1]), ]

                # Create feature
                feature <- list(
                    type = "Feature",
                    properties = list(
                        name = name
                    ),
                    geometry = list(
                        type = "LineString",
                        coordinates = coords_matrix
                    )
                )

                features[[i]] <- feature
            }
        }

        # Create GeoJSON structure
        geojson <- list(
            type = "FeatureCollection",
            features = features
        )

        # Convert to JSON
        geojson_data <- jsonlite::toJSON(geojson, auto_unbox = TRUE, pretty = TRUE)

        fileConn <- file(geojson_filename)
        writeLines(geojson_data, fileConn)
    }
    geojson_filename
}

get_kml_geodf <- function(urlstub) {
    kml_file <- download_kml(urlstub)

    geojson_file <- kml_to_geojson_rally(kml_file, urlstub)

    kml_sf <- st_read(geojson_file)
    kml_sf <- st_zm(kml_sf, drop = TRUE, what = "ZM")

    kml_df <- as.data.frame(kml_sf)
    return(list(kml_sf = kml_sf, kml_df = kml_df, geojson_file = geojson_file, kml_file = kml_file))
}

# Detect the UTM zone as an EPSG code
lonlat2UTMzone <- function(lonlat) {
    utm <- (floor((lonlat[1] + 180) / 6) %% 60) + 1
    if (lonlat[2] > 0) {
        utm + 32600
    } else {
        utm + 32700
    }
}


get_trj <- function(route) {
    TrajFromCoords(as.data.frame(st_coordinates(route$geometry)))
}

compass_relative_turn <- function(route, angle = 0) {
    TrajMeanVectorOfTurningAngles(route, angle)
}

# to_amt_track = function(route){
#  make_track(st_coordinates(route$geometry), X, Y)
# }

get_utm_projection <- function(routes) {
    # Keep track of the original proj4 string
    old_crs <- st_crs(routes[1, ])$proj4string

    sample_location_x <- st_coordinates(st_centroid(routes[1, ]))[1]
    sample_location_y <- st_coordinates(st_centroid(routes[1, ]))[2]

    # Generate a new projection in the appropriate UTM zone
    crs_zone <- lonlat2UTMzone(c(
        sample_location_x,
        sample_location_y
    ))

    new_proj4_string <- st_crs(crs_zone)$proj4string

    # Transform the route to the UTM projection
    utm_routes <- st_transform(routes, crs = new_proj4_string)
    trjs <- apply(utm_routes, 1, get_trj)

    # Add some bendiness stats
    utm_routes["straightness"] <- unlist(lapply(trjs, TrajStraightness))
    utm_routes["sinuosity"] <- unlist(lapply(trjs, TrajSinuosity2))
    utm_routes["meanTurn"] <- unlist(lapply(
        trjs,
        TrajMeanVectorOfTurningAngles
    ))
    utm_routes["meanCompass"] <- unlist(lapply(
        trjs,
        compass_relative_turn
    ))

    # amt_tracks = apply(utm_routes, 1, to_amt_track)
    # utm_routes['amt_sin'] = unlist(lapply(amt_tracks, amt::sinuosity))
    # utm_routes['amt_str'] = unlist(lapply(amt_tracks, amt::straightness))
    # utm_routes['amt_cumd'] = unlist(lapply(amt_tracks, amt::cum_dist))
    # utm_routes['amt_totd'] = unlist(lapply(amt_tracks, amt::tot_dist))
    # utm_routes['amt_int'] = unlist(lapply(amt_tracks, amt::intensity_use))


    utm_routes
    # Or should we returned a named list
    # e.g. including the original projection?
    # list(utm_routes = utm_routes, orig_crs=old_crs)
}

segment_plot <- function(route_convexity, start, end, title = "", fix_coords = TRUE) {
    # Create the route distance filter limits
    segment_filter <- route_convexity$MidMeas >= start &
        route_convexity$MidMeas <= end

    # Filter the route
    route_segment <- route_convexity[segment_filter, ]

    # Generate the stylised route plot
    g <- ggplot(route_segment) +
        geom_path(aes(x = Midpoint_X, y = Midpoint_Y)) +
        geom_point(
            data = head(route_convexity[segment_filter, ], n = 1),
            aes(x = Midpoint_X, y = Midpoint_Y)
        ) +
        theme_void()

    if (title != "") {
        g <- g + ggtitle(title)
    }

    if (fix_coords) {
        g <- g + coord_fixed()
    }

    g
}


# The final section goes to the end of the route
segment_multiplot <- function(route_convexity, i, step_length, segment_length, final = FALSE) {
    # Preface the start of the stage with a 20m lead
    start_prefix <- 2 * step_length
    start <- segment_length * (i - 1) - start_prefix
    if (final) {
        end <- Inf
    } else {
        end <- (segment_length * i)
    }

    segment_plot(route_convexity, start, end, i, fix_coords = TRUE)
}
