# Load required package for XML parsing
library(xml2)
library(tidyr)
library(sf)
library(leaflet)

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
    return(list(kml_df = kml_df, geojson_file = geojson_file, kml_file = kml_file))
}
