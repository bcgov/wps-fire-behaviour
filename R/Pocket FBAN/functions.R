# # Load required packages - if not installed, install them first
# Check that the required packages are installed, if not, install them
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
if (!requireNamespace("ows4R", quietly = TRUE)) install.packages("ows4R")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("terra", quietly = TRUE)) install.packages("terra")

pacman::p_load(
  httr, ows4R, tidyverse, sf, terra
)

# Modified clipGrids to work in Shiny app environment
# Last modified: 2025-12-05
# TO DO:
# # NOTE:
#   # The "terra" package won't work if Prometheus is installed.
#   # The following line of code will fix any PROJ_LIB conflict for this R session.
#   # Review these online instructions for how to fix this for all future R sessions.
#   # https://docs.ropensci.org/fireexposuR/articles/prometheus.html
#
# Function to clip fuel and elevation rasters using the projected Coordinate Reference System

clipGrids <- function(fuel, elev, latlon, width, height = width, cellsize, filenames) {
  # Set PROJ_LIB to avoid conflicts with Prometheus if installed
  Sys.setenv(PROJ_LIB = "")

  # Create projected SpatVector from lat/lon coordinates
  xy <- data.frame(longitude = latlon[2], latitude = latlon[1])
  xy <- vect(x = xy, geom = c("longitude", "latitude"), crs = "epsg:4326")
  xy <- project(x = xy, y = crs(fuel))
  xy <- crds(xy, df = FALSE, list = FALSE)

  # Create clip extent around xy coordinate (xmin, xmax, ymin, ymax)
  clipExt <- ext(
    xy[1] - width,
    xy[1] + width,
    xy[2] - height,
    xy[2] + height
  )

  # Align clip extent to fuel grid
  clipExt <- align(x = clipExt, y = fuel, snap = "near")

  # Save datatypes of each grid
  dtfuel <- datatype(fuel)
  dtelev <- datatype(elev)

  # Clip and resample fuel grid
  if (!missing(fuel)) {
    fuel <- crop(
      x = fuel,
      y = clipExt,
      snap = "near"
    )

    if (!missing(cellsize) & any(res(fuel) != cellsize)) {
      fuel <- project(
        x = fuel,
        y = paste0("EPSG:", crs(fuel, describe = TRUE)$code),
        method = "mode",
        res = cellsize
      )
    }

    if (!missing(filenames)) {
      fuel <- writeRaster(
        x = fuel,
        filename = filenames[1],
        overwrite = TRUE,
        datatype = dtfuel
      )
    }
  }

  # Clip and resample elevation grid
  if (!missing(elev)) {
    elev <- crop(
      x = elev,
      y = clipExt,
      snap = "near"
    )

    if (any(res(elev) != res(fuel))) {
      elev <- project(
        x = elev,
        y = paste0("EPSG:", crs(elev, describe = TRUE)$code),
        method = "bilinear",
        res = cellsize
      )
    }

    if (!missing(filenames)) {
      elev <- writeRaster(
        x = elev,
        filename = ifelse(missing(fuel), filenames[1], filenames[2]),
        overwrite = TRUE,
        datatype = dtelev
      )
    }
  }

  if (missing(elev)) {
    names(fuel) <- "fuel"
    return(fuel)
  } else if (missing(fuel)) {
    names(elev) <- "elev"
    return(elev)
  } else {
    clip <- rast(list(fuel, elev))
    names(clip) <- c("fuel", "elev")
    return(clip)
  }
}

# Get point, line, or polygon geometry types from a Web Feature Service (WFS)
# url = character string specifying the scheme, hostname, and path for the WFS.
# crs = reprojection Coordinate Reference System for the GetFeature request. Defaults to NAD83 / BC Albers.
# bbox = search for features contained (or partially contained) inside a string of user-defined coordinates "a1,b1,a2,b2".
# fexp = filter expression...
# The order of x and y coordinates depends on the crs used.
# pag = number of features to iteratively return using pagination.

getWFS <- function(url, crs = "EPSG:3005", bbox = NULL, fexp = NULL, pag = 1000) {
  # Get the namespace:featuretype portion of the url string
  featuretype <- WFSClient$new(url, serviceVersion = "2.0.0")
  featuretype <- featuretype$getFeatureTypes(pretty = TRUE)$name[1]

  # Build URL to query the number of geometeries in the WFS request
  n_features <- url %>%
    parse_url() %>%
    list_merge(query = list(
      service = "WFS",
      version = "2.0.0",
      request = "GetFeature",
      typename = featuretype,
      srsName = crs,
      bbox = bbox,
      resultType = "hits"
    )) %>%
    build_url() %>%
    GET() %>%
    content(as = "parsed") %>%
    xml2::as_list() %>%
    .$FeatureCollection %>%
    attr(which = "numberMatched", exact = FALSE) %>%
    as.numeric()


  getFeatureIndex <- function(x = 0) {
    request <- parse_url(url)

    # Add parameters to the url$query slot to instruct the WFS what information it should return
    request$query <- list(
      service = "WFS",
      version = "2.0.0",
      request = "GetFeature",
      typename = featuretype,
      srsName = crs,
      bbox = bbox,
      filter = fexp,
      startIndex = x,
      count = pag
    )

    # Build URL from the list object
    request <- build_url(request)

    # Read WFS layer
    request <- read_sf(request, crs = crs)
    return(request)
  }


  # Use startIndex and count to iteratively return all features using pagination
  features <- map(
    .x = seq(0, n_features, pag),
    .f = getFeatureIndex,
    .progress = TRUE
  )
  features <- do.call(what = rbind, args = features)

  # Convert SF object to SpatVector
  features <- vect(x = features)

  return(features)
}

# Convert to data frame safely
safe_to_df <- function(spat_obj) {
  if (is.data.frame(spat_obj)) {
    return(spat_obj)
  }
  if (inherits(spat_obj, "SpatVector")) {
    return(as.data.frame(spat_obj, geom = NULL))
  }
  return(as.data.frame(spat_obj))
}

# Check data frame for leaflet compatibility
check_leaflet_ready <- function(df, name) {
  cls <- class(df)
  n <- if (is.data.frame(df)) nrow(df) else NA
  list_cols <- names(df)[vapply(df, is.list, logical(1))]
  posixlt_cols <- names(df)[vapply(df, function(x) inherits(x, "POSIXlt"), logical(1))]
  lon_ok <- "LONGITUDE" %in% names(df) && is.numeric(df$LONGITUDE)
  lat_ok <- "LATITUDE" %in% names(df) && is.numeric(df$LATITUDE)
  cat(sprintf(
    "[%s] class=%s rows=%s list-cols=%s posixlt=%s LONGITUDE_numeric=%s LATITUDE_numeric=%s\n",
    name, paste(cls, collapse = "/"), n,
    if (length(list_cols)) paste(list_cols, collapse = ", ") else "none",
    if (length(posixlt_cols)) paste(posixlt_cols, collapse = ", ") else "none",
    lon_ok, lat_ok
  ))
  invisible(list(list_cols = list_cols, posixlt_cols = posixlt_cols, lon_ok = lon_ok, lat_ok = lat_ok))
}

# Clean data frame for leaflet use
clean_for_leaflet <- function(df, name = "") {
  # If sf, extract coords first (ensure WGS84)
  if (inherits(df, "sf")) {
    df <- tryCatch(st_transform(df, 4326), error = function(e) df)
    coords <- tryCatch(st_coordinates(df), error = function(e) NULL)
    if (!is.null(coords)) {
      df <- as.data.frame(df)
      df$LONGITUDE <- coords[, 1]
      df$LATITUDE <- coords[, 2]
    } else {
      df <- as.data.frame(df)
    }
  } else {
    df <- as.data.frame(df)
  }

  # Drop POSIXlt (list-like), other list columns, geometry
  drop_cols <- names(df)[vapply(df, function(x) is.list(x) || inherits(x, "POSIXlt"), logical(1))]
  drop_cols <- union(drop_cols, intersect(names(df), c("geometry", "geom")))
  if (length(drop_cols)) {
    message("Dropping list-like columns for ", name, ": ", paste(drop_cols, collapse = ", "))
    df <- df %>% select(-all_of(drop_cols))
  }

  # Coerce lon/lat and key popup fields to simple atomic types
  if ("LONGITUDE" %in% names(df)) df$LONGITUDE <- as.numeric(df$LONGITUDE)
  if ("LATITUDE" %in% names(df)) df$LATITUDE <- as.numeric(df$LATITUDE)
  for (cname in c("FIRE_NUMBER", "FIRE_STATUS", "GEOGRAPHIC_DESCRIPTION", "CURRENT_SIZE")) {
    if (cname %in% names(df)) df[[cname]] <- as.character(df[[cname]])
  }

  check_leaflet_ready(df, name)
  df
}
