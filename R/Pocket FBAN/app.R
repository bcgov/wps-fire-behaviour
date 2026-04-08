########################################################################################
########################################################################################
## author: Lochlan Noble
## initialization date: 2025.11.11
## purpose: Shiny app to clip fuel grids and display summary stats - test ground for HF
## notes:
## outputs: see folder ""
## updated:
## changelog:
##
## TODO: - Upload imported functions to GitHub -> single source location
##       - Add display of clipped grids on leaflet map
##       - Add summary stats display of clipped grids
########################################################################################
########################################################################################



## SETUP
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(sp)
library(xml2)
library(terra)

setwd("//SFP.IDIR.BCGOV/S165/S65002/!Project/Predictive Services/10. PROJECT WORK")

source("spatial_hfi/functions/getWFS.R")
source("BC FGM/R_code/clipGrids_app.R")

## LOAD FUEL AND ELEVATION LAYERS
# year <- as.integer(format(Sys.Date(), "%Y"))
year <- 2025
cellsize <- 50 # default for model runs

# fuel
fuel <- rast(
  x = paste0("./BC FGM/fuels/archive/FM_FUEL_TYPE_GRID_BC_", year, "_", rep(0, times = 3 - nchar(cellsize)), cellsize, "m.tif")
)
crs(fuel) <- "epsg:3005"

# LUT
lut <- read.csv("./BC FGM/fuels/FM_FUEL_TYPE_LUT_2025_BC_PROMETHEUS.csv")
names(lut) <- tolower(names(lut))

# # Constrain lookup table to clipped raster values
# lut <- lut[which(lut$grid_value %in% unique(clip$fuel)[,1]),]
# lut <- lut[order(lut$grid_value),]

# # Create color values for fuel types
# lut$col <- rgb(r=lut$r, g=lut$g, b=lut$b, maxColorValue=255)

# write.csv(lut, file=paste0("./CLIPPED GRIDS/",fnumber,"_fbp_fuel_type_lookup_table.csv"), row.names=FALSE)

# elevation
elev <- rast(x = "./BC FGM/dem/bc_dem_50m.tif")

hist_fires_points <- st_read("./BC FGM/fire_history/data/20yr_fire_pts_historical.gpkg") %>%
  st_transform(4326)

hist_fires_perims <- st_read("./BC FGM/fire_history/data/20yr_fire_polys_historical.gpkg") %>%
  st_transform(4326)


calculate_opacity <- function(years_ago) {
  case_when(
    years_ago < 3 ~ 1.0,
    years_ago < 6 ~ 0.75,
    years_ago < 9 ~ 0.5,
    years_ago < 12 ~ 0.35,
    TRUE ~ 0.2
  )
} ####### RERWRITE

# Get current year once for consistency
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Preprocess both datasets
hist_fires_points <- hist_fires_points %>%
  mutate(
    fire_year = FIRE_YEAR,
    years_ago = current_year - fire_year,
    opacity = calculate_opacity(years_ago)
  )

hist_fires_perims <- hist_fires_perims %>%
  mutate(
    fire_year = FIRE_YEAR,
    years_ago = current_year - fire_year,
    opacity = calculate_opacity(years_ago)
  )

# FIXED: Create global color palette for consistency
color_palette <- colorNumeric(
  palette = "RdYlGn",
  domain = c(
    min(hist_fires_points$fire_year, hist_fires_perims$fire_year),
    max(hist_fires_points$fire_year, hist_fires_perims$fire_year)
  ),
  reverse = TRUE # Newest (recent) = red, oldest = green
)

## LOAD WILDFIRE DATA FROM WFS
# Get current year fire points from BC Geowarehouse
filterExpression <- paste0(
  "<Filter>",
  "<PropertyIsEqualTo>",
  "<PropertyName>FIRE_TYPE</PropertyName>",
  "<Literal>Fire</Literal>",
  "</PropertyIsEqualTo>",
  "</Filter>"
)

firePt_cur_raw <- getWFS(
  url = "https://openmaps.gov.bc.ca/geo/pub/WHSE_LAND_AND_NATURAL_RESOURCE.PROT_CURRENT_FIRE_PNTS_SP/ows",
  fexp = filterExpression
)

firePoly_cur_raw <- getWFS(
  url = "https://openmaps.gov.bc.ca/geo/pub/WHSE_LAND_AND_NATURAL_RESOURCE.PROT_CURRENT_FIRE_POLYS_SP/ows",
  # fexp = filterExpression
)

# Get fire centre boundaries
fcentre <- getWFS(
  url = "https://openmaps.gov.bc.ca/geo/pub/WHSE_LEGAL_ADMIN_BOUNDARIES.DRP_MOF_FIRE_CENTRES_SP/ows"
)

# FUTURE -> Burn severity



########################################################################################
## FUNCTIONS TO PREPARE DATA FOR LEAFLET MAPPING
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

# Prepare current fires data
fires_current <- firePt_cur_raw %>% safe_to_df()
check_leaflet_ready(fires_current, "fires_current (raw)")

fires_current <- clean_for_leaflet(fires_current, "fires_current (clean)")
fires_current <- fires_current %>%
  mutate(
    INCIDENT_NAME = ifelse(is.na(INCIDENT_NAME), "", INCIDENT_NAME),
    CURRENT_SIZE = ifelse(is.na(CURRENT_SIZE), "", as.character(CURRENT_SIZE)),
    IGNITION_DATE = ifelse(is.na(IGNITION_DATE), "", as.character(IGNITION_DATE)),
    FIRE_URL = ifelse(is.na(FIRE_URL), "", FIRE_URL),
    popup_html = paste0(
      "<b>Fire #: </b>", FIRE_NUMBER, "<br/>",
      "<b>Name: </b>", INCIDENT_NAME, "<br/>",
      "<b>Status: </b>", FIRE_STATUS, "<br/>",
      "<b>Size (ha): </b>", CURRENT_SIZE, "<br/>",
      "<b>Ignition: </b>", IGNITION_DATE, "<br/>"
    )
  )

OC <- fires_current %>% filter(FIRE_STATUS == "Out of Control")
BH <- fires_current %>% filter(FIRE_STATUS == "Being Held")
UC <- fires_current %>% filter(FIRE_STATUS == "Under Control")
OUT <- fires_current %>% filter(FIRE_STATUS == "Out")


fire_history_sidebar_ui <- function() {
  tagList(
    h4("Fire History"),

    # Master toggle
    checkboxInput(
      "fireHistoryToggle",
      "Enable Fire History",
      value = FALSE
    ),
    # Controls (hidden unless toggle ON)
    conditionalPanel(
      condition = "input.fireHistoryToggle == true",
      # Radius slider
      sliderInput(
        "fireHistoryRadius",
        "Search Radius (km):",
        min = 5, max = 20, value = 10, step = 1
      ),
      # Year buttons (EXCLUSIVE radio buttons)
      radioButtons(
        "fireHistoryYears",
        "Years of History:",
        choices = c(
          "5 years" = 5, "10 years" = 10,
          "15 years" = 15, "20 years" = 20
        ),
        selected = 10,
        inline = TRUE
      ),
      # Point/Perimeter toggles (INCLUSIVE checkboxes)
      checkboxGroupInput(
        "fireHistoryType",
        "Display:",
        choices = c("Points" = "points", "Perimeters" = "perimeters"),
        selected = c("points", "perimeters"),
        inline = TRUE
      ),
      # Progress indicator
      uiOutput("fireHistoryProgress")
    )
  )
}


## UI
ui <- fluidPage(
  titlePanel("Map Test"),
  sidebarLayout(
    sidebarPanel(
      fire_history_sidebar_ui(),
      helpText("Current Wildfires in BC from WFS"),
      helpText("Data source: BC Wildfire Service Open Maps WFS"),
      textInput("fnumber", "Fire Number", value = "", placeholder = "Click on a fire marker to see fire number"),
      numericInput("fuel_year", "Fuel grid year", value = as.integer(Sys.Date() %>% format("%Y")), min = 1900, max = 2100),
      numericInput("lat", "Latitude", value = NA_real_, step = 0.0001),
      numericInput("lon", "Longitude", value = NA_real_, step = 0.0001),
      numericInput("width", "Width (m)", value = 10000, min = 1),
      numericInput("height", "Height (m)", value = 10000, min = 1),
      numericInput("cellsize", "Cell size (m)", value = 50, min = 50, step = 50),
      actionButton("run_clip", "Run clipGrids"),
      # downloadButton("download_clip", "Download Clipped Grid"),
      hr(),
      verbatimTextOutput("clip_result")
    ),
    mainPanel(leafletOutput("map"))
  )
)

server <- function(input, output, session) {
  # reactive value to store clip result
  clipped_data <- reactiveValues(clip = NULL, fnumber = NULL)

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      setView(lng = -123, lat = 54, zoom = 4.5) %>%
      addCircleMarkers(
        data = OC, lng = ~LONGITUDE, lat = ~LATITUDE, radius = 1, color = "red", fillOpacity = 1,
        popup = ~popup_html, group = "Out of Control", layerId = ~FIRE_NUMBER
      ) %>%
      addCircleMarkers(
        data = BH, lng = ~LONGITUDE, lat = ~LATITUDE, radius = 1, color = "yellow", fillOpacity = 1,
        popup = ~popup_html, group = "Being Held", layerId = ~FIRE_NUMBER
      ) %>%
      addCircleMarkers(
        data = UC, lng = ~LONGITUDE, lat = ~LATITUDE, radius = 1, color = "green", fillOpacity = 1,
        popup = ~popup_html, group = "Under Control", layerId = ~FIRE_NUMBER
      ) %>%
      addCircleMarkers(
        data = OUT, lng = ~LONGITUDE, lat = ~LATITUDE, radius = 1, color = "grey", fillOpacity = 1,
        popup = ~popup_html, group = "Out", layerId = ~FIRE_NUMBER
      ) %>%
      addLayersControl(
        overlayGroups = c("Out of Control", "Being Held", "Under Control", "Out"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # debug observe
  observe({
    click <- input$map_marker_click
    if (!is.null(click)) {
      print(click)
    }
  })

  # Observe click on map marker
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (is.null(click$id) || is.null(click$id)) {
      return()
    }
    selected <- fires_current %>%
      filter(FIRE_NUMBER == click$id) %>%
      slice(1)
    if (nrow(selected) == 0) {
      return()
    }
    updateTextInput(session, "fnumber", value = selected$FIRE_NUMBER)
    updateNumericInput(session, "fuel_year", value = as.integer(selected$FIRE_YEAR))
    updateNumericInput(session, "lat", value = selected$LATITUDE)
    updateNumericInput(session, "lon", value = selected$LONGITUDE)
  })

  # Run clipGrids on button click
  observeEvent(input$run_clip, {
    req(input$fnumber, input$fuel_year, input$lat, input$lon, input$width, input$height, input$cellsize)

    showNotification("Clipping grids, please wait...", duration = NULL, id = "clip_run")
    tryCatch(
      {
        clip_result <- clipGrids(
          fuel = fuel,
          elev = elev,
          latlon = c(input$lat, input$lon),
          width = input$width,
          height = input$height,
          cellsize = input$cellsize,
          filenames = paste0(
            "./BC FGM/fuels/CLIPPED GRIDS/", input$fnumber,
            c(paste0("_fbp_fuel_type_", year, "_"), "_elevation_"), "50m_clip.tif"
          ) # This needs debug checks
        )

        # store result
        clipped_data$clip <- clip_result
        clipped_data$fnumber <- input$fnumber

        removeNotification(id = "clip_run")
        showNotification(paste0("Grid completed for ", clipped_data$fnumber, "\n", "Downloaded to CLIPPED GRIDS folder"),
          type = "message", duration = 5
        )

        # clip LUT to clipped raster values
        lut <- lut[which(lut$grid_value %in% unique(clip_result$fuel)[, 1]), ]
        lut <- lut[order(lut$grid_value), ]

        # Create color values for fuel types
        lut$col <- rgb(r = lut$r, g = lut$g, b = lut$b, maxColorValue = 255)

        # write.csv(lut, file=paste0("./CLIPPED GRIDS/",fnumber,"_fbp_fuel_type_lookup_table.csv"), row.names=FALSE)

        # add clipped raster to map
        leafletProxy("map") %>%
          addRasterImage(
            clipped_data$clip$fuel,
            colors = lut$col,
            opacity = 0.7,
            group = "Clipped Fuel Grid"
          ) %>%
          addLayersControl(
            overlayGroups = c("Out of Control", "Being Held", "Under Control", "Out", "Clipped Fuel Grid"),
            options = layersControlOptions(collapsed = FALSE)
          )

        output$clip_result <- renderPrint({
          if (!is.null(clipped_data$clip)) {
            cat("Clipped raster stack:\n")
            print(clipped_data$clip)
          }
        })
      },
      error = function(e) {
        removeNotification(id = "clip_run")
        showNotification(paste0("Error during clipping: ", e$message), type = "error", duration = NULL)
        output$clip_result <- renderPrint(cat("Error. See notifications for details"))
        print(e)
      }
    )
    # output$download_clip <- downloadHandler()
  })
  # Reactive values for state management
  historical_data_filtered <- reactiveVal(NULL)
  query_in_progress <- reactiveVal(FALSE)

  # FIXED: Map click handler with correct coordinate extraction
  # NOTE: CircleMarkers trigger map_marker_click, NOT map_shape_click
  observeEvent(input$map_marker_click,
    {
      # Early exit if toggle OFF
      if (!input$fireHistoryToggle) {
        cat("[Fire History] Toggle is OFF, exiting\n")
        return()
      }

      cat("[Fire History] Click detected, starting query...\n")
      query_in_progress(TRUE)

      tryCatch(
        {
          click <- input$map_marker_click
          cat("[Fire History] Click object:", click$lng, ",", click$lat, "\n")

          # FIXED: Create point in WGS84
          clicked_point <- st_point(c(click$lng, click$lat)) %>%
            st_sfc(crs = 4326)

          # CRITICAL FIX: Project to BC Albers (3005) BEFORE buffering in meters
          clicked_point_proj <- st_transform(clicked_point, 3005)
          cat("[Fire History] Projected to EPSG:3005\n")

          # Create search buffer in meters (now works correctly)
          search_buffer <- st_buffer(clicked_point_proj, dist = input$fireHistoryRadius * 1000)
          cat("[Fire History] Buffer created. Radius:", input$fireHistoryRadius, "km\n")

          # Transform back to 4326 for intersection with geographic data
          search_buffer <- st_transform(search_buffer, 4326)
          cat("[Fire History] Buffer transformed to EPSG:4326\n")

          # DEBUG: Check input values before year calculation
          cat("[Fire History DEBUG] current_year:", current_year, "\n")
          cat("[Fire History DEBUG] input$fireHistoryYears:", input$fireHistoryYears, "\n")
          cat("[Fire History DEBUG] input$fireHistoryYears class:", class(input$fireHistoryYears), "\n")

          # Calculate year range
          start_year <- current_year - as.numeric(input$fireHistoryYears)
          end_year <- current_year
          cat("[Fire History] Filtering years", start_year, "-", end_year, "\n")

          filtered_data <- list()

          # FIXED: Handle st_intersects() which returns a matrix
          if ("points" %in% input$fireHistoryType) {
            cat("[Fire History] Querying points... Total available:", nrow(hist_fires_points), "\n")
            cat("[Fire History DEBUG] fire_year column type:", class(hist_fires_points$fire_year), "\n")
            cat("[Fire History DEBUG] start_year:", start_year, "type:", class(start_year), "\n")
            cat("[Fire History DEBUG] end_year:", end_year, "type:", class(end_year), "\n")

            intersects_pts <- st_intersects(hist_fires_points, search_buffer, sparse = FALSE)[, 1]
            cat("[Fire History DEBUG] intersects_pts length:", length(intersects_pts), "\n")

            # Convert fire_year to numeric to be safe
            hist_fires_points <- hist_fires_points %>%
              mutate(fire_year = as.numeric(fire_year))

            filtered_data$points <- hist_fires_points %>%
              filter(
                fire_year >= start_year,
                fire_year <= end_year,
                intersects_pts
              )
            cat("[Fire History] Points found:", nrow(filtered_data$points), "\n")
          } else {
            filtered_data$points <- NULL
            cat("[Fire History] Points disabled by user\n")
          }

          # Same for perimeters
          if ("perimeters" %in% input$fireHistoryType) {
            cat("[Fire History] Querying perimeters... Total available:", nrow(hist_fires_perims), "\n")
            cat("[Fire History DEBUG] fire_year column type:", class(hist_fires_perims$fire_year), "\n")

            intersects_prm <- st_intersects(hist_fires_perims, search_buffer, sparse = FALSE)[, 1]
            cat("[Fire History DEBUG] intersects_prm length:", length(intersects_prm), "\n")

            # Convert fire_year to numeric to be safe
            hist_fires_perims <- hist_fires_perims %>%
              mutate(fire_year = as.numeric(fire_year))

            filtered_data$perims <- hist_fires_perims %>%
              filter(
                fire_year >= start_year,
                fire_year <= end_year,
                intersects_prm
              )
            cat("[Fire History] Perimeters found:", nrow(filtered_data$perims), "\n")
          } else {
            filtered_data$perims <- NULL
            cat("[Fire History] Perimeters disabled by user\n")
          }

          historical_data_filtered(filtered_data)
        },
        error = function(e) {
          cat("[Fire History] ERROR:", e$message, "\n")
          showNotification(
            paste("Error querying fire history:", e$message),
            type = "error"
          )
        }
      )

      query_in_progress(FALSE)
      cat("[Fire History] Query complete\n")
    },
    ignoreInit = TRUE
  )

  # Progress indicator
  output$fireHistoryProgress <- renderUI({
    if (query_in_progress()) {
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #f0f0f0; border-radius: 4px;",
        tags$i(class = "fa fa-spinner fa-spin"),
        " Loading historical data..."
      )
    }
  })

  # ============================================================
  # PHASE 4: LAYER RENDERING
  # ============================================================

  # Render fire history layers on map
  observeEvent(historical_data_filtered(),
    {
      cat("[Fire History Render] Rendering triggered\n")
      proxy <- leafletProxy("map")

      # Clear previous
      proxy %>% clearGroup("fireHistory")

      data <- historical_data_filtered()

      if (is.null(data)) {
        cat("[Fire History Render] Data is NULL, exiting\n")
        return()
      }

      cat("[Fire History Render] Points to render:", if (!is.null(data$points)) nrow(data$points) else 0, "\n")
      cat("[Fire History Render] Perimeters to render:", if (!is.null(data$perims)) nrow(data$perims) else 0, "\n")

      # FIXED: Render points directly from sf object
      if (!is.null(data$points) && nrow(data$points) > 0) {
        proxy %>%
          addCircleMarkers(
            data = data$points,
            radius = 5,
            color = ~ color_palette(fire_year),
            fillColor = ~ color_palette(fire_year),
            fillOpacity = ~opacity, # FIXED: Separate opacity parameter
            weight = 2,
            popup = ~ paste(
              "Year:", fire_year,
              "<br/>Years ago:", years_ago,
              "<br/>Opacity:", round(opacity, 2)
            ),
            group = "fireHistory"
          )
      }

      # FIXED: Render perimeters directly from sf object
      if (!is.null(data$perims) && nrow(data$perims) > 0) {
        proxy %>%
          addPolygons(
            data = data$perims,
            color = ~ color_palette(fire_year),
            fillColor = ~ color_palette(fire_year),
            fillOpacity = ~opacity, # FIXED: Separate opacity parameter
            weight = 1.5,
            popup = ~ paste(
              "Year:", fire_year,
              "<br/>Years ago:", years_ago,
              "<br/>Opacity:", round(opacity, 2)
            ),
            group = "fireHistory"
          )
      }

      # FIXED: Legend now uses global palette for consistency
      proxy %>%
        addLegend(
          position = "topright",
          title = "Fire History by Year",
          pal = color_palette,
          values = seq(
            min(hist_fires_points$fire_year, hist_fires_perims$fire_year),
            max(hist_fires_points$fire_year, hist_fires_perims$fire_year)
          ),
          group = "fireHistory"
        )
    },
    ignoreInit = TRUE
  )

  # Toggle visibility
  observeEvent(input$fireHistoryToggle,
    {
      proxy <- leafletProxy("map")
      cat("[Fire History Toggle]", if (input$fireHistoryToggle) "ON" else "OFF", "\n")

      if (input$fireHistoryToggle) {
        proxy %>% showGroup("fireHistory")
      } else {
        proxy %>% hideGroup("fireHistory")
      }
    },
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)

# DEBUG

# clip_debug <- clipGrids(
#         fuel = fuel,
#         elev = elev,
#         latlon = c(57.4485, -123.1818),
#         width = 10000,
#         height = 10000,
#         cellsize = 50,
#         filenames = paste0("./BC FGM/fuels/CLIPPED GRIDS/G80352", c(paste0("_fbp_fuel_type_", year, "_"), "_elevation_"), "50m_clip.tif")
#       )
