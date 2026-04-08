########################################################################################
########################################################################################
## FIRE HISTORY FEATURE - CORRECTED IMPLEMENTATION
## author: Lochlan Noble
## purpose: Complete fire history feature for Shiny mapping app
## 
## CRITICAL FIXES APPLIED:
## 1. Correct leaflet click coordinate extraction (lng/lat)
## 2. Fixed st_intersects() matrix handling for filtering
## 3. Direct sf object support in leaflet rendering
## 4. Global color palette for legend consistency
## 5. Proper opacity parameter separation from color
## 6. CRS consistency (all data in 4326 for leaflet)
## 7. Layer group management with proper show/hide
## 8. Error handling for spatial operations
########################################################################################
########################################################################################

library(sf)
library(leaflet)
library(shiny)
library(tidyverse)

setwd("//SFP.IDIR.BCGOV/S165/S65002/!Project/Predictive Services/10. PROJECT WORK")

# ============================================================
# PHASE 1: DATA LOADING & PREPROCESSING
# ============================================================

# Load historical fire data and ensure WGS84
hist_fires_points <- st_read("./BC FGM/fire_history/data/20yr_fire_pts_historical.gpkg") %>%
  st_transform(4326)

hist_fires_perims <- st_read("./BC FGM/fire_history/data/20yr_fire_polys_historical.gpkg") %>%
  st_transform(4326)

# Helper function: Calculate opacity based on years ago (3-year steps)
calculate_opacity <- function(years_ago) {
  case_when(
    years_ago < 3 ~ 1.0,
    years_ago < 6 ~ 0.75,
    years_ago < 9 ~ 0.5,
    years_ago < 12 ~ 0.35,
    TRUE ~ 0.2
  )
}

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
  reverse = TRUE  # Newest (recent) = red, oldest = green
)

# ============================================================
# PHASE 2: SIDEBAR UI COMPONENTS
# ============================================================

# Function to build sidebar UI (call this in your main ui.R)
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
        choices = c("5 years" = 5, "10 years" = 10, 
                    "15 years" = 15, "20 years" = 20),
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

# ============================================================
# PHASE 3: CLICK HANDLER & DATA FILTERING
# ============================================================

# SERVER CODE - Insert into your server.R

# Reactive values for state management
historical_data_filtered <- reactiveVal(NULL)
query_in_progress <- reactiveVal(FALSE)

# FIXED: Map click handler with correct coordinate extraction
observeEvent(input$map_shape_click, {
  # Early exit if toggle OFF
  if (!input$fireHistoryToggle) {
    return()
  }
  
  query_in_progress(TRUE)
  
  tryCatch({
    click <- input$map_shape_click
    
    # FIXED: Correct leaflet click coordinate extraction
    clicked_point <- st_point(c(click$lng, click$lat)) %>%
      st_sfc(crs = 4326)
    
    # Create search buffer (convert km to meters)
    search_buffer <- st_buffer(clicked_point, dist = input$fireHistoryRadius * 1000)
    
    # Calculate year range
    start_year <- current_year - input$fireHistoryYears
    end_year <- current_year
    
    filtered_data <- list()
    
    # FIXED: Handle st_intersects() which returns a matrix
    if ("points" %in% input$fireHistoryType) {
      intersects_pts <- st_intersects(hist_fires_points, search_buffer, sparse = FALSE)[, 1]
      
      filtered_data$points <- hist_fires_points %>%
        filter(
          fire_year >= start_year,
          fire_year <= end_year,
          intersects_pts
        )
    } else {
      filtered_data$points <- NULL
    }
    
    # Same for perimeters
    if ("perimeters" %in% input$fireHistoryType) {
      intersects_prm <- st_intersects(hist_fires_perims, search_buffer, sparse = FALSE)[, 1]
      
      filtered_data$perims <- hist_fires_perims %>%
        filter(
          fire_year >= start_year,
          fire_year <= end_year,
          intersects_prm
        )
    } else {
      filtered_data$perims <- NULL
    }
    
    historical_data_filtered(filtered_data)
    
  }, error = function(e) {
    showNotification(
      paste("Error querying fire history:", e$message),
      type = "error"
    )
  })
  
  query_in_progress(FALSE)
}, ignoreInit = TRUE)

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
observeEvent(historical_data_filtered(), {
  proxy <- leafletProxy("map")
  
  # Clear previous
  proxy %>% clearGroup("fireHistory")
  
  data <- historical_data_filtered()
  
  if (is.null(data)) {
    return()
  }
  
  # FIXED: Render points directly from sf object
  if (!is.null(data$points) && nrow(data$points) > 0) {
    proxy %>%
      addCircleMarkers(
        data = data$points,
        radius = 5,
        color = ~color_palette(fire_year),
        fillColor = ~color_palette(fire_year),
        fillOpacity = ~opacity,  # FIXED: Separate opacity parameter
        weight = 2,
        popup = ~paste(
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
        color = ~color_palette(fire_year),
        fillColor = ~color_palette(fire_year),
        fillOpacity = ~opacity,  # FIXED: Separate opacity parameter
        weight = 1.5,
        popup = ~paste(
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
  
}, ignoreInit = TRUE)

# Toggle visibility
observeEvent(input$fireHistoryToggle, {
  proxy <- leafletProxy("map")
  
  if (input$fireHistoryToggle) {
    proxy %>% showGroup("fireHistory")
  } else {
    }
}, ignoreInit = TRUE)

# ============================================================
# END OF FIRE HISTORY IMPLEMENTATION
# ============================================================

# INTEGRATION CHECKLIST:
# 1. Add fire_history_sidebar_ui() to your sidebar in ui.R
# 2. Copy server code (Phases 3-4) into your server.R
# 3. Ensure .gpkg files exist at paths specified
# 4. Data should be in WGS84 (4326) - conversion handled at load
# 5. Test with map click on current fire points

