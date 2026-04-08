
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

# Update filter expression to get 10 years of historical fire points
filterExpression <- paste0(
  "<Filter>",
  "<PropertyIsEqualTo>",
  "<PropertyName>FIRE_TYPE</PropertyName>",
  "<Literal>Fire</Literal>",
  "</PropertyIsEqualTo>",
  "<PropertyIsGreaterThanOrEqualTo>",
  "<PropertyName>FIRE_YEAR</PropertyName>",
  paste0("<Literal>", as.integer(format(Sys.Date(), "%Y")) - 20, "</Literal>"),
  "</PropertyIsGreaterThanOrEqualTo>",
  "</Filter>"
)

firePt_hist_raw <- getWFS(
  url = "https://openmaps.gov.bc.ca/geo/pub/WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_INCIDENTS_SP/ows",
  fexp = filterExpression
)

firePr_hist_raw <- getWFS(
  url = "https://openmaps.gov.bc.ca/geo/pub/WHSE_LAND_AND_NATURAL_RESOURCE.PROT_HISTORICAL_FIRE_POLYS_SP/ows",
  fexp = filterExpression
)

# Get fire centre boundaries
fcentre <- getWFS(
  url = "https://openmaps.gov.bc.ca/geo/pub/WHSE_LEGAL_ADMIN_BOUNDARIES.DRP_MOF_FIRE_CENTRES_SP/ows"
)

# Save the historical data to file for faster loading
writeVector(firePt_hist_raw, "./BC FGM/fire_history/data/20yr_fire_pts_historical.gpkg", overwrite = TRUE)
writeVector(firePr_hist_raw, "./BC FGM/fire_history/data/20yr_fire_polys_historical.gpkg", overwrite = TRUE)
