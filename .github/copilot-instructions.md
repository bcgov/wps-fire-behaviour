# Copilot Instructions for wps-fire-behaviour

## Project Overview
This is a BC Government Predictive Services Unit (PSU) fire behaviour analysis codebase supporting three core workflows:

1. **APU (Advanced Planning Unit)** - Initial Attack (IA) success metrics and 10-day fire weather indices
2. **Model Comparison** - Validating fire growth models (Firecast, Technosylva) against observed fire perimeters
3. **Fire Exposure Analysis** - Calculating wildfire exposure using terrain, fuel, and fire behaviour data

## Architecture

### Data Flow
- **Input**: Fire perimeters (FIRMS, Sentinel-2, BCWS GDB), weather data (FTP, NWP APIs), fuel/elevation rasters
- **Processing**: Python/R scripts transform raw data to geospatial layers and metrics
- **Output**: GeoTIFF grids, shapefiles, spatial comparisons, skill score metrics

### Key Components
- `R/APU/` - Functions for weather index calculation and fire station data management (source from GitHub)
- `model_comparison/py/` - Fire model validation: `compare.py` (main entry), `model.py` (plotting), `shape_convert.py` (raster/vector conversion)
- `apu/py/IA_metrics/` - Initial Attack metrics including time-to-4ha calculations from SFMS Daily GDB
- Weather integration: Open-Meteo API, FTP data appending workflows

## Code Patterns & Conventions

### R Scripts
- **Structure**: Header comments with author, date, purpose, TODO; function includes dependency checks
- **Libraries**: terra (raster), tidyverse (data wrangling), cffdrs (fire calculations), fireexposuR (custom package)
- **Dependencies**: Remote sourcing common via `source("https://raw.githubusercontent.com/bcgov/wps-fire-behaviour/...")`
- **Example**: `calc_10day_indices()` in `R/APU/` includes automatic package installation checks

### Python Scripts
- **Mapping Focus**: Heavy use of geopandas, shapely for geospatial operations; matplotlib/cartopy for visualization
- **Data Pipelines**: comment-heavy with explicit input/output documentation and error handling
- **Fire Identification**: Fire numbers are alphanumeric (e.g., "K52125"), used as lookup keys across datasets
- **Conventions**:
  - `compare.py` uses command-line fire number argument: `python3 compare.py K52125`
  - Lookup tables in `../data/` directory (2023_fire_season_lookup_table.xlsx)
  - External model storage at `/mnt/e/MODELS/{FIRECAST,TECHNOSYLVA}/simulations/`

## Critical Workflows

### Fire Model Comparison
1. **Data Discovery**: `compare.py` → `lookup_technosylva()` matches fire name/number to simulation folders
2. **Perimeter Matching**: Selects observed fire perimeter closest to simulation end time
3. **Raster Conversion**: `shape_convert.py` → `shapefile_to_raster()` converts shapefiles to aligned grids
4. **Validation**: Confusion matrix via `confusion()` produces skill score metrics
5. **Visualization**: Plots with matplotlib show predicted vs. observed fire extent

**Key Assumption**: Technosylva simulations run ~12 hours; Firecast exactly 12 hours from ignition

### Exposure Batch Processing
1. **Setup**: Load fire metadata (number, coords, year) and elevation/fuel grids (50m resolution)
2. **Clipping**: `clipGrids.R` creates 30km buffers around fire locations
3. **Exposure Calc**: Custom `fireexposuR` package computes fire behaviour propagation
4. **Classification**: Apply hazard proportioning via `classify_proportional_hazard.R`

**Cross-Project Pattern**: `fire_exposure_batch.R` sources helper functions from local `./R_code/` and validates grid files before processing

## Key Dependencies & External Data

### Python Requirements
- Geospatial: geopandas, shapely, rasterio, fiona
- Analysis: pandas, numpy, scikit-learn
- Visualization: matplotlib, cartopy, seaborn
- Full list: See `apu/py/requirements.txt` and `requirements.txt` at workspace root

### R Libraries
- `terra` - raster processing (CRS handling via `crs(rast) <- "epsg:XXXX"`)
- `tidyverse` - data manipulation
- `cffdrs` - Canadian Fire Weather Index calculations
- `fireexposuR` - custom package (sourced from repo)

### External Data Sources
- BCWS Daily GDB: `//WildfireGeo/Geomatics$/GIS_Data/SFMS_Daily_Shapefiles/`
- Model outputs: `/mnt/e/MODELS/{FIRECAST,TECHNOSYLVA}/simulations/`
- Fire perimeters: Local `../data/` or BC Data Catalog
- Weather: Open-Meteo API with model appending logic

## Project-Specific Patterns

1. **Date-Based Naming**: Fuel grids use `FM_FUEL_TYPE_GRID_BC_{YEAR}_050m.tif`; SFMS files use `SFMSDaily_{YYYYMMDDHH}.gdb`
2. **Coordinate Systems**: Default is EPSG:3005 (BC Albers); data often converted from EPSG:4326 (lat/lon)
3. **Batch Processing**: Loops over fire lists with error handling (`file.exists()` checks, `next` statements)
4. **Fire Number Format**: 1 letter + 5 digits (N51800, V71498); used consistently across lookup tables
5. **TODO Comments**: Mark future work; common patterns: "clean up inputs", "identify correct models", "add type annotations"

## When Implementing Features

- **Geospatial ops**: Assume datum conversions, verify pixel alignment before raster operations
- **Fire data**: Validate fire numbers exist in lookup tables; some fires lack all three data sources
- **Weather integrations**: Model availability varies by region/date; appending logic essential for continuous forecasts
- **Error messages**: Include fire number/year context; reference the `fire_info.csv` pattern for metadata
- **Testing**: Use 2023 data for model comparison development; 2024 datasets may have missing fuel grids (use prior year)
