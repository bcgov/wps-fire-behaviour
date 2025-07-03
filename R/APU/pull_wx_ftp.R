#####################################################################
#####################################################################
## author: Lochlan Noble
## initialization date: 2025.06.24
## purpose: pull weather hourly wx from BCWS DataMart directory and filter daily observations
## notes: can specify forecast or actual for fwi (ffmc, isi, FWI). 
##        default settings are to use yesterday's indices with actual fwi.
## outputs: see folder ""
## updated: 
## TODO: - add output to handle NA data (display station name for unavailable data)
##       - add precipitation handling (sum daily precipitation) - daily from 00:00 and 24hr
##       - add description for inputs and outputs
##       - add more examples
######################################################################
## Variables:
## base_url: Base URL for the BCWS DataMart directory (default is set to BCWS DataMart)
## use_date: Date for which to pull the weather data (default is yesterday's date)
## use_fwi_actual: Boolean to specify whether to use actual FWI values (default is TRUE)
## save_to_file: Boolean to specify whether to save the data to a file (default is FALSE)
## save_path: Path to save the file if save_to_file is TRUE (default is current working directory)
#####################################################################
#####################################################################

pull_wx_ftp <- function(base_url, use_date, use_fwi_actual, save_to_file, save_path) {
  # Check if base_url is provided, use default if not
  if (missing(base_url)) {
    base_url <- "https://www.for.gov.bc.ca/ftp/HPR/external/!publish/BCWS_DATA_MART/"
  }

  ##DATE HANDLING
  # If use_date isn't specified, set date to yesterday's date
  if (missing(use_date)) {
    use_date <- Sys.Date() - 1
  }

  # Check if use_date is today's date, if so, make sure the time is after 14:00 PST
  if (use_date == Sys.Date()) {
    current_time <- Sys.time()
    if (format(current_time, "%H:%M") < "14:00") {
      stop("Cannot pull today's data before 14:00 PST.")
    }
    if (format(current_time, "%H:%M") < "17:00") {
      use_fwi_actual <- FALSE  # Use forecast values if before 17:00
      message("Pulling today's forecast fwi values from 12:00 LST.", "\n",
      "Actual values will be used after 17:00 LST.")
    }
  }

  # Convert use_date character to %y-%m-%d format
  use_date <- as.Date(use_date, format = "%Y-%m-%d")
  year <- format(use_date, "%Y")

  ##SCRAPE DATA
  file_url <- paste0(base_url, year, "/", use_date, ".csv")

  # Check if the file exists at the constructed URL
  if (!httr::http_error(file_url)) {
    message("File found: ", file_url)
  } else {
    stop("File not found at URL: ", file_url,
         "\nPlease ensure that the base URL and date are correct.")
  }

  # Read CSV
  df <- read.csv(file_url)

  
  # Summary of the data frame
  message("Data frame loaded with ", nrow(df), " rows and ", ncol(df), " columns.")
  
  # Convert DATE_TIME (assumes format YYYYMMDDHH, e.g., 2024010100) to POSIXct
  if ("DATE_TIME" %in% names(df)) {
    df$DATE_TIME <- as.POSIXct(as.character(df$DATE_TIME), format = "%Y%m%d%H", tz = "")

  # Add hourly column
    df$HOUR <- as.integer(format(df$DATE_TIME, "%H"))
  } else {
    stop("DATE_TIME column not found in file: ", file_url)
  }

  # Filter data
  if (missing(use_fwi_actual)) {
    use_fwi_actual <- TRUE  # Default to using actual fwi
  }
  # Columns of interest
  cols <- c("STATION_NAME", "STATION_CODE", "DATE_TIME", "HOUR", "HOURLY_TEMPERATURE",
            "HOURLY_RELATIVE_HUMIDITY", "HOURLY_WIND_SPEED",
            "HOURLY_WIND_DIRECTION", "HOURLY_PRECIPITATION", "HOURLY_FINE_FUEL_MOISTURE_CODE",
            "HOURLY_INITIAL_SPREAD_INDEX", "HOURLY_FIRE_WEATHER_INDEX",
            "PRECIPITATION", "FINE_FUEL_MOISTURE_CODE",
            "INITIAL_SPREAD_INDEX", "FIRE_WEATHER_INDEX", "DUFF_MOISTURE_CODE",
            "DROUGHT_CODE", "BUILDUP_INDEX")

  if (use_fwi_actual == FALSE) {
    # Filter hour 12, use forecast values for all indices
    df <- df[df$HOUR == 12, ]
    df <- df[cols]

  } else {
    ##########################################
    # Calculate actual daily precipitation
    ##########################################

    # Filter hour 12 and 17
    df <- df[df$HOUR %in% c(12, 17), ]
    # Print summary of the filtered data
    message("Filtered data for hours 12 and 17.")

    # copy dmc, dc, bui values from 12:00 to 17:00
    for (item in c("DUFF_MOISTURE_CODE", "DROUGHT_CODE", "BUILDUP_INDEX")) {
      if (item %in% names(df)) {
        df[[item]][df$HOUR == 17] <- df[[item]][df$HOUR == 12]
      } else {
        warning(paste("Column", item, "not found in the data frame. Skipping copy for this column."))
      }
    }

    # Trim df to only include the desired columns
    df <- df[df$HOUR == 17, ]
    df <- df[cols]

    # copy hourly ffmc, isi, fwi values from 17:00 to daily values
    for (item in c("FINE_FUEL_MOISTURE_CODE",
                   "INITIAL_SPREAD_INDEX", "FIRE_WEATHER_INDEX")) {
      if (item %in% names(df)) {
        #df[[paste0("HOURLY_", item)]][df$HOUR == 17] <- df[[item]][df$HOUR == 17]
        df[[item]] <- df[[paste0("HOURLY_", item)]]
      } else {
        warning(paste("Column", item, "not found in the data frame. Skipping copy for this column."))
      }
    }
  }

  ##SAVE DATA
  if (missing(save_to_file)) {
    save_to_file <- FALSE  # Default to not saving to file
  }
  if (save_to_file == TRUE){
    # Check if save_path is provided, use default if not
    if (missing(save_path)) {
      save_path <- getwd()  # Default to current working directory
    }

    # Construct the file name
    file_name <- paste0("BCWS_WX_OBS_", format(use_date, "%Y%m%d"), ".csv")
    full_file_path <- file.path(save_path, file_name)

    # Write the data frame to a CSV file
    data.table::fwrite(df, full_file_path, row.names = FALSE)
    message("Data saved to: ", full_file_path)
  } else {
    message("Data not saved to file.")
  }
  return(df)
}