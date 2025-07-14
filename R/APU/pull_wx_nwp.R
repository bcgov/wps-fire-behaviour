#####################################################################
#####################################################################
## author: Lochlan Noble
## initialization date: 2025.07.11
## purpose: pull NWP weather using openmeteo API 
## notes: requires daily wx data from BWCS to be compiled first for station locations
##        default model=null combines best available models for the location
##        model can be specified to use a specific NWP model (e.g., "gfs", "ecmwf", "icon")
##        see openmeteo documentation for more details on available models and parameters
##        https://open-meteo.com/en/docs
##        default forecast is 10 days, but can be adjusted
## outputs: see folder ""
## updated: 
## TODO: - 
##       - add description for inputs and outputs
##       - add more examples
######################################################################
## Variables:
## 
#####################################################################
#####################################################################

pull_wx_nwp <- function(wx_data, wx_file, model) {
    # Check if required packages are installed
    if (!requireNamespace("tidyverse", quietly = TRUE)) {
        install.packages("tidyverse")
    }
    if (!requireNamespace("openmeteo", quietly = TRUE)) {
        install.packages("openmeteo")
    }
   
    # Load necessary libraries
    library(tidyverse)
    library(openmeteo)

    working_directory <- getwd()

    # Check if wx_file exists
    if (missing(wx_data)) {
        # check if wx_file is provided
        if (missing(wx_file)) {
            stop("wx_file must be specified. Please provide the name of the weather station info file.")
        } else { 
            # check if wx_file exists in the working directory
            if (!(wx_file %in% list.files(working_directory))) {
                stop(paste("The specified wx_file:", wx_file, "does not exist in the working directory:", working_directory))
            } else {
                # read wx_data from the specified file
                wx_data <- read.csv(wx_file)
            }
        }
    }

    # Check if model is specified
    if (missing(model)) {
        model <- NULL
    }

    # Loop through each station name and pull NWP data, appending to that station's data
    # initialize empty list to store NWP data
    nwp_data_list <- list()

    # loop through wx_data by station name
    for (station in unique(wx_data$STATION_NAME)) {
        station_data <- wx_data %>% filter(STATION_NAME == station)

        location <- c(
            station_data$LATITUDE[1],
            station_data$LONGITUDE[1]
        )
        message("Pulling NWP data for station: ", station, " at location: ", location)

        
        # Pull NWP data using openmeteo API
        nwp_data <- weather_forecast(
            location = location,
            start = as.Date(station_data$DATE_TIME[1]),
            end = as.Date(station_data$DATE_TIME[1]) + 10,  # Default to 10-day forecast
            model = model,
            daily = c(
                "temperature_2m_max", "relative_humidity_2m_min",
                "precipitation_sum", "wind_speed_10m_mean"
                ),
            timezone = "auto"
        )
        
        # Store the NWP data in the list
        nwp_data_list[[station]] <- nwp_data
    }
    return(nwp_data_list)
}
