#####################################################################
#####################################################################
## author: Lochlan Noble
## initialization date: 2025.07.11
## purpose: calculate 10-day indices from compiled weather data
## notes: 
##        
## outputs: see folder ""
## updated: 
## TODO: - 
##       - add description for inputs and outputs
##       - add more examples
######################################################################
## Variables:
## working_directory: Directory where the station info file is located or will be saved
## wx_file: Name of the weather station info file 
## stn_info_file: Name of the station info file to read (default is "bcws-wx-station-info.csv")
#####################################################################
#####################################################################

calc_10day_indices <- function(wx_data, nwp_data, stations) {
    #Check for required libraries and install if needed
        # Check if required packages are installed
    if (!requireNamespace("tidyverse", quietly = TRUE)) {
        install.packages("tidyverse")
    }
    if (!requireNamespace("cffdrs", quietly = TRUE)) {
        install.packages("cffdrs")
    }
   
    # Load necessary libraries
    library(tidyverse)
    library(cffdrs)

    # Source helper functions from GitHub
    source("https://raw.githubusercontent.com/bcgov/wps-fire-behaviour/refs/heads/main/R/APU/pull_wx_ftp.R")
    source("https://raw.githubusercontent.com/bcgov/wps-fire-behaviour/refs/heads/main/R/APU/station_info.R")
    source("https://raw.githubusercontent.com/bcgov/wps-fire-behaviour/refs/heads/main/R/APU/pull_wx_nwp.R")

    # check inputs and use if available
    if ()

}

