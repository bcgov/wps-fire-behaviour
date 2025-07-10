#####################################################################
#####################################################################
## author: Lochlan Noble
## initialization date: 2025.07.03
## purpose: pull and append wx station info from BCWS DataMart or local file
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

station_info <- function(wx_file, stn_info_file) { 
    # Load necessary libraries



    working_directory <- getwd()
    
    ##ERROR HANDLING
    # Check if wx_file exists
    if (missing(wx_file)) {
        stop("wx_file must be specified. Please provide the name of the weather station info file.")
    }

    if (!(wx_file %in% list.files(working_directory))) {
        stop(paste("The specified wx_file:", wx_file, "does not exist in the working directory:", working_directory))
    }
    
    # Check if station info is provided, use default if not
    if (missing(stn_info_file)) {
        if ("bcws-wx-station-info.csv" %in% list.files(working_directory)) {
            stn_info_file <- "bcws-wx-station-info.csv"
        } else { # If no default, download from FTP
            message("No station info file found in the working directory.\n",
                    "Trying to download the latest station info file from BCWS DataMart."
            )
            year <- as.numeric(format(Sys.Date(), "%Y")) - 1
            info_bool <- FALSE
            while (info_bool == FALSE) {
                # check if info file exists for previous year
                ftp_url <- paste0(
                "https://www.for.gov.bc.ca/ftp/HPR/external/!publish/BCWS_DATA_MART/",
                year, "/"
                )
                fname <- paste0(year, "_BCWS_WX_STATIONS.csv")
                if (fname %in% list.files(ftp_url)) {
                    download.file(
                        paste0(ftp_url, fname),
                        destfile = file.path(working_directory, "bcws-wx-station-info.csv")
                    )
                    message("Downloaded station info file for year: ", year)
                    stn_info_file <- "bcws-wx-station-info.csv"
                    info_bool <- TRUE
                } else {
                year <- year - 1
                message("No station info file found for year: ", year, ". Trying previous year.")
                }
            }
        }
    }
    
    # Load wx data and station info files
    wx_data <- read.csv(wx_file, stringsAsFactors = FALSE)
    stn_info <- read.csv(stn_info_file, stringsAsFactors = FALSE)

    # Find stations that are in the wx_data but not in the stn_info
    missing_stations <- setdiff(wx_data$STATION_NAME, stn_info$STATION_NAME)
    if (length(missing_stations) > 0) {
        message("The following stations are in the wx_data but not in the stn_info file:")
        print(missing_stations)
    } 

    # Merge wx_data with stn_info to get station information
    station_data <- merge(wx_data, stn_info, by = "STATION_NAME", all.x = TRUE)

    # Return the station data
    return(station_data)

}