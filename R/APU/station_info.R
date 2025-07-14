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

station_info <- function(wx_file, stn_info_file, download_to_file) {
    # Check if required packages are installed
    if (!requireNamespace("httr", quietly = TRUE)) {
        install.packages("httr")
    }
    if (!requireNamespace("RCurl", quietly = TRUE)) {
        install.packages("RCurl")
    }

    # Load necessary libraries
    library(httr)
    library(RCurl)

    working_directory <- getwd()
    
    ##ERROR 
    if (missing(download_to_file)) {
        download_to_file <- FALSE  # Default to FALSE if not specified
    }
    # Check if wx_file exists
    if (missing(wx_file)) {
        stop("wx_file must be specified. Please provide the name of the weather station info file.")
    }

    if (!(wx_file %in% list.files(working_directory))) {
        stop(paste("The specified wx_file:", wx_file, "does not exist in the working directory:", working_directory))
    }
    
    # Check if station info is provided, use default if not
    if (missing(stn_info_file)) {
        if ("bcws_wx_stn_info_complete.csv" %in% list.files(working_directory)) {
            stn_info_file <- "bcws_wx_stn_info_complete.csv"
        } else { # If no default, download from FTP
            message("No station info file found in the working directory.\n",
                    "Trying to download the latest station info file from BCWS DataMart."
            )
            year <- as.numeric(format(Sys.Date(), "%Y")) - 1
            info_bool <- FALSE
            while (info_bool == FALSE && year > 2000) {
                # check if info file exists for previous year
                ftp_url <- paste0(
                "https://www.for.gov.bc.ca/ftp/HPR/external/!publish/BCWS_DATA_MART/",
                year, "/"
                )
                fname <- paste0(year, "_BCWS_WX_STATIONS.csv")
                # Check if the URL exists
                url_exists <- url.exists(paste0(ftp_url, fname))
                message("URL for ", year, " exists: ", url_exists)

                if (url_exists) {
                    if (download_to_file == TRUE) {
                        download.file(
                            paste0(ftp_url, fname),
                            destfile = file.path(working_directory, fname)
                        )
                        message("Downloaded station info file for year: ", year)
                        stn_info_file <- fname
                        info_bool <- TRUE
                    } else {
                        # If not downloading file, read it directly
                        stn_info <- read.csv(paste0(ftp_url, fname), stringsAsFactors = FALSE)
                        message("Read station info file for year: ", year)
                        info_bool <- TRUE
                    }
                } else {
                year <- year - 1
                message("No station info file found for year: ", year, ". Trying previous year.")
                if (year <= 2000) {
                    stop("No station info file found for any year since 2000.",
                         "\nPlease provide a valid station info file.")
                }
            }
        }
    }

    # Load wx data and station info files
    wx_data <- read.csv(wx_file, stringsAsFactors = FALSE)
    if (download_to_file == TRUE) {
        stn_info <- read.csv(stn_info_file, stringsAsFactors = FALSE)
    } else {
        stn_info <- stn_info  # Use the already read station info
    }


    # Find stations that are in the wx_data but not in the stn_info
    missing_stations <- setdiff(wx_data$STATION_NAME, stn_info$STATION_NAME)
    if (length(missing_stations) > 0) {
        message("The following stations are in the wx_data but not in the stn_info file:")
        print(missing_stations)
        print("Station data for missing stations has been excluded from the final output.")
    }

    # Merge wx_data with stn_info, using chosen columns
    station_data <- merge(wx_data, stn_info)
    # remove X column
    station_data <- station_data[, !names(station_data) %in% c("X")]

    # Return the station data
    return(station_data)
    }
}