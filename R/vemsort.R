#' Prepare VEMCO transmitter CSV files for analysis
#' 
#' \code{vemsort} finds and combines all VEMCO CSV files in a directory
#' 
#' This function assumes that all necessary CSV files are within the specified
#' directory or subdirectories within. All files must have the default headings
#' offloaded by VEMCO products. These are, in order:
#' Date and Time (UTC), Receiver, Transmitter, Transmitter Name,
#' Transmitter Serial, Sensor Value, Sensor Unit, Station Name,
#' Latitude, Longitude.
#' 
#' @param directory String. Location of CSV data, defaults to current wd.
#' @param false.det Numeric vector. Contains tag ID codes of known
#'    false detections.
#' @return Output is a dplyr table data frame containing all detections from
#'    the directory's CSV files
#' @export
#' @examples
#' vemsort('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs')
#' vemsort('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs', 
#'          c('37119', '64288'))

vemsort <- function(directory = getwd(), false.det = NULL) {
  # List all files within the "detections" folder
  files <- list.files(path = directory)
  files <- paste(directory, files, sep = '/')
  # Pull out sub-folders within the "detections" folder
  folders <- files[file.info(files)$isdir]
  
  # Create file paths for all files ending in ".csv" within the sub-folders
  file.locs1 <- NULL
  file.locs2 <- NULL
  for (i in seq(1:length(folders))){
    file.locs1 <- do.call(paste, 
                 list(folders[i], 
                      list.files(path = folders[i], pattern = '*.csv'),
                      sep = '/'))
    file.locs2 <- c(file.locs2, file.locs1)
  }
  
  # Read in files located by the steps above and rename columns
  detect.list <- lapply(file.locs2, FUN = read.csv,
                        header = T, stringsAsFactors = F)
  for (i in seq(1:length(detect.list))){
    names(detect.list[[i]]) <- c('date.utc', 'receiver', 'transmitter',
                                 'drop1', 'drop2', 'drop3', 'drop4',
                                 'station', 'lat', 'long')
  }
  
  # Make list into data frame
  detects <- do.call(rbind.data.frame, detect.list)
  # Drop emtpy columns
  detects <- detects[,c(1:3, 8, 9:10)]
  # Convert UTC to EST/EDT
  detects$date.utc <- lubridate::ymd_hms(detects$date.utc)
  detects$date.local <- lubridate::with_tz(detects$date.utc,
                                           tz = "America/New_York")
  # pull out transmitter ID Standard
  detects$trans.num <- as.numeric(sapply(strsplit(detects[,3], '-'),'[[',3))

  detects <- unique(dplyr::filter(detects, !transmitter %in% false.det))
  row.names(detects) <- NULL
  
  dplyr::tbl_df(detects)
}
