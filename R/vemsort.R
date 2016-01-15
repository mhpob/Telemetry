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
#'    false detections to produce flags.
#' @return Output is a data.table containing all detections from
#'    the directory's CSV files
#' @export
#' @examples
#' vemsort('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs')
#' vemsort('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs',
#'          c('37119', '64288'))

vemsort <- function(directory = getwd(), false.det = NULL) {
  # List all files within the provided directory
  files <- list.files(path = directory, pattern = '*.csv', full.names = T,
                      recursive = T)

  # Read in files and rename columns
  detect.list <- suppressWarnings(lapply(files, FUN = data.table::fread,
                        stringsAsFactors = F))

  for (i in seq(1:length(detect.list))){
    names(detect.list[[i]]) <- c('date.utc', 'receiver', 'transmitter',
                                 'trans.name', 'trans.serial', 'sensor.value',
                                 'sensor.unit', 'station', 'lat', 'long')
  }

  # Make list into data frame
  detects <- do.call(rbind, detect.list)
  # Convert UTC to EST/EDT
  detects$date.utc <- lubridate::ymd_hms(detects$date.utc)
  detects$date.local <- lubridate::with_tz(detects$date.utc,
                                           tz = "America/New_York")
  # pull out transmitter ID Standard
  detects <- detects[, trans.num := sapply(strsplit(transmitter, '-'), '[[', 3)]

  if(is.null(false.det)){
    detects <- unique(detects)
  } else{
    detects <- unique(detects[, flag := !transmitter %in% false.det])
  }

  row.names(detects) <- NULL

  detects
}
