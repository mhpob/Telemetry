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
#' @return Output is a data.table containing all detections from
#'    the directory's CSV files. Adds two columns: one containing local time of
#'    the detections (as defined by \code{Sys.timzone}) and one containing the
#'    detection's CSV file of origin.
#' @export
#' @examples
#' vemsort('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs')
#' vemsort('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs',
#'          c('37119', '64288'))

vemsort <- function(directory = getwd()) {
  # List all files within the provided directory
  files <- list.files(path = directory, pattern = '*.csv', full.names = T,
                      recursive = T)

  # Read in files and rename columns
  cat('Reading files...\n')
  detect.list <- suppressWarnings(
    pbapply::pblapply(files,
                      FUN = data.table::fread,
                      sep = ",",
                      stringsAsFactors = F,
                      fill = T))
  cat('Done.\n')

  for (i in seq(1:length(detect.list))){
    names(detect.list[[i]]) <- c('date.utc', 'receiver', 'transmitter',
                                 'trans.name', 'trans.serial', 'sensor.value',
                                 'sensor.unit', 'station', 'lat', 'long')
    detect.list[[i]]$file <- grep('*.csv',
                                  unlist(strsplit(files[i], '/')),
                                  value = T)
  }

  # Make list into data frame
  cat('Binding files...\n')
  detects <- do.call(rbind, detect.list)
  cat('Done.\n')

  cat('Final data manipulation...\n')
  # Convert UTC to EST/EDT
  detects$date.utc <- lubridate::ymd_hms(detects$date.utc)
  detects$date.local <- lubridate::with_tz(detects$date.utc,
                                           tz = Sys.timezone())
  detects <- detects[, c('date.utc', 'date.local', 'receiver', 'transmitter',
                         'trans.name', 'trans.serial', 'sensor.value',
                         'sensor.unit', 'station', 'lat', 'long', 'file')]
  detects <- detects[!duplicated(detects, by = c('date.utc', 'transmitter'),
                           fromLast = T),]
  row.names(detects) <- NULL
  cat('Done.\n')

  detects
}
