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

vemsort <- function(directory = getwd(), clust = NULL, prog_bar = F) {
  cat('Reading files...\n')

  # List all files within the provided directory
  files <- list.files(path = directory, pattern = '*.csv', full.names = T,
                      recursive = T)

  # Read in files and name list elements for later indexing
  if(prog_bar == T){
    detect.list <- pbapply::pblapply(cl = clust,
                                     X = files,
                                     FUN = data.table::fread,
                                     sep = ",",
                                     stringsAsFactors = F)
  } else {
    if(is.null(clust)){
      detect.list <- lapply(X = files,
                            FUN = data.table::fread,
                            sep = ",",
                            stringsAsFactors = F)
    } else {
      detect.list <- parallel::parLapply(cl = clust,
                                         X = files,
                                         fun = data.table::fread,
                                         sep = ",",
                                         stringsAsFactors = F)
    }
  }

  names(detect.list) <- grep('*.csv',
                             unlist(strsplit(files, '/')),
                             value = T)


  cat('Binding files...\n')

  # Make list into data frame
  detects <- data.table::rbindlist(detect.list, fill = T, idcol = 'file')
  names(detects) <- c('file', 'date.utc', 'receiver', 'transmitter',
                      'trans.name', 'trans.serial', 'sensor.value',
                      'sensor.unit', 'station', 'lat', 'long')


  cat('Final data manipulation...\n')

  # Convert UTC to computer's local time zone
  detects$date.utc <- lubridate::ymd_hms(detects$date.utc)
  detects$date.local <- lubridate::with_tz(detects$date.utc,
                                           tz = Sys.timezone())

  # Move columns around
  detects <- detects[, c('date.utc', 'date.local', 'receiver', 'transmitter',
                         'trans.name', 'trans.serial', 'sensor.value',
                         'sensor.unit', 'station', 'lat', 'long', 'file')]

  # Select unique detections
  detects <- unique(detects, by = c('date.utc', 'transmitter', 'station'))

  as.data.frame(detects)
}
