#' Split detections into CSV files according to ACT database
#'
#' \code{ACTsplit} filters all detections according to the researchers in the
#' \href{http://www.theactnetwork.com/}{ACT Network}
#'
#' This function uses \code{\link{vemsort}} to import detections, then matches
#' these detections with transmitters submitted to ACT. It also allows a
#' specified date range and removal of personal or private transmitters.
#'
#' @param directory String passed on to \code{\link{vemsort}}. Location of CSV
#'    data, which defaults to current working directory.
#' @param ACTtrans character. Location of ACT transmitter database.
#' @param my.trans Numeric vector. Tag ID codes that you want removed prior
#'    to distribution.
#' @param write Logical. Do you want to output CSV files? Useful if you are
#'    only looking for unidentified detections.
#' @param out String. Where do you want the CSV files placed?
#' @param start Numeric. Date in ymd form from which you want to start pulling
#'    detections, assumed to be Eastern time zone. Defaults to Jan 1, 2000.
#' @param end Numeric. Date in ymd form to stop pulling detections, assumed to
#'    be Eastern time zone. Defaults to current system date.
#' @return Outputs are CSV files in the form of ResearcherCurrentdate.csv and a
#'    data frame containing detections of unidentified codes. The CSV files
#'    will be found in your current working directory by default.
#' @seealso \code{\link{vemsort}}, \code{\link{ACTupdate}}
#' @export
#' @examples
#' ACTsplit('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs')
#' # Choose detections from April 1, 2014 up to and including August 1, 2014.
#' ACTsplit('C:/Users/MYPCNAME/Documents/Vemco/Vue/ReceiverLogs',
#'            start = 20140401, end = 20140801)

ACTsplit <- function(directory = getwd(), ACTtrans, my.trans = NULL,
                     write = TRUE, out = NULL,
                     start = 20040101, end = Sys.Date()){

  detects <- if(is.data.frame(directory)){
    directory
    } else{
      vemsort(directory)
    }

  # Filter for date range
  stdate <- lubridate::ymd(start, tz = 'America/New_York')
  enddate <- lubridate::ymd(end, tz = 'America/New_York')

  detects <- dplyr::filter(detects,
                           date.local >= stdate &
                           date.local <= enddate)

  ACTtrans <- get(load(ACTtrans))

  cat('Splitting...\n')

  id <- merge(detects, ACTtrans[, names(ACTtrans) %in%
                          c('Tag.ID.Code.Standard', 'Primary.Researcher')],
              by.x = c('transmitter'),
              by.y = c('Tag.ID.Code.Standard'), all.x = T)

  id <- merge(id, ACTtrans[, names(ACTtrans) %in%
                          c('Tag.ID.Code.Sensor.I', 'Primary.Researcher')],
              by.x = c('transmitter'),
              by.y = c('Tag.ID.Code.Sensor.I'), all.x = T)

  id <- merge(id, ACTtrans[, names(ACTtrans) %in%
                          c('Tag.ID.Code.Sensor.II', 'Primary.Researcher')],
              by.x = c('transmitter'),
              by.y = c('Tag.ID.Code.Sensor.II'), all.x = T)

  id$Primary.Researcher <- paste0(id$Primary.Researcher,
                                  id$Primary.Researcher.x,
                                  id$Primary.Researcher.y)
  id$Primary.Researcher <- gsub('NA', '', id$Primary.Researcher)
  id <- id[, !names(id) %in% c('Primary.Researcher.x', 'Primary.Researcher.y')]


  unid <- dplyr::filter(id, Primary.Researcher == ''|
                          is.na(Primary.Researcher))


  id.list <- split(data.frame(id), id$Primary.Researcher)

  csv.root <- ifelse(!is.null(out), out,
                     ifelse(is.data.frame(directory),
                            getwd(), directory))

  if(write == TRUE){
    cat('Writing files...\n')
    pb <- txtProgressBar(char = '+', width = 50, style = 3)
    for(i in seq(length(id.list))){
      id.list[[i]] <- id.list[[i]][
        c('date.utc','receiver', 'transmitter', 'trans.name', 'trans.serial',
          'sensor.value', 'sensor.unit', 'station', 'lat', 'long')]
      names(id.list[[i]]) <- c('Date and Time (UTC)', 'Receiver', 'Transmitter',
                    'Transmitter Name', 'Transmitter Serial', 'Sensor Value',
                    'Sensor Unit', 'Station Name', 'Latitude', 'Longitude')
      id.list[[i]] <- id.list[[i]][order(id.list[[i]]['Transmitter'],
                                         id.list[[i]]['Date and Time (UTC)']),]
      write.csv(id.list[[i]], file = paste(csv.root,
                                     paste0(gsub(' ', '', names(id.list[i])),
                                            Sys.Date(),'.csv'), sep = '/'),
                row.names = F)
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }

  cat('Done.\n')

  unid[, names(unid) != 'Primary.Researcher']
}
