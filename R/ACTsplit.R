#' Split detections into CSV files according to ACT database
#'
#' \code{ACTsplit} filters all detections according to the researchers in the
#' \href{http://www.theactnetwork.com/}{ACT Network}
#'
#' This function uses \code{\link{vemsort}} to import detections, then matches
#' these detections with transmitters submitted to ACT. It also allows a
#' specified date range, removal of personal or private transmitters, and false
#' detections.
#'
#' @param directory String passed on to \code{\link{vemsort}}. Location of CSV
#'    data, which defaults to current working directory.
#' @param my.trans Numeric vector. Tag ID codes that you want removed prior
#'    to distribution.
#' @param false.det Numeric vector passed on to \code{\link{vemsort}}. Contains
#'    tag ID codes of known false detections.
#' @param write Logical. Do you want to output CSV files? Useful if you are
#'    only looking for unidentified detections.
#' @param out String. Where do you want the CSV files placed?
#' @param start Numeric. Date in ymd form from which you want to start pulling
#'    detections. Defaults to Jan 1, 2000.
#' @param end Numeric. Date in ymd form to stop pulling detections. Defaults to
#'    current system date.
#' @return Outputs are CSV files in the form of ResearcherCurrentdate.csv and a
#'    data frame containing detections of unidentified codes. The CSV files
#'    will be found in your current working directory by default.
#' @seealso \code{\link{vemsort}}
#' @export
#' @examples
#' ACTsplit('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs')
#' # Choose detections from April 1, 2014 up to and including August 1, 2014.
#' ACTsplit('C:/Users/MYPCNAME/Documents/Vemco/Vue/ReceiverLogs',
#'            start = 20140401, end = 20140801)

ACTsplit <- function(directory = getwd(), my.trans = NULL, false.det = NULL,
                     write = TRUE, out = NULL,
                     start = 20000101, end = Sys.Date()){

  detects <- if(is.data.frame(directory)){
    directory
    } else{
      vemsort(directory, false.det)
    }

  # Filter for date range
  stdate <- lubridate::ymd(start, tz = 'America/New_York')
  enddate <- lubridate::ymd(end, tz = 'America/New_York')

  detects <- dplyr::filter(detects,
                           date.local >= stdate &
                           date.local <= enddate)

  # Filter for ID'ed detections that aren't yours
  id <- dplyr::filter(ACTtrans, Tag.ID.Code.Standard %in% detects$transmitter,
                      !Tag.ID.Code.Standard %in% my.trans)

  id <- merge(detects, id[, c(1:2, 13:14)],
              by.x = c('transmitter', 'trans.num'),
              by.y = c('Tag.ID.Code.Standard', 'ID.Standard'))

  flag.id <- id[id$flag == F,]
  id <- id[id$flag == T,]

  unid <- dplyr::filter(detects,
                        transmitter %in% setdiff(detects$transmitter,
                                                 ACTtrans$Tag.ID.Code.Standard),
                        flag == T)


  j <- split(id, id$Primary.Researcher)

  csv.root <- ifelse(!is.null(out), out,
                     ifelse(is.data.frame(directory),
                            getwd(), directory))

  if(write == TRUE){
    for(i in seq(length(j))){
      j[[i]] <- j[[i]][c(3:4, 1, 5:11)]
      names(j[[i]]) <- c('Date and Time (UTC)', 'Receiver', 'Transmitter',
                    'Transmitter Name', 'Transmitter Serial', 'Sensor Value',
                    'Sensor Unit', 'Station Name', 'Latitude', 'Longitude')
      j[[i]] <- j[[i]][order(j[[i]][3], j[[i]][1]),]
      write.csv(j[[i]], file = paste(csv.root,
                                     paste0(gsub(' ', '', names(j[i])),
                                            Sys.Date(),'.csv'), sep = '/'),
                row.names = F)
    }
  }

  list(UNID = unid, ID_flag = flag.id)
}
