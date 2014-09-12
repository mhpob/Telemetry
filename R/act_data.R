#' Split detections into CSV files according to ACT database
#'
#' \code{ACTsplit} filters all detections according to the researchers in ACT
#' 
#' This function uses \code{vemsort} to import detections, then matches
#' these detections with transmitters submitted to ACT. It also allows a
#' specified date range, removal of personal or private transmitters, and false
#' detections.
#' 
#' @param directory String passed on to \code{vemsort}. Location of CSV data,
#'    which defaults to current wd.
#' @param ACT String. Location of ACT's 'Active transmitters M-D-YY.csv' sheet.
#'    File must be pre-converted from XLSX to CSV in order to work. Defaults to
#'    a file with "Active" in the name within listed directory.
#' @param my.trans Numeric vector. Tag ID codes that you want removed prior
#'    to distribution.
#' @param false.det Numeric vector passed on to \code{vemsort}. Contains tag ID
#'    codes of known false detections.
#' @param start Numeric. Date in ymd form from which you want to start pulling
#'    detections. Defaults to Jan 1, 2000.
#' @param end Numeric. Date in ymd form to stop pulling detections. Defaults to
#'    current system date.
#' @return Outputs are CSV files in the form of ResearcherCurrentate.csv and a
#'    data frame containing detections of unidentified codes. The CSV files
#'    will be found in your current working directory.
#' @export
#' @examples
#' ACTsplit('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs')
#' # Specify a different location for ACT transmitters and choose detections
#' # from April 1, 2014 up to and including August 1, 2014.
#' ACTsplit('C:/Users/MYPCNAME/Documents/Vemco/Vue/ReceiverLogs',
#'          'C:/Users/MYPCNAME/Documents/Active transmitters 9-2-14.csv',
#'            start = 20140401, end = 20140801)

ACTsplit <- function(directory = getwd(),
              ACT = paste(directory,list.files(directory, 'Active'), sep = "/"),
              my.trans = NULL, false.det = NULL,
              start = 20000101, end = Sys.Date()){
  
  detects <- vemsort(directory, false.det)
  act <- read.csv(ACT, header = T, stringsAsFactors = F)
  
  # Filter for ID'ed detections that aren't yours
  id <- dplyr::filter(act, Tag.ID.Code.Standard %in% detects$transmitter,
                      !Tag.ID.Code.Standard %in% my.trans)
  
  id <- merge(detects, id[, c(1:2, 14:15)],
              by.x = c('transmitter', 'trans.num'),
              by.y = c('Tag.ID.Code.Standard', 'ID.Standard'))
  
  unid <- dplyr::filter(detects,
                        transmitter %in% setdiff(detects$transmitter,
                                                 act$Tag.ID.Code.Standard))
  print(unid)
  
  j <- split(id, id$Primary.Researcher)
  
  stdate <- lubridate::ymd(start)
  enddate <- lubridate::ymd(end) + lubridate::days(1)
  
  for(i in seq(length(j))){
    j[[i]] <- j[[i]][c(1,3:7)]
    j[[i]][7:10] <- NA
    j[[i]] <- j[[i]][c(2, 3, 1, 7:10, 4:6)]
    names(j[[i]]) <- c('Date and Time (UTC)', 'Receiver', 'Transmitter',
                       'Transmitter Name', 'Transmitter Serial', 'Sensor Value',
                       'Sensor Unit', 'Station Name', 'Latitude', 'Longitude')
    j[[i]] <- j[[i]][order(j[[i]][3], j[[i]][1]),]
    j[[i]] <- j[[i]][j[[i]][,1] >= stdate & j[[i]][,1] <= enddate,]
    write.csv(j[[i]], file = paste(gsub(' ', '', names(j[i])),
                                   Sys.Date(),'.csv', sep = ''),
              row.names = F)
  }
}