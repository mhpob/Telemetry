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
#' @param start Character date in a standard unambiguous format
#'    (e.g., YYYY-MM-DD) from which you want to start pulling
#'    detections, assumed to be Eastern time zone. Defaults to Jan 1, 2000.
#' @param end Character date in a standard unambiguous format
#'    (e.g., YYYY-MM-DD) to stop pulling detections, assumed to
#'    be Eastern time zone. Defaults to current system date.
#' @param ... Optional arguments to \code{\link{vemsort}} (parallel? progress bar?)
#' @return Outputs are CSV files in the form of ResearcherCurrentdate.csv and a
#'    data frame containing detections of unidentified codes. The CSV files
#'    will be found in your current working directory by default.
#' @seealso \code{\link{vemsort}}, \code{\link{ACTupdate}}
#' @export
#' @examples
#' ACTsplit('C:/Users/mypcname/Documents/Vemco/Vue/ReceiverLogs')
#' # Choose detections from April 1, 2014 up to and including August 1, 2014.
#' ACTsplit('C:/Users/MYPCNAME/Documents/Vemco/Vue/ReceiverLogs',
#'            start = '2014-04-01', end = '2014-08-01')

ACTsplit <- function(directory = getwd(), ACTtrans, my.trans = NULL,
                     write = TRUE, out = NULL,
                     start = '2000-01-01', end = Sys.Date(), ...){

  detects <- if(is.data.frame(directory)){
    directory
    } else{
      vemsort(directory, ...)
    }

  cat('Starting to split...\n')

  # Filter for date range
  detects <- data.table::setDT(detects)[,
                           date.local >= start &
                           date.local <= end]

  ACTtrans <- data.table::setDT(get(load(ACTtrans)))

  cat('Finding researcher...\n')

  id <- ACTtrans[, c('Tag.ID.Code.Standard', 'Primary.Researcher')][
    detects, on = c(Tag.ID.Code.Standard = 'transmitter')]

  id <- ACTtrans[, c('Tag.ID.Code.Sensor.I', 'Primary.Researcher')][
    id, on = c(Tag.ID.Code.Sensor.I = 'Tag.ID.Code.Standard')]

  id <- ACTtrans[, c('Tag.ID.Code.Sensor.II', 'Primary.Researcher')][
    id, on = c(Tag.ID.Code.Sensor.II = 'Tag.ID.Code.Sensor.I')]


  id[, Primary.Researcher := paste0(id$Primary.Researcher,
                                    id$i.Primary.Researcher,
                                    id$i.Primary.Researcher.1)]
  id[, Primary.Researcher := gsub('NA', '', Primary.Researcher)]
  id[, ':='(i.Primary.Researcher = NULL,
            i.Primary.Researcher.1 = NULL)]
  data.table::setnames(id, 'Tag.ID.Code.Sensor.II', 'transmitter')


  unid <- id[Primary.Researcher == '' | is.na(Primary.Researcher)]


  if(write == TRUE){
    cat('Preparing to write...\n')
    id <- dplyr::select(id,
                        date.utc, receiver, transmitter, trans.name,
                        trans.serial, sensor.value, sensor.unit, station,
                        lat, long, Primary.Researcher) %>%
      dplyr::arrange(transmitter, date.utc)

    names(id) <- c('Date and Time (UTC)', 'Receiver', 'Transmitter',
                   'Transmitter Name', 'Transmitter Serial', 'Sensor Value',
                   'Sensor Unit', 'Station Name', 'Latitude', 'Longitude',
                   'Primary.Researcher')

    id.list <- split(data.table::data.table(id),
                     by = 'Primary.Researcher',
                     keep.by = F)

    csv.root <- ifelse(!is.null(out), out,
                       ifelse(is.data.frame(directory),
                              getwd(), directory))


    cat('Writing files...\n')
    pb <- txtProgressBar(char = '+', width = 50, style = 3)
    for(i in seq(length(id.list))){
      data.table::fwrite(id.list[[i]],
                         file = paste(csv.root,
                                      paste0(gsub(' ', '', names(id.list[i])),
                                             Sys.Date(),'.csv'), sep = '/'),
                         dateTimeAs = 'write.csv')
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }

  unid[, names(unid) != 'Primary.Researcher']
}
