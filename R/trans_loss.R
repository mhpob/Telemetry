#' Calculate number of transmitters remaining over time
#'
#' \code{trans_loss} returns the number of transmitters (or other group) remaining
#' each day of a given time period
#'
#' This function finds the most-recent date of detection for each group (i.e.,
#' transmitter/fish). For each date in a given range, it then counts the number
#' of fish that have a more-recent final detection date.
#'
#' @param data Data frame. Detection data including date and time of detection,
#'    as well as a group identifier.
#' @param dates Date- or POSIX-class. Date and time of detections.
#' @param group Character. Group identifier (transmitter, fish ID, etc.).
#' @param stdate Date- or POSIX-class. Which day you want the time series to start?
#'    Defaults to day of earliest detection in the data set. Must be the same
#'    class as \code{enddate}
#' @param enddate Date- or POSIX-class.
#' @return Output is a data frame containing the number of each group remaining
#'    at each date.
#' @export

trans_loss <- function(data, dates, group, stdate = NULL, enddate = NULL){

  if(is.null(stdate)){
    stdate <- lubridate::floor_date(min(data.frame(data)[, dates]),
                                    unit = 'day')
  }
  if(is.null(enddate)){
    enddate <- lubridate::floor_date(max(data.frame(data)[, dates]),
                                     unit = 'day')
  }

  data <- data %>%
    dplyr::group_by_(group) %>%
    dplyr::summarize_(last.record = lazyeval::interp(~max(x),
                                                     x = as.name(dates)))

  data$interval <- lubridate::interval(stdate, data$last.record)

  date.seq <- data.frame(date = seq(stdate, enddate, by = 'day'))
  date.seq$remaining <- sapply(date.seq$date,
              function(x) sum((lubridate::'%within%' (x, data$interval)) == T))

  date.seq
}
