#' Create coarse movement tracks.
#'
#' \code{tracks} returns locations (station, array, etc.) in the order in which
#' they were visited.
#'
#' This function orders detection data by date/time and only returns detections
#' where the fish moved from one identified location to another.
#'
#' @param data Data frame. Location of date/time and identifier values.
#' @param dates character. Column name of date/time values within \code{data}.
#' @param ids character. Column name(s) of identifiers within \code{data}.
#'    Multiple names may be provided in a vector: c('station', 'lat', 'long'),
#'    e.g.
#' @return Output is a data frame with date/times at unique combinations of
#'    provided IDs.
#' @export

track <- function(data, dates, ids){
  data <- data[order(data.frame(data)[, dates]), c(dates, ids)]

  if(dim(data)[1] <= 1){
    track <- data[1,]
  } else{
    track <- data[1,]
    for(i in seq(1, dim(data)[1] - 1)){
      if(F %in% (data[i, !names(data) == dates] ==
                 data[i + 1, !names(data) == dates])){
        track <- rbind(track, data[i + 1,])
      }
    }
  }
  row.names(track) <- NULL
  track
}
