

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
