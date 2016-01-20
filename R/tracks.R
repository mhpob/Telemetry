load('p:/obrien/biotelemetry/striped bass/past-analysis/secor.sb.rda')
test <- dplyr::filter(secor.sb, trans.num == 25434)
dates <- data.frame(test)[,'date.local']
loc.id <- data.frame(test)[, c('lat','long')]
vemsort <- F

test2 <- test[1,]
dates <- data.frame(test2)[,'date.local']
id <- data.frame(test2)[,'receiver']

# need station ids or lat/long, dates
# allow for vemsort data to be automatic

tracks <- function(dates, loc.id, vemsort = F, data){
  if(vemsort){
    dates <- data$date.local
    loc.id <- data$station
  }

  data <- data.frame(dates, loc.id)
  data <- data[order(data$dates),]

  if(dim(data)[1] <= 1){
    track <- data[1,]
  } else{
    track <- data[1,]
    for(i in seq(1, dim(data)[1] - 1)){
      if(F %in% (data[i, !names(data) == "dates"] ==
                 data[i + 1, !names(data) == "dates"])){
        track <- rbind(track, data[i + 1,])
      }
    }
  }
  row.names(track) <- NULL
  track
}


