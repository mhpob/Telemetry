load('p:/obrien/biotelemetry/striped bass/past-analysis/secor.sb.rda')
test <- dplyr::filter(secor.sb, trans.num == 25434)
dates <- data.frame(test)[,'date.local']
id <- data.frame(test)[,'receiver']

test2 <- test[1,]
dates <- data.frame(test2)[,'date.local']
id <- data.frame(test2)[,'receiver']

# need station ids or lat/long, dates, fish ids

tracks <- function(dates, locations, fish, vemsort = F, data = NULL){
#   if(vemsort == T){
#     dates <- data$date.local
#     locations <- data$station
#   }

  odr <- order(dates)
  dates <- dates[odr]
  locations <- locations[odr]

  if(length(unique(locations)) <= 1){
    track <- unique(locations)
  } else{
    track <- locations[1]
    for(i in seq(1, length(locations) - 1)){
      if(locations[i] != locations[i + 1]){
        track <- c(track, locations[i + 1])
      }
    }
  }
  track
}


