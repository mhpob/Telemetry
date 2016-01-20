load('p:/obrien/biotelemetry/striped bass/past-analysis/secor.sb.rda')
test <- dplyr::filter(secor.sb, trans.num == 25434)
dates <- data.frame(test)[,'date.local']
loc.id <- data.frame(test)[,c('lat','long')]

test2 <- test[1,]
dates <- data.frame(test2)[,'date.local']
id <- data.frame(test2)[,'receiver']

# need station ids or lat/long, dates, fish ids
# allow for vemsort data to be automatic

tracks <- function(dates, loc.id, fish, vemsort = F, data = NULL){
#   if(vemsort == T){
#     dates <- data$date.local
#     loc.id <- data$station
#   }

  odr <- order(dates)
  dates <- dates[odr]
  is.1D <- is.null(dim(loc.id))
  if(is.1D){
    loc.id <- loc.id[odr]
  } else{
    loc.id <- loc.id[odr,]
  }

  has.moved <- ifelse(is.1D, length(unique(loc.id)) > 1, dim(loc.id)[1] > 1)

  if(!has.moved){
    track <- unique(loc.id)
  } else{
    if(is.1D){
      track <- loc.id[1]
      for(i in seq(1, length(loc.id) - 1)){
        if(loc.id[i] != loc.id[i + 1]){
          track <- c(track, loc.id[i + 1])
        }
      }
    } else{
      track <- loc.id[1,]
      for(i in seq(1, dim(loc.id)[1] - 1)){
        if(loc.id[i, 1] != loc.id[i + 1, 1] &
           loc.id[i, 2] != loc.id[i + 1, 2]){
          track <- rbind(track, loc.id[i + 1,])
        }
      }
    }
  }
  track
}


