load('p:/obrien/biotelemetry/striped bass/past-analysis/secor.sb.rda')

gen.movement <- function(data){
  if(dim(data)[1] <= 1){
    track <- data[1, 'array']
  }
  else{
      track <- data[1, 'array']
      for(i in seq(1, dim(data)[1] - 1)){
        if(data[i, 'array'] != data[i + 1, 'array']){
          track <- c(track, data[i + 1, 'array'])
        }
      }
  }
  track
}