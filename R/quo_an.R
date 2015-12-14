data <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                 stringsAsFactors = F)

env <- data[data$Type == 'B', 'Temp']
det <- data[data$Type == 'B', 'Detections']
bin_width <- 1

quo_an <- function(env, det, bin_width = 1){
  # Create grouping claesses
  classes <- cut(env, seq(floor(range(env)[1]),
                         ceiling(range(env)[2]),
                         bin_width))

  # Aggregate by grouping classes.
  fish <- aggregate(det ~ classes, FUN = sum)
  station <- aggregate(env ~ classes, FUN = length)

  q_an <- merge(fish, station)

  q_an$pse <- q_an$env/sum(q_an$env)
  q_an$pme <- q_an$det/sum(q_an$det)

  q_an$qe <- q_an$pme/q_an$pse

  q_an
}
