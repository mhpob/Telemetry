
quo_an <- function(env, det, bin_width = 1, pres_abs = F){
  # Create grouping classes.
  classes <- cut(env, seq(floor(range(env)[1]),
                         ceiling(range(env)[2]),
                         bin_width))

  # Aggregate by environmental classes.
  if(pres_abs == T){
    # Presence/Absence
    fish <- data.frame(det, classes)
    fish <- fish[det > 0,]
    fish <- aggregate(det ~ classes, data = fish, FUN = length)
  } else{
    # Incidence
    fish <- aggregate(det ~ classes, FUN = sum)
  }

  station <- aggregate(env ~ classes, FUN = length)

  q_an <- merge(fish, station, all = T)
  q_an[is.na(q_an)] <- 0

  q_an$pme <- q_an$det/sum(q_an$det)
  q_an$pse <- q_an$env/sum(q_an$env)

  q_an$qe <- q_an$pme/q_an$pse

  q_an
}
