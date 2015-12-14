data <- read.csv('p:/obrien/biotelemetry/csi/listening/activedata.csv',
                 stringsAsFactors = F)

data <- data[data$Type == 'B', c('Temp', 'Detections')]

data$qabins <- cut(data$Temp, seq(12,31,1))

fish <- aggregate(data$Detections ~ data$qabins, FUN = sum)
site <- aggregate(data$Temp ~ data$qabins, FUN = length)

d2 <- merge(fish, site)

d2$pse <- d2$'data$Temp'/sum(d2$'data$Temp')
d2$pme <- d2$'data$Detections'/sum(d2$'data$Detections')
d2$qe <- d2$pme/d2$pse
