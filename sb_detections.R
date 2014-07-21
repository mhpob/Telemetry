source('p:/obrien/biotelemetry/detections/detection_sort.R')
setwd('p:/obrien/biotelemetry/striped bass')


secor.sb <- filter(detects, trans.num >= 25434 & trans.num <= 25505)

tag.data <- read.csv('p:/obrien/biotelemetry/striped bass/taggingdata.csv', header = T, stringsAsFactors = F)
tag.data$Date <- mdy(tag.data$Date, tz = 'America/New_York')
tag.data <- tag.data[, c(1,2,4,5,7)]
names(tag.data) <- c('tag.date', 'trans.num', 'size', 'length', 'weight')

secor.sb <- merge(secor.sb, tag.data, all.x = T)
