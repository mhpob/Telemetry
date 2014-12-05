library(TelemetryR); library(lubridate); library(dplyr)

# False detections as determined by VEMCO
false <- c('37119', '64288')
detects <- vemsort('p:/obrien/biotelemetry/detections', false)

# Some data from MD DNR came in with location data missing
# dnr <- data.frame(receiver = c('VR2W-106474', 'VR2W-102036', 'VR2W-106473',
#                                'VR2W-106478'),
#                 station1 = c('Kent Island A', 'Kent Island B', 'Kent Island C',
#                              'Kent Island D'),
#                   lat1 = c(38.9953333, 38.9913167, 38.9841500,
#                            38.9799167),
#                   long1 = c(-76.3995333, -76.3902333, -76.3727833,
#                             -76.3539667),
#                   stringsAsFactors = F)
#   
# detects <- merge(detects, dnr, all.x = T)
# detects$station <- ifelse(is.na(detects$station), detects$station1,
#                           detects$station)
# detects$lat <- ifelse(is.na(detects$lat), detects$lat1, detects$lat)
# detects$long <- ifelse(is.na(detects$long), detects$long1, detects$long)
# detects <- detects[,-c(9:11)]
# detects <- detects[,c(2,1,3:8)]
# 
#     
# arr <- function(part){grepl(part, detects[,4], ignore.case = T)}
#   
# detects$array <- ifelse(detects[,4] == 'CBL Pier', 'CBL Pier',
#             ifelse(arr('cedar'), 'Cedar Point',
#             ifelse(arr('piney'), 'Piney Point',
#             ifelse(arr('pot'), 'CBIBS',
#             ifelse(arr('301'), 'Rt 301',
#             ifelse(arr('kent'), 'Kent Island',
#             ifelse(arr('chop'), 'Choptank',
#             ifelse(arr('marsh'), 'Marshyhope',
#             ifelse(arr('nan'), 'Nanticoke',
#             ifelse(arr('poco'), 'Pocomoke',
#             ifelse(arr('repo'), 'Reports',
#             ifelse(arr('dmf') | arr('vine'), 'Mass',
#             ifelse(detects$station %in%
#                      c('CC LS', 'LC2', 'NCD', 'NN 1ER FWS',
#                        'NN 22 NOAA SP', 'NN DANGER FWS', 'Y wat'), 'Navy',
#                        'Other')))))))))))))
  

secor.sb <- filter(detects, trans.num >= 25434 & trans.num <= 25533)

tag.data <- read.csv('p:/obrien/biotelemetry/striped bass/taggingdata.csv',
                     header = T, stringsAsFactors = F)
tag.data$Date <- mdy(tag.data$Date, tz = 'America/New_York')
tag.data <- tag.data[, c(1, 2, 5, 7, 8)]
names(tag.data) <- c('tag.date', 'trans.num', 'length', 'weight', 'sex')

# we reused tag A69-1601-25465 on 2014-10-30. Need to split tagging data to
# reflect this.
firsttagging.25465 <- secor.sb %>% 
  filter(trans.num == 25465, date.utc <= '2014-06-15') %>%
  merge(tag.data[32,], all.x = T)

secondtagging.25465 <- secor.sb %>% 
  filter(trans.num == 25465, date.utc >= '2014-10-29') %>%
  merge(tag.data[101,], all.x = T)

secor.sb <- secor.sb %>% 
  filter(trans.num != 25465) %>% 
  merge(tag.data, all.x = T) %>%
  rbind(firsttagging.25465, secondtagging.25465) %>%
  tbl_df()


rm(detects, false, tag.data, firsttagging.25465, secondtagging.25465)
