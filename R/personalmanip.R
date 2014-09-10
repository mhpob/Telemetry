# Some data from MD DNR came in with location data missing
dnr <- data.frame(receiver = c('VR2W-106474', 'VR2W-102036', 'VR2W-106473',
                               'VR2W-106478', 'VR2W-123776'),
                station1 = c('Kent Island A', 'Kent Island B', 'Kent Island C',
                             'Kent Island D', 'Choptank/USCG Lighted Buoy 1'),
                  lat1 = c(38.9953333, 38.9913167, 38.9841500,
                           38.9799167, 38.5762694),
                  long1 = c(-76.3995333, -76.3902333, -76.3727833,
                            -76.3539667, -76.0649111),
                  stringsAsFactors = F)
  
detects <- merge(detects, dnr, all.x = T)
detects$station <- ifelse(is.na(detects$station), detects$station1,
                          detects$station)
detects$lat <- ifelse(is.na(detects$lat), detects$lat1, detects$lat)
detects$long <- ifelse(is.na(detects$long), detects$long1, detects$long)
detects <- detects[,-c(9:11)]
  
    
arr <- function(part){grepl(part, detects[,4], ignore.case = T)}
  
detects$array <- ifelse(detects[,4] == 'CBL Pier', 'CBL Pier',
            ifelse(arr('cedar'), 'Cedar Point',
            ifelse(arr('piney'), 'Piney Point',
            ifelse(arr('pot'), 'CBIBS',
            ifelse(arr('301'), 'Rt 301',
            ifelse(arr('kent'), 'Kent Island',
            ifelse(arr('chop'), 'Choptank',
            ifelse(arr('marsh'), 'Marshyhope',
            ifelse(arr('nan'), 'Nanticoke',
            ifelse(arr('poco'), 'Pocomoke',
            ifelse(arr('repo'), 'Reports',
            ifelse(arr('dmf') | arr('vine'), 'Mass',
            ifelse(detects$station %in%
                     c('CC LS', 'LC2', 'NCD', 'NN 1ER FWS',
                       'NN 22 NOAA SP', 'NN DANGER FWS', 'Y wat'), 'Navy',
                       'Other')))))))))))))
  
# False detections as determined by VEMCO
false.det <- c('37119', '64288')







id <- filter(act, Tag.ID.Code.Standard %in% detects$transmitter,
                  !Tag.ID.Code.Standard %in%
                    paste('A69-1601', seq(25434, 25505), sep = '-'))

  