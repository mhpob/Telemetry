vemsort <- function(directory) {
  library(lubridate); library(dplyr)
  
  # List all files within the "detections" folder
  files <- list.files(path = directory)
  files <- paste(directory, files, sep = '/')
  # Pull out sub-folders within the "detections" folder
  folders <- files[file.info(files)$isdir]
  
  # Create file paths for all files ending in ".csv" within the sub-folders
  file.locs1 <- NULL
  file.locs2 <- NULL
  for (i in seq(1:length(folders))){
    file.locs1 <- do.call(paste, 
                 list(folders[i], 
                      list.files(path = folders[i], pattern = '*.csv'),
                      sep = '/'))
    file.locs2 <- c(file.locs2, file.locs1)
  }
  
  # Read in files located by the steps above and rename columns
  detect.list <- lapply(file.locs2, FUN = read.csv,
                        header = T, stringsAsFactors = F)
  for (i in seq(1:length(detect.list))){
    names(detect.list[[i]]) <- c('date.utc', 'receiver', 'transmitter',
                                 'drop1', 'drop2', 'drop3', 'drop4',
                                 'station', 'lat', 'long')
  }
  
  # Make list into data frame, drop emtpy columns, convert UTC to EST/EDT,
  # pull out transmitter ID
  detects <- do.call(rbind.data.frame, detect.list)
  detects <- detects[,c(1:3, 8, 9:10)]
  detects$date.utc <- ymd_hms(detects$date.utc)
  detects$date.local <- with_tz(detects$date.utc, tz = "America/New_York")
  detects$trans.num <- as.numeric(sapply(strsplit(detects[,3], '-'),'[[',3))
  
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
                   'Other'))))))))))))
  
  # False detections as determined by VEMCO
  false.det <- c('37119', '64288')
  
  detects <- unique(filter(detects, !trans.num %in% false.det))
  row.names(detects) <- NULL
  
  tbl_df(detects)
}
