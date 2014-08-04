source('vemsort.R')

detects <- vemsort('p:/obrien/biotelemetry/detections')
act <- read.csv('p:/obrien/biotelemetry/detections/active transmitters 7-22-14.csv',
                header = T, stringsAsFactors = F)

# Filter for ID'ed detections that aren't Dave's
id <- filter(act, Tag.ID.Code.Standard %in% detects$transmitter,
                  !Tag.ID.Code.Standard %in%
                    paste('A69-1601', seq(25434, 25505), sep = '-'))

id <- merge(detects, id[, c(1:2, 14:15)],
           by.x = c('transmitter', 'trans.num'),
           by.y = c('Tag.ID.Code.Standard', 'ID.Standard'))

unid <- filter(detects, 
               transmitter %in% setdiff(detects$transmitter, act$Tag.ID.Code.Standard))

j <- split(id, id$Primary.Researcher)


stdate <- ymd(20140605)
enddate <- ymd(20140804) + days(1)

for(i in seq(length(j))){
j[[i]] <- j[[i]][c(1,3:7)]
j[[i]][7:10] <- NA
j[[i]] <- j[[i]][c(3, 2, 1, 7:10, 4:6)]
names(j[[i]]) <- c('Date and Time (UTC)', 'Receiver', 'Transmitter', 'Transmitter Name',
                   'Transmitter Serial', 'Sensor Value', 'Sensor Unit', 'Station Name',
                   'Latitude', 'Longitude')
j[[i]] <- j[[i]][order(j[[i]][3], j[[i]][1]),]
j[[i]] <- j[[i]][j[[i]][,1] >= stdate & j[[i]][,1] <= enddate,]
write.csv(j[[i]], file = paste(gsub(' ', '', names(j[i])), Sys.Date(),'.csv', 
                               sep = ''), row.names = F)
}
