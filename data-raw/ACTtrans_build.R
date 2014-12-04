active <- read.csv('data-raw/Active transmitters 12-4-14.csv', header = T,
                     stringsAsFactors = F)
active$Status <- 'Active'
archived <- read.csv('data-raw/Archived transmitters 12-4-14.csv', header = T,
                     stringsAsFactors = F)
archived$Status <- 'Archived'
unknown <- read.csv('data-raw/Unknown status 12-3-14.csv', header = T,
                     stringsAsFactors = F)
unknown$Status <- 'Unknown'
ACTtrans <- rbind(active, archived, unknown)

devtools::use_data(ACTtrans, overwrite = T)