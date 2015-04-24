active <- read.csv('data-raw/Active transmitters 4-15-15.csv', header = T,
                     stringsAsFactors = F)
active$Status <- 'Active'
archived <- read.csv('data-raw/Archived transmitters 3-24-15.csv', header = T,
                     stringsAsFactors = F)
archived$Status <- 'Archived'
unknown <- read.csv('data-raw/Unknown status 3-24-15.csv', header = T,
                     stringsAsFactors = F)
unknown$Status <- 'Unknown'
ACTtrans <- rbind(active, archived, unknown)

devtools::use_data(ACTtrans, overwrite = T)
