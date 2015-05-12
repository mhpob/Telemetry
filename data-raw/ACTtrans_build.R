active <- read.csv('data-raw/Active transmitters 5-8-15.csv', header = T,
                     stringsAsFactors = F)
active$Status <- 'Active'

# Note: "Archived" sheet usually has "collaborators" column missing.
archived <- read.csv('data-raw/Archived transmitters 5-8-15.csv', header = T,
                     stringsAsFactors = F)
archived$Status <- 'Archived'

unknown <- read.csv('data-raw/Unknown status 5-8-15.csv', header = T,
                     stringsAsFactors = F)
unknown$Status <- 'Unknown'

ACTtrans <- rbind(active, archived, unknown)

devtools::use_data(ACTtrans, overwrite = T)
