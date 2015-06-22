ACTtrans <- read.csv('data-raw/all transmitters 6-16-15.csv',
                     stringsAsFactors = F)

devtools::use_data(ACTtrans, overwrite = T)

# On June 16, ACT prepared a combined document. Keeping this code here in case
# this doesn't happen in the future.
#
# active <- read.csv('data-raw/Active transmitters 6-16-15.csv', header = T,
#                      stringsAsFactors = F)
# active$Status <- 'Active'
#
# # Note: "Archived" sheet usually has "collaborators" column missing.
# archived <- read.csv('data-raw/Archived transmitters 6-16-15.csv', header = T,
#                      stringsAsFactors = F)
# archived$Status <- 'Archived'
#
# unknown <- read.csv('data-raw/Unknown status 6-16-15.csv', header = T,
#                      stringsAsFactors = F)
# unknown$Status <- 'Unknown'
#
# ACTtrans <- rbind(active, archived, unknown)
