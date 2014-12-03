ACTtrans <- read.csv('data-raw/Active transmitters 11-24-14.csv', header = T,
                     stringsAsFactors = F)
devtools::use_data(ACTtrans, overwrite = T)
