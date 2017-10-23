# library(TelemetryR)
unids <- TelemetryR::ACTsplit('p:/obrien/biotelemetry/detections/cbl pier', 'p:/obrien/randomr/actactive.rda', write = F)
out <- 'p:/obrien/biotelemetry/detections/cbl pier'
directory <- 'p:/obrien/biotelemetry/detections/cbl pier'
UNIDprep <- function(unids, directory = getwd(), out = getwd()){
  Unks <- data.frame(Transmitters = unique(unids$transmitter))

  vrls <- gsub('csv', 'vrl',
                   unique(unids$file))
  vrl_rlds <- strsplit(vrls, '_')
  vrl_rld_front <- lapply(vrl_rlds, function(x) paste0(x[1], '-RLD'))
  vrl_rld_back <- lapply(vrl_rlds, function(x) {
    paste(unlist(x[2:length(x)]), collapse = '_')
    })
  vrl_rlds <- paste(vrl_rld_front, vrl_rld_back, sep = '_')
  vrl_all <- c(vrls, vrl_rlds)

  output_location <- file.path(out, 'Unknown for VEMCO')
  dir.create(output_location)
  write.csv(Unks, file.path(output_location, 'unknown_ids.csv'),
            row.names = F)

  #works for one folder, will run into recursive issues
  file.copy(from = file.path(directory, vrl_all),
            to = file.path(output_location, vrl_all))

}
