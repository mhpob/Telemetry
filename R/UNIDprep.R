# library(TelemetryR)
unids <- TelemetryR::ACTsplit('p:/obrien/biotelemetry/detections/cbl pier', 'p:/obrien/randomr/actactive.rda', write = F)
out <- 'p:/obrien/biotelemetry/detections/cbl pier'
directory <- 'p:/obrien/biotelemetry/detections/cbl pier'
UNIDprep <- function(unids, directory = getwd(), out = getwd()){
  Unks <- data.frame(Transmitters = unique(unids$transmitter))

  # List VRL and VRL-RLD files
  vrls <- gsub('csv', 'vrl',
                   unique(unids$file))
  vrl_rlds <- strsplit(vrls, '_')
  vrl_rld_front <- lapply(vrl_rlds, function(x) paste0(x[1], '-RLD'))
  vrl_rld_back <- lapply(vrl_rlds, function(x) {
    paste(unlist(x[2:length(x)]), collapse = '_')
    })
  vrl_rlds <- paste(vrl_rld_front, vrl_rld_back, sep = '_')
  vrl_all <- c(vrls, vrl_rlds)

  # Find location of files within directory
  vrl_locs <- unlist(
    lapply(vrl_all, function(x) list.files(path = directory, pattern = x,
                                           recursive = T, full.names = T))
    )

  # Output UNID list, copy VRL and VRL-RLDs into new folder.
  output_location <- file.path(out, 'Unknown for VEMCO')
  dir.create(output_location)
  write.csv(Unks, file.path(output_location, 'unknown_ids.csv'),
            row.names = F)

  file.copy(from = vrl_locs,
            to = file.path(output_location, vrl_all))

}
