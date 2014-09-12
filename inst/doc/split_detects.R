## ------------------------------------------------------------------------
library(TelemetryR)
vemsort('p:/obrien/biotelemetry/detections')

## ------------------------------------------------------------------------
vemsort('p:/obrien/biotelemetry/detections', c('A69-1601-37119','A69-1601-64288'))

## ------------------------------------------------------------------------
# This assumes that the transmitter file is located at
# p:/obrien/biotelemetry/detections/active transmitters 9-2-14.csv
ACTsplit('p:/obrien/biotelemetry/detections')
# These are unidentified detections.

## ----, eval = F----------------------------------------------------------
#  ACTsplit('p:/obrien/biotelemetry/detections', ACT = 'p:/obrien/active transmitters 9-2-14.csv')

## ----, eval = F----------------------------------------------------------
#  # Splits detections from June 1 up to and including August 1, 2014
#  ACTsplit(directory = 'p:/obrien/biotelemetry/detections',
#           my.trans = paste0('A69-1601-',seq(25434,25505)),
#           false.det = c('A69-1601-37119','A69-1601-64288'),
#           start = 20140601, end = 20140801)

