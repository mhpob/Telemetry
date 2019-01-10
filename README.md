TelemetryR
=========
Aggregate, separate, and disperse detections.

## September 13, 2018
## Does `ACTsplit` not work? Change how your input dates are written.
An update to `dplyr` broke the ability to select by dates. Whereas you used to be able to use a number to filter by date:

```
ACTsplit('C:/Users/MYPCNAME/Documents/Vemco/Vue/ReceiverLogs',
         start = 20140401, end = 20140801)
```

You now need to use character dates in a standard unambigious format: "yyyy-mm-dd".

```
ACTsplit('C:/Users/MYPCNAME/Documents/Vemco/Vue/ReceiverLogs',
         start = '2014-04-01', end = '2014-08-01')
```

## June 12, 2018
## TelemetryR Update: Now with more speed!
Hi, All-
I pushed an update which allows the use of [parallel computing](http://gforge.se/2015/02/how-to-go-parallel-in-r-basics-tips/) when importing VUE csv files, a speed up when splitting detections to send out to ACT, and a more lightweight package.

Parallelization speeds things up a bit when there are a lot of detections, but probably won’t help if you’re only pulling in a file or two. It now takes almost half the time it usually does to use `vemsort` on our whole csv detection database (0.75 GB of data; 3.5 million detections). Note that there are now three new (optional) arguments: `cl` for parallel evaluation, `prog_bar` to see the progress of the import (with a little bit of overhead), and `creation_date` to download only the most-recently created files.

```
devtools::install_github('mhpob/TelemetryR')
```

### Normal way, with progress bar:

```
system.time(
  TelemetryR::vemsort('p:/obrien/biotelemetry/detections', prog_bar = T)
)
```
```
## Reading files...
## Binding files...
## Final data manipulation...
##    user  system elapsed 
##   27.07    5.52  114.66
```

### Parallelized, without progress bar:
```
cl <- parallel::makeCluster(parallel::detectCores() - 1)
system.time(
  TelemetryR::vemsort('p:/obrien/biotelemetry/detections', prog_bar = F,
                      clust = cl)
)
```
```
## Reading files...
## Binding files...
## Final data manipulation...
##    user  system elapsed 
##   11.71    3.55   67.97
```
```
parallel::stopCluster(cl)
```

### Selecting the most-recent detections:
You can also now select files that were created after a certain date. This becomes useful if you’re only trying to analyze or send out the most-recent data–you don’t even have to touch the other stuff.

```
system.time(
  TelemetryR::vemsort('p:/obrien/biotelemetry/detections',
                      creation_date = '2018-01-01')
)
```
```
## Reading files...
## Binding files...
## Final data manipulation...
##    user  system elapsed 
##    2.07    2.11   16.28
```

### Faster ACT-splitting:
`ACTsplit` has been redone with more-efficient loops and an increase in writing speed by using data.table::fwrite over base::write.csv. You can also pass on the parallelization/subsetting arguments to vemsort.
```
TelemetryR::ACTsplit('p:/obrien/biotelemetry/detections',
                     ACTtrans = 'p:/obrien/randomr/actactive.rda',
                     creation_date = '2018-01-01')
```
### Lighter package
The dependencies on `ggplot2` and `pbapply` have been removed, meaning that installing/updating the package should be quicker. That being said, you will have to have to install the `pbapply` package if you want to see a progress bar.

I hope you find this useful! As always, please feel free to contact me with any questions.
