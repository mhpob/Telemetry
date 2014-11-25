#' Create Google Earth circles around a Lat/Long point
#' 
#' \code{GEcircle} creates approximate circles in KML to be opened in Google
#' Earth
#' 
#' This function uses degree, minute, second format of latitude/longitude points
#' to allow copy/paste of points from Google Earth. Make sure the degree and
#' prime symbols are removed and replaced with spaces before running, as in
#' the example.
#' 
#' @param lat String. Vector of latitudes.
#' @param long String. Vector of corresponding longitudes
#' @param radius Numeric. Radius, of desired circle in meters.
#' @param color Vector of any of the three kinds of R color specifications.
#'    Determines the circle's color; value is passed onto
#'    \code{\link[grDevices]{col2rgb}}.
#' @param west Logical. Are the points in the western hemisphere? If values for
#'    \code{long} are already negative, this should be \code{F}.
#' @return This functions will write KML files to your working directory.
#' @export
#' @seealso \code{\link{ptcirc}}
#' @examples
#' buoys <- data.frame(rbind(c('38 19 1.71', '76 27 4.07'),
#'                           c('38 18 40.44', '76 18 40.81'),
#'                           c('38 18 56.44', '76 17 3.80'),
#'                           c('38 18 27.13', '76 19 55.57')),
#'                     stringsAsFactors = F)
#' GEcircle(buoys[,1], buoys[,2], radius = 900, color = 'red', west = T)

GEcircle <- function (lat, long, radius, color, west = T){
  DD <- function (x, western = F, long = F){
    d <- as.numeric(substr(x, 1, 2))
    m <- as.numeric(substr(x, 4, 5)) / 60
    s <- as.numeric(substr(x, 7, 11)) / 3600
    
    if(western == T & long == T) -d - m - s
    else d + m + s
  }
  
  dmslat <- DD(lat) 
  dmslong <- DD(long, western = west, long = T)
  
  paths <- TelemetryR::ptcirc(data.frame(cbind(dmslong, dmslat)), radius)
  paths <- split(paths, paths$circle)
  paths <- lapply(paths, function(x){x['circle'] <- 0; x})
  
  # Converts color to KML color code
  circ.col <- col2rgb(color)
  circ.col <- rgb(circ.col[1], circ.col[2], circ.col[3], 255,
                  maxColorValue = 255)
  circ.col <- paste0(substr(circ.col, 8, 9), substr(circ.col, 6, 7),
                     substr(circ.col, 4, 5), substr(circ.col, 2, 3))
  
  # Writes KML files to your working directory
  for(i in seq(1,length(paths))){
    cat(paste0('<?xml version="1.0" encoding="UTF-8"?> <kml xmlns="http://earth.google.com/kml/2.1"> <Placemark> <name>',
               names(paths)[i],
               '</name> <Style> <geomColor>',
               circ.col,
               '</geomColor> </Style> <LineString> <coordinates>'),
        file = paste0(names(paths)[i],'.kml'))
    write.table(paths[[i]], row.names = F, col.names = F, sep = ',',
                file = paste0(names(paths)[i],'.kml'), append = T)
    cat('</coordinates> </LineString> </Placemark> </kml>',
        file = paste0(names(paths)[i],'.kml'), append = T)
  }
}