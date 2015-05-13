library(gdistance); library(rgdal); library(dplyr)
library(geoR); library(SpatialTools)

# test.data <- read.csv('c:/users/secor lab/downloads/2005_spring_seasonal_water quality data_cbp.csv')
# coordinates <- test.data[,c(2,1)]
# data <- test.data$SALINITY
# res <- 2


CBinterp <- function(data, coordinates, res){
  if(is.data.frame(data) == F) data <- as.data.frame(data)

  water_qual <- SpatialPointsDataFrame(coords = coordinates,
                                       data = data,
                                       proj4string = CRS('+proj=longlat'))
  water_qual <- spTransform(water_qual,
                            CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
  water_qual@data <- water_qual@data %>%
    mutate(reasting = res * round(water_qual@coords[, 1] / res),
           rnorthing = res * round(water_qual@coords[, 2] / res)) %>%
    group_by(reasting, rnorthing) %>%
    summarize(median = median(data)) %>%
    as.data.frame()
  water_qual@coords <- as.matrix(water_qual@data[, c('reasting','rnorthing')])

  ## Make grid to interpolate over, make into spatial object with same projection
  ## as shapefile. Use SpatialPixels to more-easily convert to raster later.
  grid <- expand.grid(
    seq(min(water_qual@data$reasting), max(water_qual@data$reasting), res),
    seq(min(water_qual@data$rnorthing), max(water_qual@data$rnorthing), res))
  grid <- SpatialPixelsDataFrame(points = grid[, 1:2], data = grid,
                            tolerance = 0.99,
                            proj4string =
                              CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
  ## Select points that lie within water and label them as such.
  grid_water <- grid[chesapeake,]
  grid_water$water <- 'YES'

  ## Label whether the points are within water, make it harder to interpolate over
  ## land (i.e., conductance of near-0 (land) v near-1 (water)).
  grid@data <- grid@data %>%
    left_join(grid_water@data) %>%
    mutate(water = ifelse(is.na(water), 'NO', water),
           cond = ifelse(water == 'NO', 0.01, 0.99)) %>%
    select(cond)
  ## Create a raster of data
  ches.ras <- raster(grid, layer = 1)

  ## Create transition matrix that represets a pairwise product of
  ## cells' conductance
  ches.trans <- transition(ches.ras, function(x) x[1] * x[2], 8)
  ches.trans <- geoCorrection(ches.trans)

  dist <- costDistance(ches.trans, water_qual)
  dist <- as.matrix(as.dist(dist, diag = T, upper = T))

  ### Interpolate ----------------------------------------------------------------

  # Use transition matrix to calculate corrected distances (as the fish swim)
  pred.dist <- costDistance(ches.trans, grid_water)
  pred.dist <- as.matrix(as.dist(pred.dist, diag = TRUE, upper = TRUE))

  # Calculate distances between observed and predicted values
  op.dist <- costDistance(ches.trans, water_qual, grid_water)
  op.dist <- as.matrix(op.dist, diag = TRUE, upper = TRUE)


  # Fit geostatistical model for the temp data
  vg <- variog(coords = water_qual@data[, c('reasting','rnorthing')],
               data = water_qual@data[, c('reasting','rnorthing', 'median')],
               max.dis = 600, dists.mat = dist)
  # ML fit
  vpar <- maxlik.cov.sp(as.matrix(cbind(1,
              water_qual@data[, c('reasting','rnorthing')])),
              water_qual@data[, 'median'],
              coords = as.matrix(water_qual@data[, c('reasting','rnorthing')]),
              sp.type = "matern", range.par = 600, error.ratio = 0.5,
              D = dist, reml = T)

  # Define spatial structure of prediction matrix from fitted spatial model
  V0 <- cov.sp(coords = as.matrix(water_qual@data[, c('reasting','rnorthing')]),
               sp.type = "matern", sp.par = vpar$sp.par,
               error.var = vpar$error.var, smoothness = vpar$smoothness,
               finescale.var = 0,
               pcoords = as.matrix(grid_water@data[,1:2]),
               D = dist, Dp = pred.dist, Dop = op.dist)

  # Apply spatial structure nd model to predict values
  krige <- krige.uk(water_qual@data[, 'median'],
                    V = V0$V, Vop = V0$Vop, Vp = V0$Vp,
                    X = as.matrix(cbind(1,
                              water_qual@data[, c('reasting','rnorthing')])),
                    Xp = as.matrix(cbind(1, grid_water@data[,1:2])), nsim = 0)
  grid_water[['value']] <- krige$pred
  grid_water[['se']] <- krige$mspe
  grid_water <- spTransform(grid_water, CRS('+proj=longlat'))
  grid_water@data <- cbind(grid_water@coords,
                           grid_water@data[, c('value', 'se')])
  grid_water
}
