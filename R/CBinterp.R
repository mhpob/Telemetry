#' Interpolate variables over the Chesapeake Bay
#'
#'
#'

CBinterp <- function(data, coordinates, res){
  if(is.data.frame(data) == F) data <- as.data.frame(data)

  water_qual <- sp::SpatialPointsDataFrame(coords = coordinates,
                                       data = data,
                                       proj4string = sp::CRS('+proj=longlat'))
  water_qual <- sp::spTransform(water_qual,
                        sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))
  water_qual@data <- water_qual@data %>%
    dplyr::mutate(reasting = res * round(water_qual@coords[, 1] / res),
                  rnorthing = res * round(water_qual@coords[, 2] / res)) %>%
    dplyr::group_by(reasting, rnorthing) %>%
    dplyr::summarize(median = median(data)) %>%
    as.data.frame()
  water_qual@coords <- as.matrix(water_qual@data[, c('reasting','rnorthing')])

  # Make grid to interpolate over, make into spatial object with same
  # projection as shapefile. Use SpatialPixels to more-easily convert to
  # raster later.
  grid <- expand.grid(
    seq(min(water_qual@data$reasting), max(water_qual@data$reasting), res),
    seq(min(water_qual@data$rnorthing), max(water_qual@data$rnorthing), res))
  grid <- sp::SpatialPixelsDataFrame(points = grid[, 1:2], data = grid,
                        tolerance = 0.99,
                        proj4string =
                          sp::CRS('+proj=utm +zone=18 +datum=NAD83 +units=km'))

  # Select points that lie within water and label them as such.
  grid_water <- grid[chesapeake,]
  grid_water$water <- 'YES'

  # Label whether the points are within water, make it harder to interpolate over
  # land (i.e., conductance of near-0 (land) v near-1 (water)).
  grid@data <- grid@data %>%
    dplyr::left_join(grid_water@data) %>%
    dplyr::mutate(water = ifelse(is.na(water), 'NO', water),
                  cond = ifelse(water == 'NO', 0.01, 0.99)) %>%
    dplyr::select(cond)
  ## Create a raster of data
  ches.ras <- raster::raster(grid, layer = 1)

  ## Create transition matrix that represets a pairwise product of
  ## cells' conductance
  ches.trans <- gdistance::transition(ches.ras, function(x) x[1] * x[2], 8)
  ches.trans <- gdistance::geoCorrection(ches.trans)

  dist <- gdistance::costDistance(ches.trans, water_qual)
  dist <- as.matrix(as.dist(dist, diag = T, upper = T))

  ## Interpolation steps
  # Use transition matrix to calculate corrected distances (as the fish swim)
  pred.dist <- gdistance::costDistance(ches.trans, grid_water)
  pred.dist <- as.matrix(as.dist(pred.dist, diag = TRUE, upper = TRUE))

  # Calculate distances between observed and predicted values
  op.dist <- gdistance::costDistance(ches.trans, water_qual, grid_water)
  op.dist <- as.matrix(op.dist, diag = TRUE, upper = TRUE)


  # Fit geostatistical model for the temp data
  vg <- geoR::variog(coords = water_qual@data[, c('reasting','rnorthing')],
                 data = water_qual@data[, c('reasting','rnorthing', 'median')],
                 max.dis = 600, dists.mat = dist)
  # ML fit
  vpar <- SpatialTools::maxlik.cov.sp(as.matrix(cbind(1,
              water_qual@data[, c('reasting','rnorthing')])),
              water_qual@data[, 'median'],
              coords = as.matrix(water_qual@data[, c('reasting','rnorthing')]),
              sp.type = "matern", range.par = 600, error.ratio = 0.5,
              D = dist, reml = T)

  # Define spatial structure of prediction matrix from fitted spatial model
  V0 <- SpatialTools::cov.sp(coords = as.matrix(
                                water_qual@data[, c('reasting','rnorthing')]),
               sp.type = "matern", sp.par = vpar$sp.par,
               error.var = vpar$error.var, smoothness = vpar$smoothness,
               finescale.var = 0,
               pcoords = as.matrix(grid_water@data[,1:2]),
               D = dist, Dp = pred.dist, Dop = op.dist)

  # Apply spatial structure nd model to predict values
  krige <- SpatialTools::krige.uk(water_qual@data[, 'median'],
                    V = V0$V, Vop = V0$Vop, Vp = V0$Vp,
                    X = as.matrix(cbind(1,
                              water_qual@data[, c('reasting','rnorthing')])),
                    Xp = as.matrix(cbind(1, grid_water@data[,1:2])), nsim = 0)
  grid_water[['value']] <- krige$pred
  grid_water[['se']] <- krige$mspe
  grid_water <- sp::spTransform(grid_water, sp::CRS('+proj=longlat'))
  grid_water@data <- cbind(grid_water@coords,
                           grid_water@data[, c('value', 'se')])
  grid_water
}
