## R function to perform spatial join on raster and polygon files
## This function outputs pixel values of individual points within a polygon area 
## Author: Daljit Singh (singhdj2@ksu.edu) with inputs from Jan and Matej @KSU
## Created: 11-2017
## Updated: 02-2018

# load required packages
library(pacman)
p_load(sp,raster,rgdal,parallel)


## Usage example: call this function in your windows machine as 
#tip: run detectCores() first to find out total cores available in your machine
#p17 <- joinRastPoints(rastLayerPath, polyLayerPath, numCores = 10)  


## get number of cores available
# numCores <- detectCores() - 1
#numCores <- 40

joinRastPoints <- function(rastLayerPath, polyLayerPath, numCores) {
  starttime <- Sys.time()
  
  ## set cores to use
  #no_cores <- detectCores()
  no_cores <- numCores
  ## Batch polygons for lower overhead
  batchby <- 20
  
  # load the input data
  r <- raster(rastLayerPath, xy=T,na.rm=T)
  if (class(r) != 'RasterLayer') {
    stop("Object of class 'RasterLayer' expected for 'Raster'")
  }
  
  polys <- readOGR(polyLayerPath)
  if (proj4string(polys) != proj4string(r)) {
    stop("Geographical projections of layers 'polygons' and 'raster' must match")
  }
  
  cat("\n")
  cat("Good News: Geographical Projection Strings match for Raster and Polygon\n")
  cat("Now preparing to perform 'spatial join' operation\n")
  cat("Note: This process may take a few minutes to finish\n")
  cat("\n")
  
  # crop the image to minimal bounding box of all polygons
  r <- crop(r, polys)
  
  # Initiate parallel cluster
  cl <- makeCluster(no_cores, output="")
  # load required packages for each worker
  clusterExport(cl=cl, varlist=c("r", "polys", "batchby"), envir=environment())
  clusterEvalQ(cl, library(sp))
  clusterEvalQ(cl, library(raster))
  
  # for each polygon, create their spatial join dataframe and join them with rbind
  pts.poly = do.call(rbind, parLapply(cl, 1:(nrow(polys)/batchby), function (n2) {
    do.call(rbind, lapply((n2*batchby-batchby+1):(n2*batchby), function(n) {
      p <- polys[n,]
      # crop raster just for the bounding box of polygon
      pts <- rasterToPoints(crop(r, p), spatial=TRUE)
      # omit lines not in polygon
      na.omit(data.frame(pts@data, over(pts, p)))
    }))
  }))
  
  stopCluster(cl)
  cat("Time taken in joinRastPoints:", as.numeric(Sys.time() - starttime, units="secs"), "s\n")
  
  # output spatialdataFrame object
  pts.poly
}


## end




