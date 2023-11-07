#' Simulates a state of the target using a wrapper around virtualspecies functions
#'
#' @param background a SpatRaster from which the extent and resolution will be used
#' @param environment a SpatRaster from which the extent and resolution will be used
#' @param params list of vectors providing parameters to be passed to each NMLR function call
#' @return A SpatRaster with extent and resolution of background but values  describing the environment
sim_state_target_virtualspecies <- function(background,environment,n_targets=1,prop_env=1,params=NULL){

  #check if NLMR is installed and available
  if("virtualspecies" %in% installed.packages()[,"Package"]){
    pkg_data <- installed.packages()[installed.packages()[,"Package"]=="virtualspecies",]
    message(paste0("virtualspecies version ",pkg_data["Version"]," is installed and will be loaded"))
    library(virtualspecies)
  } else {
    stop("Attempting to use sim_state_target_virtualspecies() which requires that {virtualspecies} is installed, but virtualspecies isn't installed. Please install virtualspecies.")
  }


  if (is.null(params)){
    params <- rep(list(NA),n_targets)
  }

  layers <- list()

  for (i in 1:n_targets){

    param <- as.list(params[[i]])
    param$raster.stack <- raster::raster(environment)
    param$plot<-F
    param <- param[!is.na(param)]

    layer <- do.call("generateRandomSp", param)

    layers[[i]] <- terra::rast(layer$suitab.raster)
  }

  #turn into spatraster
  layers <- terra::rast(layers)

  #get crs
  terra::crs(layers) <- terra::crs(background)
  names(layers) <- paste0("target_",1:n_targets)

  #unload package
  detach("package:virtualspecies", unload = TRUE)

  layers
}
