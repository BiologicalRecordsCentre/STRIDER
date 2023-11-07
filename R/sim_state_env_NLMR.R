#' Simulates a state of the environment using a wrapper around NLMR functions
#'
#' @param background a SpatRaster from which the extent and resolution will be used
#' @param alg single or list of algorithms as character
#' @param params list of vectors providing parameters to be passed to each NMLR function call
#' @return A SpatRaster with extent and resolution of background but values  describing the environment
#' @examples
#' \dontrun{
#' sim_state_env_NLMR(terra::rast(nrows=100,ncols=100),"random")
#' }
sim_state_env_NLMR <- function(background,alg,params=NULL){

  #check if NLMR is installed and available
  if("NLMR" %in% installed.packages()[,"Package"]){
    pkg_data <- installed.packages()[installed.packages()[,"Package"]=="NLMR",]
    message(paste0("NLMR version ",pkg_data["Version"]," is installed and will be loaded"))
    library(NLMR)
  } else {
    stop("Attempting to use sim_state_env_NLMR() which requires that {NLMR} is installed, but NLMR isn't installed. Please install NLMR.")
  }

  if(sum(alg %in% c("curds"))>0){
    stop(paste0("Curds algorithm not implemented correctly yet"))
  }

  if (is.null(params)){
    params <- rep(list(NA),length(alg))
  }

  #get nrows and ncols from background
  nrows <- nrow(background)
  ncols <- ncol(background)

  layers <- list()

  #loop through the layers
  for (i in 1:length(alg)){
    a <- alg[i]
    fnc_name <- paste0("nlm_",a)

    param <- as.list(params[[i]])
    #add ncol+nrow
    if(!("ncol" %in% names(param))){
      param$ncol<-ncols
    }
    if(!("nrow" %in% names(param))){
      param$nrow<-nrows
    }

    param <- param[!is.na(param)]

    layer <- do.call(what = fnc_name, args = param)

    layer <- terra::rast(layer) # turn into spat raster
    layer <- terra::resample(layer,background) #resize
    names(layer) <- a
    layers[[i]] <- layer
  }

  layers <- terra::rast(layers)
  names(layers) <- paste0(names(layers),"_",1:length(names(layers)))

  terra::crs(layers) <- terra::crs(background)

  #unload package
  detach("package:NLMR", unload = TRUE)

  layers
}
