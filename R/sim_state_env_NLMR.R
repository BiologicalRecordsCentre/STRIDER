#' Simulates a state of the environment using a wrapper around NLMR functions
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param alg single or list of algorithms as character
#' @param params list of vectors providing parameters to be passed to each NMLR function call
#' @return An updated simulation object with the newly calculated state of the environment in the correct slot
#' @examples
#' \dontrun{
#' sim_state_env_NLMR(simulation_object, "random")
#' }
sim_state_env_NLMR <- function(simulation_object, alg, params = NULL) {

  background <- simulation_object@background

  #check if NLMR is installed and available
  if ("NLMR" %in% installed.packages()[,"Package"]) {
    pkg_data <- installed.packages()[installed.packages()[,"Package"]=="NLMR",]
    message(paste0("NLMR version ", pkg_data["Version"], " is installed and will be loaded"))
    library(NLMR)
  } else {
    stop("Attempting to use sim_state_env_NLMR() which requires that {NLMR} is installed, but NLMR isn't installed. Please install NLMR.")
  }

  if (sum(alg %in% c("curds")) > 0) {
    stop(paste0("Curds algorithm not implemented correctly yet"))
  }

  if (is.null(params)) {
    params <- rep(list(NA), length(alg))
  }

  #get nrows and ncols from background
  nrows <- nrow(background)
  ncols <- ncol(background)

  layers <- list()

  #loop through the layers
  for (i in 1:length(alg)) {
    a <- alg[i]
    fnc_name <- paste0("nlm_", a)

    param <- as.list(params[[i]])
    #add ncol+nrow
    if (!("ncol" %in% names(param))) {
      param$ncol <- ncols
    }
    if (!("nrow" %in% names(param))) {
      param$nrow <- nrows
    }

    param <- param[!is.na(param)]

    layer <- do.call(what = fnc_name, args = param)

    layer <- terra::rast(layer) # turn into spat raster
    layer <- terra::resample(layer, background) #resize
    names(layer) <- a
    layers[[i]] <- layer
  }

  layers <- terra::rast(layers)
  names(layers) <- paste0(names(layers), "_", 1:length(names(layers)))

  terra::crs(layers) <- terra::crs(background)

  #unload package
  detach("package:NLMR", unload = TRUE)

  simulation_object@state_env <- layers

  # Return the updated simulation_object
  return(simulation_object)
}
