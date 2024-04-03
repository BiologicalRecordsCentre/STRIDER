#' Simulates a state of the target using a wrapper around virtualspecies functions
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param n_targets numeric for the number of targets to generate
#' @param params list of vectors providing parameters to be passed to each NMLR function call
#' @return An updated simulation object with the newly calculated state of the target in the correct slot
sim_state_target_suitability_virtualspecies <- function(simulation_object, n_targets = 1, params = NULL) {
  simulation_object_original <- simulation_object <- read_sim_obj_rasters(simulation_object)

  background <- simulation_object@background
  environment <- simulation_object@state_env

  #check if virtualspecies is installed and available
  if ("virtualspecies" %in% installed.packages()[,"Package"]) {
    pkg_data <- installed.packages()[installed.packages()[,"Package"]=="virtualspecies",]
    message(paste0("virtualspecies version ", pkg_data["Version"], " is installed and will be loaded"))
    library(virtualspecies)
  } else {
    stop("Attempting to use sim_state_target_suitability_virtualspecies() which requires that {virtualspecies} is installed, but virtualspecies isn't installed. Please install virtualspecies.")
  }

  if (is.null(params)) {
    params <- rep(list(NA), n_targets)
  }

  layers <- list()

  for (i in 1:n_targets) {
    param <- as.list(params[[i]])
    param$raster.stack <- raster::raster(environment)
    param$plot <- FALSE
    param <- param[!is.na(param)]

    layer <- do.call("generateRandomSp", param)

    layers[[i]] <- layer$suitab.raster
  }

  # Convert to spatraster
  layers <- terra::rast(layers)

  # Set the CRS
  terra::crs(layers) <- terra::crs(background)
  names(layers) <- paste0("target_", 1:n_targets)

  # Unload the package
  detach("package:virtualspecies", unload = TRUE)

  simulation_object_original@state_target_suitability <- layers

  # Return the updated simulation_object
  return(simulation_object_original)
}
