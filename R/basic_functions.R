state_env_gradient <- function(simulation_object, from = 0, to = 1) {
  #create the gradient spatraster
  background <- simulation_object@background
  sim_state <- background[[1]]
  terra::values(sim_state) <- rep(seq(from = from, to = to, length.out = dim(background)[2]), dim(background)[1])
  names(sim_state) <- "env"
  sim_state
}

state_env_uniform <- function(simulation_object, value = 1) {
  #create the gradient spatraster
  background <- simulation_object@background
  sim_state <- background[[1]]
  terra::values(sim_state) <- value
  names(sim_state) <- "env"
  sim_state
}


state_target_suitability_uniform <- function(simulation_object,value = 0.5,n_targets=1){
  background <- simulation_object@background

  sim_state <- rep(background[[1]],n_targets)
  for(i in 1:n_targets){
    terra::values(sim_state[[i]]) <- value
  }
  names(sim_state) <- paste0("target_",1:n_targets)
  sim_state
}

state_target_suitability_virtsp <- function(simulation_object, n_targets = 1, params = NULL) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

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


state_target_realise_binomial <- function(simulation_object){
  state_target <- binary_state_target <- simulation_object@state_target_suitability
  for (i in 1:dim(state_target)[3]){
    # Get the probability values from the state target
    prob_values <- terra::values(state_target[[i]])

    # Simulate binary values from the binomial distribution based on the probability values
    binary_values <- rbinom(length(prob_values), 1, prob_values)

    terra::values(binary_state_target[[i]]) <- binary_values
  }
  binary_state_target
}


state_target_realise_threshold <- function(simulation_object,threshold){
  state_target <- binary_state_target <- simulation_object@state_target_suitability
  for (i in 1:dim(state_target)[3]){
    # Get the probability values from the state target
    prob_values <- terra::values(state_target[[i]])

    # Simulate binary values from the binomial distribution based on the probability values
    binary_values <- as.numeric(prob_values >= threshold)

    terra::values(binary_state_target[[i]]) <- binary_values
  }
  binary_state_target
}
