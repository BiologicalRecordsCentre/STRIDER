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



effort_uniform <- function(simulation_object, n_samplers = 1, n_visits = 1, n_sample_units=1, replace = FALSE) {

  #which cells are visited
  state_target <- simulation_object@state_target_suitability
  visited_cells <- rep(sample(terra::cells(state_target), size = n_samplers*n_visits, replace = replace),each = n_sample_units)

  # capture data
  sim_effort_points <- as.data.frame(terra::xyFromCell(state_target, visited_cells))
  sim_effort_points$sampler <- rep(1:n_samplers,each = n_visits*n_sample_units)
  sim_effort_points$visit <- rep(1:n_visits,n_samplers,each = n_sample_units)
  sim_effort_points$unit <- rep(1:n_sample_units,n_samplers*n_visits)

  sim_effort_points$cell_id <- visited_cells

  effort_sf <- sf::st_as_sf(sim_effort_points, coords = c("x", "y"), crs = terra::crs(state_target))

  #get values from env, suitability, realised
  extracted_values <- terra::extract(simulation_object@state_env,effort_sf$cell_id)
  effort_sf[,names(extracted_values)] <- extracted_values
  extracted_values <- terra::extract(simulation_object@state_target_suitability,effort_sf$cell_id)
  effort_sf[,paste0("suit_",names(extracted_values))] <- extracted_values
  extracted_values <- terra::extract(simulation_object@state_target_realised,effort_sf$cell_id)
  effort_sf[,paste0("real_",names(extracted_values))] <- extracted_values

  effort_sf

}




detect_equal <- function(simulation_object, prob = 0.5) {

  background <- simulation_object@background
  state_env <- simulation_object@state_env
  state_target <- simulation_object@state_target_realised
  effort <- simulation_object@effort

  detections_all <- data.frame()

  #how many targets states are there?
  if(length(dim(state_target))<3){
    n_targets <- 1
  } else {
    n_targets <- dim(state_target)[3]
  }

  #loop through each of the targets
  for (i in 1:n_targets) {
    detections <- effort
    detections$target <- i

    detections$state_realised <- unname(terra::extract(state_target[[i]], effort,ID=F,raw=T))

    #detect based probability value provided as argument
    detections$detected <- detections$state_realised * (runif(nrow(detections)) < prob)

    #in this basic example all are identified correctly
    detections$identified_as <- detections$target
    detections$identified_correct <- detections$identified_as==detections$target

    detections_all <- rbind(detections_all, detections)
  }

  # Update simulation_object with the new results
  detections_all
}
