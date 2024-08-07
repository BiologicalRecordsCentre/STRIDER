#' Create a Gradient SpatRaster for Environmental State
#'
#' @param simulation_object A SimulationObject containing the background layer.
#' @param from The starting value of the gradient. Default is 0.
#' @param to The ending value of the gradient. Default is 1.
#'
#' @return A SpatRaster representing the gradient environmental state.
#' @examples
#' \dontrun{
#' sim_state <- state_env_gradient(simulation_object, from = 0, to = 1)
#' }
#' @export
state_env_gradient <- function(simulation_object, from = 0, to = 1) {
  #create the gradient spatraster
  background <- simulation_object@background
  sim_state <- background[[1]]
  terra::values(sim_state) <- rep(seq(from = from, to = to, length.out = dim(background)[2]), dim(background)[1])
  names(sim_state) <- "env"
  sim_state
}

#' Create a Uniform SpatRaster for Environmental State
#'
#' @param simulation_object A SimulationObject containing the background layer.
#' @param value The value to fill the raster with. Default is 1.
#'
#' @return A SpatRaster representing the uniform environmental state.
#' @examples
#' \dontrun{
#' sim_state <- state_env_uniform(simulation_object, value = 1)
#' }
#' @export
state_env_uniform <- function(simulation_object, value = 1) {
  #create the gradient spatraster
  background <- simulation_object@background
  sim_state <- background[[1]]
  terra::values(sim_state) <- value
  names(sim_state) <- "env"
  sim_state
}

#' Create a Uniform Suitability SpatRaster for Target State
#'
#' @param simulation_object A SimulationObject containing the background layer.
#' @param value The value to fill the raster with. Default is 0.5.
#' @param n_targets The number of target layers. Default is 1.
#'
#' @return A SpatRaster representing the uniform suitability for target state.
#' @examples
#' \dontrun{
#' sim_state <- state_target_suitability_uniform(simulation_object, value = 0.5, n_targets = 2)
#' }
#' @export
state_target_suitability_uniform <- function(simulation_object,value = 0.5,n_targets=1){
  background <- simulation_object@background

  sim_state <- rep(background[[1]],n_targets)
  for(i in 1:n_targets){
    terra::values(sim_state[[i]]) <- value
  }
  names(sim_state) <- paste0("target_",1:n_targets)
  sim_state
}


#' Realize Target Suitability Using Binomial Distribution
#'
#' @param simulation_object A SimulationObject containing the target suitability layers.
#'
#' @return A SpatRaster with realized target states as binary values.
#' @examples
#' \dontrun{
#' binary_state <- state_target_realise_binomial(simulation_object)
#' }
#' @export
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

#' Realize Target Suitability Using Threshold
#'
#' @param simulation_object A SimulationObject containing the target suitability layers.
#' @param threshold A numeric value specifying the threshold for realization.
#'
#' @return A SpatRaster with realized target states as binary values based on the threshold.
#' @examples
#' \dontrun{
#' binary_state <- state_target_realise_threshold(simulation_object, threshold = 0.5)
#' }
#' @export
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


#' Generate Sampling Effort Points
#'
#' @param simulation_object A SimulationObject containing the target suitability layers.
#' @param n_samplers The number of samplers. Default is 1.
#' @param n_visits The number of visits per sampler. Default is 1.
#' @param n_sample_units The number of sample units per visit. Default is 1.
#' @param replace A logical value indicating whether sampling with replacement is allowed. Default is FALSE.
#' @param prob_raster A SpatRaster providing the probability of sampling each cell. Default is NULL.
#' @param ... Any other arguments to function
#'
#' @return An sf object containing the sampling effort points.
#' @examples
#' \dontrun{
#' effort <- effort_basic(simulation_object, n_samplers = 2, n_visits = 3)
#' }
#' @export
effort_basic <- function(simulation_object, n_samplers = 1, n_visits = 1, n_sample_units=1, replace = FALSE,prob_raster = NULL,...) {

  #which cells are visited
  state_target <- simulation_object@state_target_suitability

  if (is.null(prob_raster)){
    prob <- NULL
  } else {
    prob <- terra::values(prob_raster)
  }
  visited_cells <- rep(sample(terra::cells(state_target),
                              size = n_samplers*n_visits,
                              replace = replace,
                              prob = prob
                              ),
                       each = n_sample_units)

  # capture sampling meta data (who, when, etc.)
  sim_effort_points <- as.data.frame(terra::xyFromCell(state_target, visited_cells))
  sim_effort_points$sampler <- rep(1:n_samplers,each = n_visits*n_sample_units)
  sim_effort_points$visit <- rep(1:n_visits,n_samplers,each = n_sample_units)
  sim_effort_points$unit <- rep(1:n_sample_units,n_samplers*n_visits)

  sim_effort_points$cell_id <- visited_cells

  effort_sf <- sf::st_as_sf(sim_effort_points, coords = c("x", "y"), crs = terra::crs(state_target))

  effort_sf
}



#' Detect Presence/Absence Based on Equal Probability
#'
#' @param simulation_object A SimulationObject containing the realized target states and sampling effort points.
#' @param prob The detection probability. Default is 1.
#'
#' @return A data frame containing the detection results.
#' @examples
#' \dontrun{
#' detections <- detect_equal(simulation_object, prob = 0.8)
#' }
#' @export
detect_equal <- function(simulation_object, prob = 1) {
  #get objects
  detections <- simulation_object@effort


  #detect based probability value provided as argument
  detections$detection_ability <- runif(nrow(detections)) < prob
  #recorded presence/absense
  detections$state_detected <- detections$detection_ability * detections$target_realised
  #in this basic example all are identified correctly (ignore for now)
  # detections$identified_as <- detections$target
  # detections$identified_correct <- detections$identified_as==detections$target

  # Update simulation_object with the new results
  detections
}

#' Report Detections Based on Reporting Probability
#'
#' @param simulation_object A SimulationObject containing the detection results.
#' @param prob The reporting probability. Default is 1.
#' @param platform The platform used for reporting. Default is "None".
#'
#' @return A data frame containing the reporting results.
#' @examples
#' \dontrun{
#' reports <- report_equal(simulation_object, prob = 0.8, platform = "Online")
#' }
#' @export
report_equal <- function(simulation_object, prob = 1, platform = "None") {
  detect <- simulation_object@detect

  reports <- detect
  reports$reported <- runif(nrow(reports)) < prob
  reports$platform <- platform

  reports
}
