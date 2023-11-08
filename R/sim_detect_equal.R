#' Simulates detection where all detections occur at an equal probability and identified correctly
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param prob a numeric probability of each target being detected
#' @return A simple feature collection with geometry type POINTs
#' @examples
#' \dontrun{
#' sim_detect_equal()
#' }
sim_detect_equal <- function(simulation_object, prob = 1) {
  background <- simulation_object@background
  state_env <- simulation_object@state_env
  state_target <- simulation_object@state_target
  effort <- simulation_object@effort

  detections_all <- data.frame()

  for (i in 1:dim(state_target)[3]) {
    detections <- effort
    detections$state <- terra::extract(state_target[[i]], effort)$abundance
    detections$target <- i

    detections$detected <- runif(nrow(detections)) < prob
    detections$identified <- TRUE

    detections_all <- rbind(detections_all, detections)
  }

  # Update simulation_object with the new results
  simulation_object@detect <- detections_all

  # Return the updated simulation_object
  return(simulation_object)
}
