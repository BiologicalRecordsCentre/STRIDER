#' Simulates detection / identification where all detections occur at an equal probability and identified correctly
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param prob a numeric probability of each target being detected
#' @return A simple feature collection with geometry type POINTs
#' @examples
#' \dontrun{
#' sim_detect_equal()
#' }
sim_detect_equal <- function(simulation_object, prob = 0.5) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

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
  simulation_object_original@detect <- detections_all

  # Return the updated simulation_object
  simulation_object_original
}
