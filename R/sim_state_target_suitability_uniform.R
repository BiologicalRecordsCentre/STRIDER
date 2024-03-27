#' Simulates a uniform state of the target
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param value a number indicating the value of the target in each cell
#' @param n_targets how many targets to generate
#' @return An updated simulation object with the newly calculated state of the target in the correct slot
#' @examples
#' \dontrun{
#' sim_state_target_suitability_uniform(simulation_object, 0.5)
#' }
sim_state_target_suitability_uniform <- function(simulation_object, value = 0.5,n_targets=1) {
  background <- simulation_object@background

  sim_state <- rep(background[[1]],n_targets)
  for(i in 1:n_targets){
    terra::values(sim_state[[i]]) <- value

  }

  names(sim_state) <- paste0("target_",1:n_targets)
  simulation_object@state_target_suitability <- sim_state

  # Return the updated simulation_object
  return(simulation_object)
}
