#' Simulates a uniform state of the environment
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param value the value for the SpatRaster
#' @return An updated simulation object with the newly calculated state of the environment in the correct slot
#' @examples
#' \dontrun{
#' sim_state_env_uniform(simulation_object, 0)
#' }
sim_state_env_uniform <- function(simulation_object, value = 0) {
  simulation_object_original <- simulation_object_original <- simulation_object <- read_sim_obj_rasters(simulation_object)
  background <- simulation_object@background

  sim_state <- background[[1]]
  terra::values(sim_state) <- value
  names(sim_state) <- "env"

  simulation_object_original@state_env <- sim_state

  # Return the updated simulation_object
  return(simulation_object_original)
}
