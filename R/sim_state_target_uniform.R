#' Simulates a uniform state of the target
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param abundance a number indicating the abundance of the target in each cell
#' @return An updated simulation object with the newly calculated state of the target in the correct slot
#' @examples
#' \dontrun{
#' sim_state_target_uniform(simulation_object, 42)
#' }
sim_state_target_uniform <- function(simulation_object, abundance = 10) {
  background <- simulation_object@background

  sim_state <- background[[1]]
  terra::values(sim_state) <- abundance
  names(sim_state) <- "abundance"

  simulation_object@state_target <- sim_state

  # Return the updated simulation_object
  return(simulation_object)
}
