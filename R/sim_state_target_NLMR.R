#' Simulates a state of the target using a wrapper around sim_state_env_NLMR
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param alg single or list of algorithms as character
#' @param params list of vectors providing parameters to be passed to each NMLR function call
#' @param abundance Abundance of species
#' @return An updated simulation object with the newly calculated state of the target in the correct slot
sim_state_state_NLMR <- function(simulation_object, alg, params = NULL, abundance = 1) {
  background <- simulation_object@background
  state_env <- simulation_object@state_env

  sim_state <- sim_state_env_NLMR(background = background, alg = alg, params = params) * abundance

  names(sim_state) <- "abundance"
  simulation_object@state_target <- sim_state

  # Return the updated simulation_object
  return(simulation_object)
}
