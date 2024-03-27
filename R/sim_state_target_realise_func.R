#' Realizes the state_target into binary or abundance using a function
#'
#' @param sim_obj a SimulationObject
#' @param func a function that takes a suitability value and outputs a realised values, either a presence/absence or an abundance
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_state_target_realise_func(sim_obj)
#' }
sim_state_target_realise_func <- function(sim_obj, func) {
  state_target <- sim_obj@state_target_suitability
  realised_state_target <- state_target

  for (i in 1:dim(state_target)[3]){
    # Get the probability values from the state target
    prob_values <- terra::values(state_target[[i]])

    # Simulate binary values from the binomial distribution based on the probability values
    realised_values <- func(prob_values)

    terra::values(realised_state_target[[i]]) <- realised_values
  }

  # Update the SimulationObject with the binary state_target
  sim_obj@state_target_realised <- realised_state_target

  sim_obj
}
