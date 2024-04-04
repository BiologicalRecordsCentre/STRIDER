#' Realizes the state_target into binary from the continuous probability values
#'
#' @param simulation_object a SimulationObject
#' @return A SimulationObject with a binary state_target
#' @examples
#' \dontrun{
#' sim_state_target_binary(simulation_object)
#' }
sim_state_target_realise_binomial <- function(simulation_object) {
  #TODO
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  state_target <- simulation_object@state_target_suitability
  binary_state_target <- state_target

  for (i in 1:dim(state_target)[3]){
    # Get the probability values from the state target
    prob_values <- terra::values(state_target[[i]])

    # Simulate binary values from the binomial distribution based on the probability values
    binary_values <- rbinom(length(prob_values), 1, prob_values)

    terra::values(binary_state_target[[i]]) <- binary_values
  }

  # Update the SimulationObject with the binary state_target
  simulation_object_original@state_target_realised <- binary_state_target

  simulation_object_original
}
