#' Realizes the state_target into binary from the continuous probability values
#'
#' @param sim_obj a SimulationObject
#' @return A SimulationObject with a binary state_target
#' @examples
#' \dontrun{
#' sim_state_target_binary(sim_obj)
#' }
sim_state_target_realise_binomial <- function(sim_obj) {
  state_target <- sim_obj@state_target
  binary_state_target <- state_target

  for (i in 1:dim(sim_obj@state_target)[3]){
    # Get the probability values from the state target
    prob_values <- terra::values(state_target[[i]])

    # Simulate binary values from the binomial distribution based on the probability values
    binary_values <- rbinom(length(prob_values), 1, prob_values)

    terra::values(binary_state_target[[i]]) <- binary_values
  }

  # Update the SimulationObject with the binary state_target
  sim_obj@state_target_realised <- binary_state_target

  sim_obj
}
