#' Realizes the state_target into binary from the continuous probability values
#'
#' @param sim_obj a SimulationObject
#' @param threshold a threshold value from 0 to 1 from which suitability values >=threshold will be realised as 1s and <threshold will be realised as 0s
#' @return A SimulationObject with a binary state_target_realised
#' @examples
#' \dontrun{
#' sim_state_target_realised_threshold(sim_obj)
#' }
sim_state_target_realise_threshold <- function(sim_obj,threshold = 0.5) {
  state_target <- sim_obj@state_target_suitability
  binary_state_target <- state_target

  for (i in 1:dim(state_target)[3]){
    # Get the probability values from the state target
    prob_values <- terra::values(state_target[[i]])

    # Simulate binary values from the binomial distribution based on the probability values
    binary_values <- as.numeric(prob_values >= threshold)

    terra::values(binary_state_target[[i]]) <- binary_values
  }

  # Update the SimulationObject with the binary state_target
  sim_obj@state_target_realised <- binary_state_target

  sim_obj
}
