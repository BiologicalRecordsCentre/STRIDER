#' Realizes the state_target into binary from the continuous probability values
#'
#' @param simulation_object a SimulationObject
#' @param threshold a threshold value from 0 to 1 from which suitability values >=threshold will be realised as 1s and <threshold will be realised as 0s
#' @return A SimulationObject with a binary state_target_realised
#' @examples
#' \dontrun{
#' sim_state_target_realised_threshold(simulation_object)
#' }
sim_state_target_realise_threshold <- function(simulation_object,filename=NULL,threshold = 0.5) {

  threshold_fun<- function(simulation_object,threshold){
    state_target <- binary_state_target <- simulation_object@state_target_suitability
    for (i in 1:dim(state_target)[3]){
      # Get the probability values from the state target
      prob_values <- terra::values(state_target[[i]])

      # Simulate binary values from the binomial distribution based on the probability values
      binary_values <- as.numeric(prob_values >= threshold)

      terra::values(binary_state_target[[i]]) <- binary_values
    }
    binary_state_target
  }

  sim_state_target_realise_fun(simulation_object,
                               filename=filename,
                               fun = threshold_fun,
                               threshold=threshold)
}
