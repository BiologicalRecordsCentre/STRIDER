#' Realizes the state_target into binary from the continuous probability values
#'
#' @param simulation_object a SimulationObject
#' @return A SimulationObject with a binary state_target
#' @examples
#' \dontrun{
#' sim_state_target_binary(simulation_object)
#' }
sim_state_target_realise_binomial <- function(simulation_object,filename=NULL) {

  binary_fun<- function(simulation_object){
    state_target <- binary_state_target <- simulation_object@state_target_suitability
    for (i in 1:dim(state_target)[3]){
      # Get the probability values from the state target
      prob_values <- terra::values(state_target[[i]])

      # Simulate binary values from the binomial distribution based on the probability values
      binary_values <- rbinom(length(prob_values), 1, prob_values)

      terra::values(binary_state_target[[i]]) <- binary_values
    }
    binary_state_target
  }

  sim_state_target_realise_fun(simulation_object,
                               filename=filename,
                               fun = binary_fun)
}
