#' Determines the state_target_suitability from state_env using a custom function
#'
#' @param sim_obj a SimulationObject
#' @param fun a function that takes an environmental SpatRaster and outputs a suitability SpatRaster values from 0 to 1
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_state_target_realise_fun(sim_obj, fun, ...)
#' }
sim_state_target_suitability_fun <- function(sim_obj, fun, ...) {
  # apply the function
  sim_obj <- fun(sim_obj, ...)

  # validity checks

  sim_obj
}
