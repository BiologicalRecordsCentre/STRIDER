#' Realizes the state_target into binary or abundance using a custom function
#'
#' @param sim_obj a SimulationObject
#' @param fun a function that takes a suitability SpatRaster and outputs a realised SpatRaster, either a presence/absence or an abundance
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_state_target_realise_fun(sim_obj, fun, ...)
#' }
sim_state_target_realise_fun <- function(sim_obj, fun, ...) {
  # apply the function
  sim_obj <- fun(sim_obj, ...)

  # validity checks

  sim_obj
}
