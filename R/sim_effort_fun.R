#' Defines effort using a custom function
#'
#' @param sim_obj a SimulationObject
#' @param fun a function that takes the simulation object and returns a simulation object with  data in effort slot
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_effort_fun(sim_obj, fun, ...)
#' }
sim_effort_fun <- function(sim_obj, fun, ...) {
  # apply the function
  sim_obj <- fun(sim_obj, ...)

  # validity checks

  sim_obj
}
