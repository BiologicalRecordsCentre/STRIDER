#' Defines detection using a custom function
#'
#' @param sim_obj a SimulationObject
#' @param fun a function that takes the simulation object and returns a simulation object with  data in detection slot
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a detection
#' @examples
#' \dontrun{
#' sim_effort_fun(sim_obj, fun, ...)
#' }
sim_detect_fun <- function(sim_obj, fun, ...) {
  # apply the function
  sim_obj <- fun(sim_obj, ...)

  # validity checks

  sim_obj
}
