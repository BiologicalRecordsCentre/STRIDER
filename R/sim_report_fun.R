#' Defines reporting using a custom function
#'
#' @param sim_obj a SimulationObject
#' @param fun a function that takes the simulation object and returns a simulation object with  data in reporting slot
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with reporting
#' @examples
#' \dontrun{
#' sim_report_fun(sim_obj, fun, ...)
#' }
sim_report_fun <- function(sim_obj, fun, ...) {
  # apply the function
  sim_obj <- fun(sim_obj, ...)

  # validity checks

  sim_obj
}
