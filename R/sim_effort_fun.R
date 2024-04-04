#' Defines effort using a custom function
#'
#' @param simulation_object a SimulationObject
#' @param fun a function that takes the simulation object and returns a simulation object with  data in effort slot
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_effort_fun(simulation_object, fun, ...)
#' }
sim_effort_fun <- function(simulation_object, fun, ...) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  # apply the function
  effort <- fun(simulation_object, ...)

  # validity checks

  simulation_object_original@effort <- effort
  simulation_object_original
}
