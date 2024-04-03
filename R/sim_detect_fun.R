#' Defines detection using a custom function
#'
#' @param simulation_object a SimulationObject
#' @param fun a function that takes the simulation object and returns a simulation object with  data in detection slot
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a detection
#' @examples
#' \dontrun{
#' sim_effort_fun(simulation_object, fun, ...)
#' }
sim_detect_fun <- function(simulation_object, fun, ...) {
  simulation_object_original <- simulation_object <- read_sim_obj_rasters(simulation_object)

  # apply the function
  detections <- fun(simulation_object, ...)

  # validity checks

  simulation_object_original@detect <- detections
  simulation_object_original
}
