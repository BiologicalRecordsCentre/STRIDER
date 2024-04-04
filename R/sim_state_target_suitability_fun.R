#' Determines the state_target_suitability from state_env using a custom function
#'
#' @param simulation_object a SimulationObject
#' @param fun a function that takes an environmental SpatRaster and outputs a suitability SpatRaster values from 0 to 1
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_state_target_realise_fun(simulation_object, fun, ...)
#' }
sim_state_target_suitability_fun <- function(simulation_object,filename = NULL, fun, ...) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  # apply the function
  suitability <- fun(simulation_object, ...)

  # validity checks
  if(!is.null(filename)){
    suitability <- write_raster_return_filename(suitability,filename)
  }

  simulation_object_original@state_target_suitability <- suitability
  simulation_object_original@hash <- hash_sim_obj(simulation_object_original)
  simulation_object_original
}
