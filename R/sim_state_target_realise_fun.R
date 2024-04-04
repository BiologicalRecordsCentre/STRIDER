#' Realizes the state_target into binary or abundance using a custom function
#'
#' @param simulation_object a SimulationObject
#' @param fun a function that takes a suitability SpatRaster and outputs a realised SpatRaster, either a presence/absence or an abundance
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_state_target_realise_fun(simulation_object, fun, ...)
#' }
sim_state_target_realise_fun <- function(simulation_object, filename=NULL, fun, ...) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  # apply the function
  realised <- fun(simulation_object, ...)

  # validity checks
  if(!is.null(filename)){
    realised <- write_raster_return_filename(realised,filename)
  }

  simulation_object_original@state_target_realised <- realised
  simulation_object_original@hash <- hash_sim_obj(simulation_object_original)
  simulation_object_original
}
