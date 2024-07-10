#' Determine the Target Suitability from the State Environment
#'
#' This function calculates the state target suitability for a given simulation object
#' using either a predefined or a custom function. The updated simulation object with the
#' new state target suitability and metadata is returned.
#'
#' @param simulation_object A SimulationObject
#' @param fun Either 'uniform' to use the included uniform suitability function or a custom function that takes a SimulationObject with an environment slot and outputs a target suitability SpatRaster with as many bands as there are targets
#' @param filename A character string specifying the filename to save the resultant SpatRaster. If `NULL`, the SpatRaster is not saved to a file.
#' @param ... Additional arguments to be passed to the function specified in `fun`.
#'
#' @return The updated simulation object with the new state target suitability.
#'
#' @details
#' - If `fun` is provided as 'uniform', the function uses the included uniform suitability function. This is unlikely to be a useful function but provided as a baseline.
#' - If `fun` is a character string corresponding to a function name, the function checks its existence and retrieves it.
#' - If `fun` is a custom function, it will be applied to the simulation object.
#' - If `filename` is provided, the resultant SpatRaster is saved, and the filename is returned.
#'
#' @examples
#' \dontrun{
#' sim_obj <- sim_state_target_suitability(sim_obj, fun = state_target_suitability_uniform, value = 0.5)
#' sim_obj <- sim_state_target_suitability(sim_obj, fun = my_custom_function)
#' sim_obj <- sim_state_target_suitability(sim_obj, fun = my_custom_function, filename = "output.tif")
#' }
#'
#' @export
sim_state_target_suitability <- function(simulation_object,fun,filename = NULL, ...) {
  check_fun(fun)
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)


  # apply the function
  suitability <- fun(simulation_object, ...)

  # validity checks
  if(!is.null(filename)){
    suitability <- write_raster_return_filename(suitability,filename)
  }

  fun_args <- as.list(match.call())
  simulation_object_original@metadata[["state_target_suitability"]] <- fun_args[3:length(fun_args)]

  simulation_object_original@state_target_suitability <- suitability
  simulation_object_original@hash <- hash_sim_obj(simulation_object_original)
  simulation_object_original
}
