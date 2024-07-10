#' Realizes the State Target into Binary or Abundance
#'
#' This function realizes the state target for a given simulation object into either
#' binary (presence/absence) or abundance using a predefined or a custom function.
#' The updated simulation object with the new state target realization and metadata is returned.
#'
#' @param simulation_object A SimulationObject containing the state environment.
#' @param fun A custom function that takes a SimulationObject with an environment slot and outputs a target suitability SpatRaster indicating either presence/absence or abundance.
#' @param filename A character string specifying the filename to save the resultant SpatRaster. If `NULL`, the SpatRaster is not saved to a file. Default is `NULL`.
#' @param ... Additional arguments to be passed to the function specified in `fun`.
#'
#' @return The updated simulation object with the new state target realization.
#'
#' @details
#' - If `fun` is a custom function, it will be applied to the simulation object.
#' - If `filename` is provided, the resultant SpatRaster is saved, and the filename is returned.
#'
#' @examples
#' \dontrun{
#' sim_obj <- sim_state_target_realise(sim_obj, fun = state_target_realise_binomial)
#' sim_obj <- sim_state_target_realise(sim_obj, fun = state_target_realise_threshold, threshold = 0.5)
#' sim_obj <- sim_state_target_realise(sim_obj, fun = my_custom_function)
#' sim_obj <- sim_state_target_realise(sim_obj, fun = my_custom_function, filename = "output.tif")
#' }
#'
#' @export
sim_state_target_realise <- function(simulation_object,fun, filename=NULL, ...) {
  check_fun(fun)

  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)



  # apply the function
  realised <- fun(simulation_object, ...)

  # validity checks
  if(!is.null(filename)){
    realised <- write_raster_return_filename(realised,filename)
  }

  fun_args <- as.list(match.call())
  simulation_object_original@metadata[["state_target_realised"]] <- fun_args[3:length(fun_args)]

  simulation_object_original@state_target_realised <- realised
  simulation_object_original@hash <- hash_sim_obj(simulation_object_original)
  simulation_object_original
}
