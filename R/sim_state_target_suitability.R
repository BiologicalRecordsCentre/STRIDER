#' Determines the state_target_suitability from state_env using a custom function
#'
#' @param simulation_object a SimulationObject
#' @param fun either 'uniform' to use the included uniform suitability function or a function that takes an SimulationObject with an environment slot and outputs a SimulationObject with a target suitability SpatRaster with values from 0 to 1
#' @param filename a file name and path to save the spatraster
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_state_target_suitability(simulation_object, fun, ...)
#' }
sim_state_target_suitability <- function(simulation_object,fun,filename = NULL, ...) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  if(is.character(fun)){
    if(!(fun %in% c("uniform"))){
      stop("Provided function must be 'uniform'")
    }
    fun <- get(paste0("state_target_suitability_",fun))
  }

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
