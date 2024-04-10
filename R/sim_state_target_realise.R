#' Realizes the state_target into binary or abundance using a custom function
#'
#' @param simulation_object a SimulationObject
#' @param fun either 'uniform' to use the included uniform suitability function or a function that takes an SimulationObject with an environment slot and outputs a SimulationObject with a target suitability SpatRaster with either a presence/absence or an abundance
#' @param filename a file name and path to save the spatraster
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_state_target_realise(simulation_object, fun, ...)
#' }
sim_state_target_realise <- function(simulation_object,fun, filename=NULL, ...) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  if(is.character(fun)){
    if(!(fun %in% c("binomial","threshold"))){
      stop("Provided function must be 'binomial' or 'threshold'")
    }
    fun <- get(paste0("state_target_realise_",fun))
  }

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
