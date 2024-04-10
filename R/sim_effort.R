#' Defines effort using a custom function
#'
#' @param simulation_object a SimulationObject
#' @param fun a function that takes the simulation object and returns a simulation object with  data in effort slot
#' @param ... other parameters for the user supplied function fun
#' @return A SimulationObject with a state_target_realised
#' @examples
#' \dontrun{
#' sim_effort(simulation_object, fun, ...)
#' }
sim_effort <- function(simulation_object, fun, ...) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  if(is.character(fun)){
    if(!(fun %in% c("uniform"))){
      stop("Provided function must be 'uniform'")
    }
    fun <- get(paste0("effort_",fun))
  }

  # apply the function
  effort <- fun(simulation_object, ...)

  # validity checks
  fun_args <- as.list(match.call())
  simulation_object_original@metadata[["effort"]] <- fun_args[3:length(fun_args)]

  simulation_object_original@effort <- effort
  simulation_object_original@hash <- hash_sim_obj(simulation_object_original)
  simulation_object_original
}
