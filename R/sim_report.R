#' #' Defines reporting using a custom function
#' #'
#' #' @param simulation_object a SimulationObject
#' #' @param fun a function that takes the simulation object and returns a simulation object with  data in reporting slot
#' #' @param ... other parameters for the user supplied function fun
#' #' @return A SimulationObject with reporting
#' #' @examples
#' #' \dontrun{
#' #' sim_report_fun(simulation_object, fun, ...)
#' #' }
#' sim_report <- function(simulation_object, fun, ...) {
#'   check_fun(fun)
#'   simulation_object_original <- simulation_object
#'   simulation_object <- read_sim_obj_rasters(simulation_object)
#'
#'   # apply the function
#'   report <- fun(simulation_object, ...)
#'
#'   # validity checks
#'   fun_args <- as.list(match.call())
#'   simulation_object_original@metadata[["report"]] <- fun_args[3:length(fun_args)]
#'
#'   simulation_object_original@report <- report
#'   simulation_object_original@hash <- hash_sim_obj(simulation_object_original)
#'   simulation_object_original
#' }
