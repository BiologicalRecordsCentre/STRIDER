#' Simulate Detection Process
#'
#' This function simulates the detection process in a biodiversity study using a specified function to extract the realised state.
#'
#' @param simulation_object A `SimulationObject` that contains the state of the environment, target, effort, and other relevant simulation information.
#' @param realised_extract_fun A function to extract the realised state used in \code{\link[=sim_extract]{sim_extract()}}. The default is `mean`.
#'
#' @return A `SimulationObject` with the detection information added.
sim_detect <- function(simulation_object, realised_extract_fun = mean) {
  check_fun(realised_extract_fun)

  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  extracted <- sim_extract(simulation_object,realised_extract_fun=realised_extract_fun)


  # validity checks
  fun_args <- as.list(match.call())
  simulation_object_original@metadata[["detect"]] <- fun_args[3:length(fun_args)]

  simulation_object_original@detect <- extracted
  simulation_object_original@hash <- hash_sim_obj(simulation_object_original)
  simulation_object_original
}
