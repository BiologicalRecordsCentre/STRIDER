#' Simulates a gradiated state of the environment
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param from lower value of gradient
#' @param to upper value of gradient
#' @return An updated simulation object with the newly calculated state of the environment in the correct slot
#' @examples
#' \dontrun{
#' sim_state_env_gradient(simulation_object, 0, 1)
#' }
sim_state_env_gradient <- function(simulation_object, filename=NULL, from = 0, to = 1) {

  #create the gradient spatraster
  simulation_object <- read_sim_obj_rasters(simulation_object)
  background <- simulation_object@background
  sim_state <- background[[1]]
  terra::values(sim_state) <- rep(seq(from = from, to = to, length.out = dim(background)[2]), dim(background)[1])
  names(sim_state) <- "env"

  #use byod function to add it to the simulation object
  simulation_object <- sim_state_env_byod(
    simulation_object = simulation_object,
    filename = filename,
    spatraster =  sim_state)

  # Return the updated simulation_object
  return(simulation_object)
}
