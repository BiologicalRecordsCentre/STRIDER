#' Simulates a uniform effort
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param n_visits a number indicating the average number of visits
#' @param replace a logical indicating whether to sample with replacement
#' @return An updated simulation object with the newly calculated effort in the correct slot
#' @examples
#' \dontrun{
#' sim_effort_uniform(simulation_object, 100, FALSE)
#' }
sim_effort_uniform <- function(simulation_object, n_visits = 100, replace = FALSE) {
  state_target <- simulation_object@state_target

  visited_cells <- sample(terra::cells(state_target), size = n_visits, replace = replace)

  sim_effort_points <- as.data.frame(terra::xyFromCell(state_target, visited_cells))
  sim_effort_points$sampler_id <- 1
  sim_effort_points$cell_id <- visited_cells

  effort_sf <- sf::st_as_sf(sim_effort_points, coords = c("x", "y"), crs = terra::crs(state_target))

  simulation_object@effort <- effort_sf

  # Return the updated simulation_object
  return(simulation_object)
}
