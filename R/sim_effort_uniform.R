#' Simulates a uniform effort
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param n_samplers a number indicating the number of samplers
#' @param n_visits a number indicating the average number of visits per sampler
#' @param n_sample_events a number of sampling events made per visit
#' @param replace a logical indicating whether to sample with replacement
#' @return An updated simulation object with the newly calculated effort in the correct slot
#' @examples
#' \dontrun{
#' sim_effort_uniform(simulation_object, 100, 20,1 FALSE)
#' }
sim_effort_uniform <- function(simulation_object, n_samplers = 1, n_visits = 1, n_sample_events=1, replace = FALSE) {

  #which cells are visits
  state_target <- simulation_object@state_target
  visited_cells <- rep(sample(terra::cells(state_target), size = n_samplers*n_visits, replace = replace),each = n_sample_events)

  # capture data
  sim_effort_points <- as.data.frame(terra::xyFromCell(state_target, visited_cells))
  sim_effort_points$sampler_id <- rep(1:n_samplers,each = n_visits*n_sample_events)
  sim_effort_points$visit_id <- rep(1:n_visits,n_samplers,each = n_sample_events)
  sim_effort_points$sample_id <- rep(1:n_sample_events,n_samplers*n_visits)

  sim_effort_points$cell_id <- visited_cells

  effort_sf <- sf::st_as_sf(sim_effort_points, coords = c("x", "y"), crs = terra::crs(state_target))
  simulation_object@effort <- effort_sf

  # Return the updated simulation_object
  return(simulation_object)
}
