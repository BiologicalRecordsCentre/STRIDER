#' Simulates a uniform effort
#'
#' @param sim_state a SpatRaster from which the extent and resolution will be used
#' @param n_visits a number indicating the average number of visits
#' @param replace a logical indicating whether to sample with replacement
#' @return A simple feature collection with geometry type POINTs
#' @examples
#' sim_effort_uniform(state,100,T)
sim_effort_uniform <- function(sim_state,n_visits=100,replace=F){
  visited_cells <- sample(terra::cells(sim_state),size = n_visits,replace=replace)

  sim_effort_points <- as.data.frame(terra::xyFromCell(sim_state,visited_cells))
  sim_effort_points$sampler_id <- 1
  sim_effort_points$cell_id <- visited_cells

  sf::st_as_sf(sim_effort_points,coords= c("x","y"),crs=terra::crs(sim_state))
}
