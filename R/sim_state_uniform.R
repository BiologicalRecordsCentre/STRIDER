#' Simulates a uniform state of the target
#'
#' @param background a SpatRaster from which the extent and resolution will be used
#' @param abundance a number indicating the abundance of the target in each cell
#' @return A SpatRaster with extent and resolution of background but values from abundance
#' @examples
#' sim_state_uniform(terra::rast(nrows=100,ncols=100),42)
sim_state_uniform <- function(background,abundance =10){
  sim_state <- background[[1]]
  terra::values(sim_state) <- abundance
  names(sim_state) <- "abundance"
  sim_state
}

