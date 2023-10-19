#' Simulates a uniform state of the environment
#'
#' @param background a SpatRaster from which the extent and resolution will be used
#' @return A SpatRaster with extent and resolution of background but values from abundance
#' @examples
#' sim_state_env_uniform(terra::rast(nrows=100,ncols=100))
sim_state_env_uniform <- function(background){
  sim_state <- background[[1]]
  terra::values(sim_state) <- 0
  names(sim_state) <- "env"
  sim_state
}

