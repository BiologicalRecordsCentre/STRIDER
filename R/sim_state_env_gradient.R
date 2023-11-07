#' Simulates a uniform state of the environment
#'
#' @param background a SpatRaster from which the extent and resolution will be used
#' @param from lower value of gradient
#' @param to upper value of gradient
#' @return A SpatRaster with extent and resolution of background but values from abundance
#' @examples
#' \dontrun{
#' sim_state_env_gradient(terra::rast(nrows=100,ncols=100))
#' }
sim_state_env_gradient <- function(background,from=0,to=1){
  sim_state <- background[[1]]
  terra::values(sim_state) <- rep(seq(from=from,to=to,length.out=dim(background)[2]),dim(background)[1])
  names(sim_state) <- "env"
  sim_state
}

