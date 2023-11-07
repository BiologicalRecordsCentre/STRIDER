#' Simulates a state of the target using a wrapper around sim_state_env_NLMR
#'
#' @param background a SpatRaster from which the extent and resolution will be used
#' @param alg single or list of algorithms as character
#' @param params list of vectors providing parameters to be passed to each NMLR function call
#' @return A SpatRaster with extent and resolution of background but values  describing the environment
sim_state_state_NLMR <- function(background,alg,params=NULL,abundance =10){
  sim_state <- sim_state_env_NLMR(background=background,alg=alg,params=params)*abundance

  names(sim_state) <- "abundance"
  sim_state
}
