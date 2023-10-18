#' Simulates detection where all detections occur at an equal probability and identified correctly
#'
#' @param sim_state a SpatRaster for the true state to be detected from,and from which the extent and resolution will be used
#' @param sim_effort a sf of sampled points
#' @param prob a numeric probability of each target being detected
#' @return A simple feature collection with geometry type POINTs
#' @examples
#' sim_detect_equal()
sim_detect_equal <- function(sim_state,sim_effort,prob=1){
  detections <- sim_effort
  detections$state <- terra::extract(sim_state,sim_effort)$abundance

  detections$detected <- runif(nrow(detections)) < prob
  detections$identified <- TRUE

  detections
}
