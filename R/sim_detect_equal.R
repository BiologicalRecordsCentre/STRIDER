#' Simulates detection where all detections occur at an equal probability and identified correctly
#'
#' @param sim_state a SpatRaster for the true state to be detected from,and from which the extent and resolution will be used
#' @param sim_effort a sf of sampled points
#' @param prob a numeric probability of each target being detected
#' @return A simple feature collection with geometry type POINTs
#' @examples
#' sim_detect_equal()
sim_detect_equal <- function(sim_state,sim_effort,prob=1){
  detections_all <- data.frame()

  for (i in 1:dim(sim_state)[3]){
    detections <- sim_effort
    detections$state <- terra::extract(sim_state[[i]],sim_effort)$abundance
    detections$target <- i

    detections$detected <- runif(nrow(detections)) < prob
    detections$identified <- TRUE

    detections_all <- rbind(detections_all,detections)
  }

  detections_all
}
