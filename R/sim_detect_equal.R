#' Simulates detection where all detections occur at an equal probability and identified correctly
#'
#' @param background the background
#' @param state_env description
#' @param state_target a SpatRaster for the true state to be detected from,and from which the extent and resolution will be used
#' @param effort a sf of sampled points
#' @param prob a numeric probability of each target being detected
#' @return A simple feature collection with geometry type POINTs
#' @examples
#' \dontrun{
#' sim_detect_equal()
#' }
sim_detect_equal <- function(background, state_env, state_target, effort,prob=1){
  detections_all <- data.frame()

  for (i in 1:dim(state_target)[3]){
    detections <- effort
    detections$state <- terra::extract(state_target[[i]],effort)$abundance
    detections$target <- i

    detections$detected <- runif(nrow(detections)) < prob
    detections$identified <- TRUE

    detections_all <- rbind(detections_all,detections)
  }

  detections_all
}
