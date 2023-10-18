#' Simulates reporting where all reports occur at an equal probability and identified correctly
#'
#' @param sim_state a SpatRaster for the true state to be detected from, and from which the extent and resolution will be used
#' @param sim_detections a sf of sampled points
#' @param prob a numeric probability of each target being reported
#' @return A simple feature collection with geometry type POINTs
#' @examples
#' sim_report_equal(state,detections,0.5)
sim_report_equal <- function(sim_state,sim_detections,prob=1,platform="iRecord"){

  reports <- sim_detections
  reports$reported <- runif(nrow(reports)) < prob

  reports$reported[reports$detected == F] <- F

  reports$platform <- platform

  reports
}
