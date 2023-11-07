#' Simulates reporting where all reports occur at an equal probability and identified correctly
#'
#' @param background the background
#' @param state_env description
#' @param state_target a SpatRaster for the true state to be detected from, and from which the extent and resolution will be used
#' @param effort effort
#' @param detect a sf of sampled points
#' @param prob a numeric probability of each target being reported
#' @param platform name of the recording platform
#' @return A simple feature collection with geometry type POINTs
#' @examples
#' \dontrun{
#' sim_report_equal(state,detections,0.5)
#' }
sim_report_equal <- function(background, state_env, state_target, effort, detect,prob=1,platform="iRecord"){

  reports <- detect
  reports$reported <- runif(nrow(reports)) < prob

  reports$reported[reports$detected == F] <- F

  reports$platform <- platform

  reports
}
