#' Simulates reporting where all reports occur at an equal probability and identified correctly
#'
#' @param simulation_object an R object of class 'SimulationObject' containing all the necessary information for the simulation
#' @param prob a numeric probability of each target being reported
#' @param platform name of the recording platform
#' @return An updated simulation object with the newly calculated report in the correct slot
#' @examples
#' \dontrun{
#' sim_report_equal(simulation_object, 0.5, "iRecord")
#' }
sim_report_equal <- function(simulation_object, prob = 1, platform = "iRecord") {
  detect <- simulation_object@detect

  reports <- detect
  reports$reported <- runif(nrow(reports)) < prob

  reports$reported[reports$detected == FALSE] <- FALSE

  reports$platform <- platform

  simulation_object@report <- reports

  # Return the updated simulation_object
  return(simulation_object)
}
