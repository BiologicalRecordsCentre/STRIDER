#' Extract Environmental, Suitability, and Realised Values at Sampling Locations
#'
#' This function extracts values from the environmental state, target suitability, and realised target state
#' at the locations defined by the sampling effort in a `SimulationObject`.
#'
#' @param simulation_object A `SimulationObject` containing the environmental state, target suitability,
#'   realised target state, and sampling effort.
#' @param fun fun argument passed to \link[terra]{extract} for extracting realised values.
#' @param ... Additional arguments passed to \link[terra]{extract} for extracting realised values.
#'
#' @return A `SimulationObject` with updated `@effort` slot containing extracted values for environmental
#'   state, target suitability, and realised target state at each sampling location.
#'
#' @details
#' This function extracts the values from the environmental state (`@state_env`), target suitability
#' (`@state_target_suitability`), and realised target state (`@state_target_realised`) at the sampling locations
#' defined in the `@effort` slot of the input `SimulationObject`. The extracted values are added as columns to the `@effort` slot.
#'
#' @export
sim_extract <- function(simulation_object,realised_extract_fun=mean) {
  #get the effort
  effort_sf <- simulation_object@effort

  #get values from env, suitability, realised
  extracted_values <- terra::extract(simulation_object@state_env,effort_sf,fun = mean)
  effort_sf[,names(extracted_values)] <- extracted_values

  #loop through each target
  targets_sf <- list()
  for (target in names(simulation_object@state_target_suitability)){
    target_sf <- effort_sf
    target_sf$target <- target

    #extract suitability
    extracted_values <- terra::extract(simulation_object@state_target_suitability[target],effort_sf,ID=F,fun = mean)
    target_sf[,"target_suitability"] <- extracted_values

    #target realised
    extracted_values <- terra::extract(simulation_object@state_target_realised[target],effort_sf,ID=F,fun = mean)
    target_sf[,"target_realised"] <- extracted_values

    #extract realised
    extracted_values <- terra::extract(simulation_object@state_target_realised[target],effort_sf,ID=F,fun = realised_extract_fun)
    target_sf[,"target_detected"] <- extracted_values

    targets_sf[[target]] <- target_sf
  }

  effort_sf <- do.call(rbind,targets_sf)
  rownames(effort_sf) <- NULL

  effort_sf_extracted <- effort_sf
  effort_sf_extracted
}
