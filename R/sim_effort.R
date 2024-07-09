#' Defines effort using a built-in function, a custom function, or sf POINTS
#'
#' This function applies a user-supplied function to a SimulationObject to define effort.
#'
#' @param simulation_object A SimulationObject containing simulation data.
#' @param fun A function that takes the simulation_object and additional parameters, and returns a modified simulation_object with effort data.
#' @param sf Optional; if provided, skips applying 'fun' and directly uses this Spatial*DataFrame (sf) for effort calculation.
#' @param ... Additional parameters passed to the user-supplied function 'fun'.
#' @return A SimulationObject with updated effort information.
#' @examples
#' \dontrun{
#' sim_effort(simulation_object, fun, ...)
#' }
#' @export
sim_effort <- function(simulation_object, fun, sf=NULL, ...) {
  simulation_object_original <- simulation_object
  simulation_object <- read_sim_obj_rasters(simulation_object)

  if (is.null(sf)){
    if(is.character(fun)){
      if(!(fun %in% c("basic"))){
        stop("Provided function must be 'basic'")
      }
      fun <- get(paste0("effort_",fun))
    }
    # apply the function
    effort_sf <- fun(simulation_object, ...)
  } else {
    effort_sf <- sf
  }

  #get values from env, suitability, realised
  extracted_values <- terra::extract(simulation_object@state_env,effort_sf,ID=T)
  effort_sf[,names(extracted_values)] <- extracted_values

  #loop through each target
  targets_sf <- list()
  for (target in names(simulation_object@state_target_suitability)){
    target_sf <- effort_sf
    target_sf$target <- target

    #extract suitability
    extracted_values <- terra::extract(simulation_object@state_target_suitability[target],effort_sf,ID=F)

    target_sf[,"target_suitability"] <- extracted_values

    #extract realised
    extracted_values <- terra::extract(simulation_object@state_target_realised[target],effort_sf,ID=F)
    target_sf[,"target_realised"] <- extracted_values

    targets_sf[[target]] <- target_sf
  }

  effort_sf <- do.call(rbind,targets_sf)
  rownames(effort_sf) <- NULL

  # validity checks
  fun_args <- as.list(match.call())
  simulation_object_original@metadata[["effort"]] <- fun_args[3:length(fun_args)]

  simulation_object_original@effort <- effort_sf
  simulation_object_original@hash <- hash_sim_obj(simulation_object_original)
  simulation_object_original
}
