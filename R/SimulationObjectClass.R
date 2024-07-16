# Define a custom class for the SimulationObject
setClass("SimulationObject",
         slots = list(
           background = "ANY",
           state_env = "ANY",
           state_target_suitability = "ANY",
           state_target_realised = "ANY",
           effort = "ANY",
           detect = "ANY",
           report = "ANY",
           metadata = "ANY",
           hash = "ANY"
         )
)

#' Create a SimulationObject
#'
#' A constructor for the `SimulationObject` class, which initializes the object with the specified background and optional state components.
#'
#' @param background A `SpatRaster` object representing the background raster data.
#' @param state_env Optional. A `SpatRaster` object representing the environmental state. Default is `NULL`.
#' @param state_target_suitability Optional. A `SpatRaster` object representing the target suitability state. Default is `NULL`.
#' @param state_target_realised Optional. A `SpatRaster` object representing the realised target state. Default is `NULL`.
#' @param effort Optional. A data frame or spatial object representing the sampling effort. Default is `NULL`.
#' @param detect Optional. A data frame or spatial object representing the detection events. Default is `NULL`.
#' @param report Optional. A data frame or spatial object representing the reporting events. Default is `NULL`.
#' @return An object of class `SimulationObject`.
#' @examples
#' \dontrun{
#' background <- terra::rast(matrix(0, 500, 500))
#' sim_obj <- SimulationObject(background)
#' }
#' @export
SimulationObject <- function(background, state_env = NULL, state_target_suitability = NULL, state_target_realised= NULL, effort = NULL, detect = NULL, report = NULL) {
  tmp <- new("SimulationObject",
      background = background,
      state_env = state_env,
      state_target_suitability = state_target_suitability,
      state_target_realised = state_target_realised,
      effort = effort,
      detect = detect,
      report = report,
      metadata = list(),
      hash = NULL)

  new("SimulationObject",
      background = background,
      state_env = state_env,
      state_target_suitability = state_target_suitability,
      state_target_realised = state_target_realised,
      effort = effort,
      detect = detect,
      report = report,
      metadata = list(),
      hash = hash_sim_obj(tmp)
  )
}

#' Plot a SimulationObject
#'
#' This method plots the different components of a `SimulationObject`, including the environmental state, target suitability, realised target state, effort, detection events, and reporting events.
#'
#' @param x A `SimulationObject` to be plotted.
#' @return Plots the various components of the `SimulationObject` to the current graphics device.
#' @examples
#' \dontrun{
#' background <- terra::rast(matrix(0, 500, 500))
#' sim_obj <- SimulationObject(background)
#' plot(sim_obj)
#' }
#' @export
setMethod("plot", "SimulationObject", function(x) {
  x <- read_sim_obj_rasters(x)

  if(!is.null(x@state_env)){
    plot(x@state_env,
         main = paste("@state_env ",names(x@state_env)))
  }

  if(!is.null(x@state_target_suitability)){
    plot(x@state_target_suitability,
         main = paste("@state_target_suitability ",names(x@state_target_suitability)))
  }

  if(!is.null(x@state_target_realised)){
    plot(x@state_target_realised,
         main = paste("@state_target_realised ",names(x@state_target_realised)))
  }

  if(!is.null(x@effort)){
    plot(x@background, main="@effort",legend = F,col = "white")
    plot(x@effort$geometry,add=T)
  }

  if(!is.null(x@detect)){
    plot(x@background, main="@detect",legend = F,col = "white")
    plot(x@detect[x@detect$target_detected>0,c("geometry","target_detected")],col = "blue",add=T,pch=19)
    plot(x@detect[x@detect$target_detected==0,c("geometry","target_detected")],col = "red",pch = 4,add=T)
  }
})
