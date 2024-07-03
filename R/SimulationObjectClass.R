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

# Create a constructor for the SimulationObject class
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
    plot(x@detect[x@detect$state_detected>0,c("geometry","state_detected")],col = "blue",add=T,pch=19)
    plot(x@detect[x@detect$state_detected==0,c("geometry","state_detected")],col = "red",pch = 4,add=T)
  }

  if(!is.null(x@report)){
    plot(x@background, main="@report",legend = F,col = "white")
    plot(x@report[x@report$reported==T,c("geometry")],col = "blue",add=T,pch=19)
    plot(x@detect[x@report$reported==F,c("geometry")],col = "red",pch = 4,add=T)
  }
})
