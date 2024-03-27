# Define a custom class for the SimulationObject
setClass("SimulationObject",
         slots = list(
           background = "ANY",
           state_env = "ANY",
           state_target_suitability = "ANY",
           state_target_realised = "ANY",
           effort = "ANY",
           detect = "ANY",
           report = "ANY"
         )
)

# Create a constructor for the SimulationObject class
SimulationObject <- function(background, state_env = NULL, state_target_suitability = NULL, state_target_realised= NULL, effort = NULL, detect = NULL, report = NULL) {
  new("SimulationObject",
      background = background,
      state_env = state_env,
      state_target_suitability = state_target_suitability,
      state_target_realised = state_target_realised,
      effort = effort,
      detect = detect,
      report = report
  )
}
