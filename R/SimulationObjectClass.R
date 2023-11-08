# Define a custom class for the SimulationObject
setClass("SimulationObject",
         slots = list(
           background = "ANY",
           state_env = "ANY",
           state_target = "ANY",
           effort = "ANY",
           detect = "ANY",
           report = "ANY"
         )
)

# Create a constructor for the SimulationObject class
SimulationObject <- function(background, state_env = NULL, state_target = NULL, effort = NULL, detect = NULL, report = NULL) {
  new("SimulationObject",
      background = background,
      state_env = state_env,
      state_target = state_target,
      effort = effort,
      detect = detect,
      report = report
  )
}
