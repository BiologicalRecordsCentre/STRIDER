state_env_gradient <- function(simulation_object, from = 0, to = 1) {
  #create the gradient spatraster
  background <- simulation_object@background
  sim_state <- background[[1]]
  terra::values(sim_state) <- rep(seq(from = from, to = to, length.out = dim(background)[2]), dim(background)[1])
  names(sim_state) <- "env"
  sim_state
}

state_env_uniform <- function(simulation_object, value = 1) {
  #create the gradient spatraster
  background <- simulation_object@background
  sim_state <- background[[1]]
  terra::values(sim_state) <- value
  names(sim_state) <- "env"
  sim_state
}
