library(STRIDER)

# Create the background
background <- terra::rast(matrix(0,1000,600))

# 1 Create the simulation object
sim_obj <- SimulationObject(background = background)

#create a custom environment that we'll implement using env_byod
env_byod <- c(terra::rast(matrix(1:1000,1000,600)),terra::rast(matrix(round(runif(1000*600)*100),1000,600)))
names(env_byod) <- c("rainfall","altitude")

sim_obj <- sim_state_env(sim_obj,spatraster =  env_byod)

# 2 Simulate a uniform state of the target across the background within the simulation object
suit_fun <- function(sim_obj){
  target_suitability <- sim_obj@background
  terra::values(target_suitability) <- 0.2
  names(target_suitability) <- "frog"

  # suitability of 0.7 when rainfall>500 and altitude < 50
  target_suitability[sim_obj@state_env$rainfall>500 & sim_obj@state_env$altitude<50] <- 0.4
  target_suitability[sim_obj@state_env$rainfall>800 & sim_obj@state_env$altitude<40] <- 0.9

  target_suitability
}

sim_obj <- sim_state_target_suitability(sim_obj, fun = suit_fun)

#function for realising suitability
realise_fun <- function(sim_obj){
  as.numeric(sim_obj@state_target_suitability > 0.5)
}

# 2.5 realise the distribution
sim_obj <- sim_state_target_realise(sim_obj,fun = realise_fun)



sim_obj

#1
test_that("Test creating a uniform environment", {
  expect_true(class(sim_obj) == "SimulationObject")
  expect_true(class(sim_obj@background) == "SpatRaster")
})

#2
test_that("Creating a uniform target distribution", {
  expect_true(class(sim_obj@state_target_suitability) == "SpatRaster")
})

#2.5
test_that("Creating a realising the target distribution", {
  expect_true(class(sim_obj@state_target_realised) == "SpatRaster")
})


