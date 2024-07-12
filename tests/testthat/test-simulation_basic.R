library(STRIDER)
library(sf)
library(terra)

# Create the background
background <- terra::rast(matrix(0,440,700))

# 1 Create the simulation object
sim_obj <- SimulationObject(background = background)

# 2 Simulate a uniform state of the target across the background within the simulation object
sim_obj <- sim_state_env(sim_obj, fun = state_env_uniform, value = 0.6)

# 2 Simulate a uniform state of the target across the background within the simulation object
sim_obj <- sim_state_target_suitability(sim_obj, fun=state_target_suitability_uniform, value= 0.5,n_targets = 2)

# 2.5 realise the distribution
sim_obj <- sim_state_target_realise(sim_obj,fun = state_target_realise_threshold,threshold = 0.5)
sim_state_target_realise(sim_obj, fun = state_target_realise_binomial)

# 3 Simulate effort across the landscape within the simulation object
sim_obj <- sim_effort(sim_obj,fun=effort_basic, n_samplers = 2, n_visits = 3, n_sample_units=2, replace = FALSE)

# 4 Simulate detection within the simulation object
sim_obj <- sim_detect(sim_obj)

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


#3
test_that("Simulating effort across the landscape", {
  expect_identical(class(sim_obj@effort), c("sf","data.frame"))
})

#4
test_that("Simulating detection", {
  expect_identical(class(sim_obj@detect), c("sf","data.frame"))
})


