library(STRIDER)

# Create the background
background <- terra::rast(matrix(0,1000,600))

# 1 Create the simulation object
sim_obj <- SimulationObject(background = background)

sim_obj <- sim_state_env_gradient(sim_obj)

# 2 Simulate a uniform state of the target across the background within the simulation object
sim_obj <- sim_state_target_suitability_uniform(sim_obj, value= 0.5,n_targets = 2)

#test others
#library(virtualspecies)
#sim_state_target_suitability_virtualspecies(sim_obj)

# 2.5 realise the distribution
sim_state_target_realise_func(sim_obj,func = function(x){round(x*100)})
sim_state_target_realise_threshold(sim_obj,threshold=0.4)
sim_obj <- sim_state_target_realise_binomial(sim_obj)


# 3 Simulate effort across the landscape within the simulation object
sim_obj <- sim_effort_uniform(sim_obj, n_samplers = 2, n_visits = 3, n_sample_units=2, replace = FALSE)

# 4 Simulate detection within the simulation object
sim_obj <- sim_detect_equal(sim_obj, prob = 0.5)

# 5 Simulate reporting within the simulation object
sim_obj <- sim_report_equal(sim_obj, prob = 0.8, platform = "iRecord")

#print(sim_obj)

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

#5
test_that("Simulating reporting", {
  expect_identical(class(sim_obj@report), c("sf","data.frame"))
})

