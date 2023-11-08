library(STRIDER)

# Create the background
background <- terra::rast(matrix(0,1000,600))

# 1 Create the simulation object
sim_obj <- SimulationObject(background = background)

# 2 Simulate a uniform state of the target across the background within the simulation object
sim_obj <- sim_state_target_uniform(sim_obj, abundance = 42)

# 3 Simulate effort across the landscape within the simulation object
sim_obj <- sim_effort_uniform(sim_obj, n_visits = 100, replace = FALSE)

# 4 Simulate detection within the simulation object
sim_obj <- sim_detect_equal(sim_obj, prob = 0.5)

# 5 Simulate reporting within the simulation object
sim_obj <- sim_report_equal(sim_obj, prob = 0.8, platform = "iRecord")


#1
test_that("Test creating a uniform environment", {
  expect_true(class(sim_obj) == "SimulationObject")
  expect_true(class(sim_obj@background) == "SpatRaster")
  expect_equal(dim(sim_obj@background), dim(sim_obj@background))
})

#2
test_that("Creating a uniform target distribution", {
  expect_true(class(sim_obj@state_target) == "SpatRaster")
  expect_equal(dim(sim_obj@background), dim(sim_obj@state_target))
})

#3
test_that("Simulating effort across the landscape", {
  expect_identical(class(sim_obj@effort), c("sf","data.frame"))
  expect_equal(nrow(sim_obj@effort), 100)
})

#4
test_that("Simulating detection", {
  expect_identical(class(sim_obj@detect), c("sf","data.frame"))
  expect_equal(nrow(sim_obj@detect), nrow(sim_obj@effort))
})

#5
test_that("Simulating reporting", {
  expect_identical(class(sim_obj@report), c("sf","data.frame"))
  expect_equal(nrow(sim_obj@report), nrow(sim_obj@detect))
})

