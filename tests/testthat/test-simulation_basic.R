library(STRIDER)

# create background
background <- terra::rast(matrix(0,1000,1000))

#simulate a uniform state of the environment across the background
state_env <- sim_state_env_uniform(background)

test_that("Test creating a uniform environment", {
  expect_true(is(state_env, "SpatRaster"))
  expect_equal(dim(background),dim(state_env))
})


#simulate a uniform state of the target across the background
state_target <- sim_state_target_uniform(background,state_env,42)

test_that("Creating a uniform target distribution", {
  expect_true(is(state_target, "SpatRaster"))
  expect_equal(dim(background),dim(state_target))
})


#simulate effort across the landscape
effort <- sim_effort_uniform(background,state_env,state_target,n_visits=100,replace=F)

test_that("Simulating effort across the landscape", {
  expect_true(is(effort, "sf"))
  expect_equal(nrow(effort),100)
})


# simulate detection
detections <-sim_detect_equal(background,state_env,state_target,effort,prob=0.5)

test_that("Simulating detection", {
  expect_true(is(detections, "sf"))
  expect_equal(nrow(detections),nrow(effort))
})

# simulate reporting
reports <- sim_report_equal(background,state_env,state_target,effort,detections,prob=0.8,platform="iRecord")

test_that("Simulating reporting", {
  expect_true(is(reports, "sf"))
  expect_equal(nrow(reports),nrow(detections))
})



