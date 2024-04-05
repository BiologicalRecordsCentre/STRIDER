library(STRIDER)

# 1 Create the simulation object
sim_obj1 <- SimulationObject(background = terra::rast(matrix(1,470,600)))
sim_obj2 <- SimulationObject(background = terra::rast(matrix(1.0,470,600)))
sim_obj3 <- SimulationObject(background = terra::rast(matrix(1,500,600)))

test_that("Testing hashing at SimulationObject creation",{
  expect_true(sim_obj1@hash == sim_obj2@hash)
  expect_false(sim_obj2@hash == sim_obj3@hash)
})

# 2 Simulate a uniform state of the target across the background within the simulation object
sim_obj1 <- sim_state_env_uniform(sim_obj1, value = 0.5000)
sim_obj2 <- sim_state_env_uniform(sim_obj2, value = 0.5)
sim_obj3 <- sim_state_env_uniform(sim_obj2, value = 0.6)

test_that("Testing hashing at environment creation",{
  expect_true(sim_obj1@hash == sim_obj2@hash)
  expect_false(sim_obj2@hash == sim_obj3@hash)
})

# state suitability
sim_obj1 <- sim_state_target_suitability_uniform(sim_obj1, value = 0.5000)
sim_obj2 <- sim_state_target_suitability_uniform(sim_obj2, value = 0.5)
sim_obj3 <- sim_state_target_suitability_uniform(sim_obj2, value = 0.6)

test_that("Testing hashing at environment suitability simulation",{
  expect_true(sim_obj1@hash == sim_obj2@hash)
  expect_false(sim_obj2@hash == sim_obj3@hash)
})


# state suitability
sim_obj1_thresh <- sim_state_target_realise_threshold(sim_obj1, threshold = 0.5000)
sim_obj2_thresh <- sim_state_target_realise_threshold(sim_obj2, threshold = 0.5)
sim_obj3_thresh <- sim_state_target_realise_threshold(sim_obj2, threshold = 0.6)

test_that("Testing hashing at environment suitability simulation - threshold",{
  expect_true(sim_obj1_thresh@hash == sim_obj2_thresh@hash)
  expect_false(sim_obj2_thresh@hash == sim_obj3_thresh@hash)
})

# state suitability
sim_obj1_bin <- sim_state_target_realise_binomial(sim_obj1)
sim_obj2_bin <- sim_state_target_realise_binomial(sim_obj2)
sim_obj3_bin <- sim_state_target_realise_binomial(sim_obj2)

test_that("Testing hashing at environment suitability simulation - Ensure that the hash changes",{
  expect_false(sim_obj1_bin@hash == sim_obj2_bin@hash)
  expect_false(sim_obj2_bin@hash == sim_obj3_bin@hash)
})

sim_obj1_effort <- sim_effort_uniform(sim_obj1_thresh,n_samplers = 10)

test_that("Ensure that the hash changes after effort simulation",{
  expect_false(sim_obj1_thresh@hash == sim_obj1_effort@hash)
})

sim_obj1_detect <- sim_detect_equal(sim_obj1_effort)

test_that("Ensure that the hash changes after detection simulation",{
  expect_false(sim_obj1_effort@hash == sim_obj1_detect@hash)
})

sim_obj1_report <- sim_report_equal(sim_obj1_detect)

test_that("Ensure that the hash changes after reporting simulations",{
  expect_false(sim_obj1_detect@hash == sim_obj1_report@hash)
})

