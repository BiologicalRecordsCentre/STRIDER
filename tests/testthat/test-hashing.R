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
sim_obj1 <- sim_state_target_realise_threshold(sim_obj1, threshold = 0.5000)
sim_obj2 <- sim_state_target_realise_threshold(sim_obj2, threshold = 0.5)
sim_obj3 <- sim_state_target_realise_threshold(sim_obj2, threshold = 0.6)

test_that("Testing hashing at environment suitability simulation",{
  expect_true(sim_obj1@hash == sim_obj2@hash)
  expect_false(sim_obj2@hash == sim_obj3@hash)
})

