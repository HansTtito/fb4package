# ============================================================================
# BIOENERGETIC CLASS TESTS
# ============================================================================

test_that("Bioenergetic constructor works correctly", {
  # Minimal valid construction
  species_params <- list(
    consumption = list(CEQ = 1, CA = 0.303, CB = -0.275, CQ = 0.1),
    respiration = list(REQ = 1, RA = 0.05, RB = -0.3, RQ = 0.1),
    egestion = list(EGEQ = 1, FA = 0.2),
    excretion = list(EXEQ = 1, UA = 0.1),
    predator = list(PREDEDEQ = 1, ED_ini = 4500, ED_end = 4500)
  )
  
  species_info <- list(
    scientific_name = "Salmo salar",
    common_name = "Atlantic salmon"
  )
  
  bio_obj <- Bioenergetic(
    species_params = species_params,
    species_info = species_info
  )
  
  expect_true(inherits(bio_obj, "Bioenergetic"))
  expect_true(is.Bioenergetic(bio_obj))
  expect_equal(bio_obj$species_info$scientific_name, "Salmo salar")
  expect_false(bio_obj$fitted)
  
  # Test with complete parameters
  environmental_data <- list(
    temperature = data.frame(Day = 1:100, Temperature = 15 + 5*sin(1:100/20))
  )
  
  diet_data <- list(
    proportions = data.frame(Day = 1:100, fish = 0.6, zooplankton = 0.4),
    energies = data.frame(Day = 1:100, fish = 5000, zooplankton = 3000),
    prey_names = c("fish", "zooplankton")
  )
  
  simulation_settings <- list(
    initial_weight = 10,
    duration = 100
  )
  
  bio_obj_complete <- Bioenergetic(
    species_params = species_params,
    species_info = species_info,
    environmental_data = environmental_data,
    diet_data = diet_data,
    simulation_settings = simulation_settings
  )
  
  expect_true(inherits(bio_obj_complete, "Bioenergetic"))
  expect_equal(bio_obj_complete$simulation_settings$initial_weight, 10)
  expect_equal(bio_obj_complete$simulation_settings$duration, 100)
  
  # Test error cases
  expect_error(Bioenergetic(species_params = NULL), "must be a non-null list")
  expect_error(Bioenergetic(species_params = "invalid"), "must be a non-null list")
  expect_error(Bioenergetic(species_params = list(), species_info = "invalid"), "must be a list")
})


test_that("Bioenergetic print and summary methods work", {
  species_params <- list(
    consumption = list(CEQ = 1, CA = 0.303, CB = 0.275, CQ = 0.1)
  )
  
  species_info <- list(
    scientific_name = "Salmo salar",
    common_name = "Atlantic salmon",
    family = "Salmonidae"
  )
  
  bio_obj <- Bioenergetic(
    species_params = species_params,
    species_info = species_info,
    simulation_settings = list(initial_weight = 15, duration = 365)
  )
  
  # Test print method (should not error)
  expect_message(print(bio_obj), "FB4 Bioenergetic Model")
  expect_message(print(bio_obj), "Salmo salar")
  expect_message(print(bio_obj), "15g")
  
  # Test summary method (should not error)
  expect_message(summary(bio_obj), "FB4 Bioenergetic Model Summary")
  expect_message(summary(bio_obj), "Atlantic salmon")
  expect_message(summary(bio_obj), "Salmonidae")
})


test_that("Bioenergetic configuration methods work", {
  species_params <- list(
    consumption = list(CEQ = 1, CA = 0.303, CB = 0.275, CQ = 0.1),
    predator = list(PREDEDEQ = 1, ED_ini = 4500, ED_end = 4500)
  )
  
  # Suprimir warning esperado para species_info
  expect_warning(
    bio_obj <- Bioenergetic(species_params = species_params),
    "species_info is NULL"
  )
  
  # Test set_environment
  temp_data <- data.frame(Day = 1:50, Temperature = 15 + 3*sin(1:50/10))
  bio_obj <- set_environment.Bioenergetic(bio_obj, temp_data)
  
  expect_equal(nrow(bio_obj$environmental_data$temperature), 50)
  expect_equal(bio_obj$environmental_data$duration, 50)
  expect_false(bio_obj$fitted)
  
  # Test set_diet
  diet_props <- data.frame(Day = 1:50, fish = 0.7, invertebrates = 0.3)
  prey_energy <- data.frame(Day = 1:50, fish = 5200, invertebrates = 2800)
  
  bio_obj <- set_diet.Bioenergetic(bio_obj, diet_props, prey_energy)
  
  expect_equal(length(bio_obj$diet_data$prey_names), 2)
  expect_true("fish" %in% bio_obj$diet_data$prey_names)
  expect_true("invertebrates" %in% bio_obj$diet_data$prey_names)
  expect_false(bio_obj$fitted)
  
  # Test set_simulation_settings
  bio_obj <- set_simulation_settings.Bioenergetic(bio_obj, initial_weight = 12, duration = 50)
  
  expect_equal(bio_obj$simulation_settings$initial_weight, 12)
  expect_equal(bio_obj$simulation_settings$duration, 50)
  
  # Test set_model_options - expect warning for override
  expect_warning(
    bio_obj <- set_model_options.Bioenergetic(bio_obj, 
                                              calc_mortality = TRUE, 
                                              calc_reproduction = FALSE,
                                              output_daily = TRUE),
    "Overriding calc_mortality"
  )
  
  expect_true(bio_obj$model_options$calc_mortality)
  expect_false(bio_obj$model_options$calc_reproduction)
  expect_true(bio_obj$model_options$output_daily)
})
