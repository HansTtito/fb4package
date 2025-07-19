# ============================================================================
# COMPLETE SIMULATION TESTS
# ============================================================================

test_that("Complete FB4 simulation works", {
  # Create a complete test case
  species_params <- list(
    consumption = list(
      CEQ = 1, CA = 0.303, CB = 0.275, CQ = 0.1
    ),
    respiration = list(
      REQ = 1, RA = 0.0548, RB = 0.299, RQ = 0.1,
      RTO = 5, RTM = 25, RTL = 35, RK1 = 2, RK4 = -0.2, RK5 = 0.08
    ),
    activity = list(
      ACT = 1.5, BACT = 0.05
    ),
    sda = list(
      SDA = 0.1
    ),
    egestion = list(
      EGEQ = 1, FA = 0.2
    ),
    excretion = list(
      EXEQ = 1, UA = 0.1
    ),
    predator = list(
      PREDEDEQ = 1, ED_ini = 4500, ED_end = 4500
    )
  )
  
  species_info <- list(
    scientific_name = "Salmo salar",
    common_name = "Atlantic salmon"
  )
  
  # Environmental data
  n_days <- 30
  environmental_data <- list(
    temperature = data.frame(
      Day = 1:n_days, 
      Temperature = 15 + 5*sin(1:n_days/10)
    )
  )
  
  # Diet data
  diet_data <- list(
    proportions = data.frame(
      Day = 1:n_days, 
      fish = 0.6, 
      zooplankton = 0.4
    ),
    energies = data.frame(
      Day = 1:n_days, 
      fish = 5000, 
      zooplankton = 3000
    ),
    indigestible = data.frame(
      Day = 1:n_days, 
      fish = 0.05, 
      zooplankton = 0.02
    ),
    prey_names = c("fish", "zooplankton")
  )
  
  simulation_settings <- list(
    initial_weight = 10,
    duration = n_days
  )
  
  # Create bioenergetic object
  bio_obj <- Bioenergetic(
    species_params = species_params,
    species_info = species_info,
    environmental_data = environmental_data,
    diet_data = diet_data,
    simulation_settings = simulation_settings
  )
  
  # Test simulation with p-value
  result_p <- run_fb4.Bioenergetic(
    x = bio_obj,
    fit_to = "p-value",
    fit_value = 0.5,
    output_daily = TRUE
  )
  
  expect_true(inherits(result_p, "fb4_result"))
  expect_true(result_p$fit_info$fit_successful)
  expect_equal(result_p$summary$initial_weight, 10)
  expect_gt(result_p$summary$final_weight, 0)
  expect_true(!is.null(result_p$daily_output))
  expect_equal(nrow(result_p$daily_output), n_days)
  
  # Check daily output structure
  expected_cols <- c("Day", "Weight", "Temperature", "Consumption_gg", 
                     "Consumption_energy", "Respiration", "Egestion", 
                     "Excretion", "SDA", "Net_energy", "P_value")
  expect_true(all(expected_cols %in% names(result_p$daily_output)))
  
  # Test simulation with ration percent
  result_ration <- run_fb4.Bioenergetic(
    x = bio_obj,
    fit_to = "Ration",
    fit_value = 2.5,  # 2.5% of body weight per day
    output_daily = FALSE
  )
  
  expect_true(inherits(result_ration, "fb4_result"))
  expect_true(result_ration$fit_info$fit_successful)
  expect_true(is.null(result_ration$daily_output))
  
  # Test simulation with weight fitting
  expect_warning(
  result_weight <- run_fb4.Bioenergetic(
    x = bio_obj,
    fit_to = "Weight",
    fit_value = 15,  # Target final weight
    output_daily = TRUE,
    tolerance = 0.01,
    max_iterations = 15
  ),
  "Model fitting did not converge|Binary search did not achieve"
  )
  
  expect_true(inherits(result_weight, "fb4_result"))
  expect_false(result_weight$fit_info$fit_successful)

})

test_that("FB4 input validation works", {
  # Create minimal valid object
  species_params <- list(
    consumption = list(CEQ = 1, CA = 0.303, CB = -0.275, CQ = 0.1),
    predator = list(PREDEDEQ = 1, ED_ini = 4500, ED_end = 4500)
  )
  
  bio_obj <- Bioenergetic(species_params = species_params)
  
  # Test invalid fit_to values
  expect_error(
    validate_fb4_inputs(bio_obj, fit_to = "Invalid", fit_value = 10),
    "fit_to must be one of"
  )
  
  # Test missing fit_value
  expect_error(
    validate_fb4_inputs(bio_obj, fit_to = "Weight", fit_value = NULL),
    "fit_value must be a positive number"
  )
  
  # Test negative fit_value
  expect_error(
    validate_fb4_inputs(bio_obj, fit_to = "Weight", fit_value = -5),
    "fit_value must be a positive number"
  )
  
  # Test invalid day ranges
  expect_error(
    validate_fb4_inputs(bio_obj, first_day = 0),
    "first_day must be >= 1"
  )
  
  expect_error(
    validate_fb4_inputs(bio_obj, first_day = 10, last_day = 5),
    "first_day must be less than last_day"
  )
})

test_that("Data processing functions work correctly", {
  # Create test bioenergetic object with complete data
  species_params <- list(
    predator = list(PREDEDEQ = 1, ED_ini = 4500, ED_end = 5000)
  )
  
  environmental_data <- list(
    temperature = data.frame(Day = 1:100, Temperature = 15 + 5*sin(1:100/20))
  )
  
  diet_data <- list(
    proportions = data.frame(Day = 1:100, fish = 0.6, zooplankton = 0.4),
    energies = data.frame(Day = 1:100, fish = 5000, zooplankton = 3000),
    indigestible = data.frame(Day = 1:100, fish = 0.05, zooplankton = 0.02),
    prey_names = c("fish", "zooplankton")
  )
  
  simulation_settings <- list(initial_weight = 10)
  
  bio_obj <- Bioenergetic(
    species_params = species_params,
    environmental_data = environmental_data,
    diet_data = diet_data,
    simulation_settings = simulation_settings
  )
  
  # Test data processing
  processed_data <- process_bioenergetic_data(bio_obj, first_day = 1, last_day = 50)
  
  expect_true(is.list(processed_data))
  expect_equal(length(processed_data$temperature), 50)
  expect_equal(nrow(processed_data$diet_proportions), 50)
  expect_equal(ncol(processed_data$diet_proportions), 2)  # fish, zooplankton
  expect_equal(nrow(processed_data$prey_energies), 50)
  expect_equal(length(processed_data$predator_energy_density), 50)
  expect_equal(length(processed_data$reproduction), 50)
  expect_equal(processed_data$duration, 50)
  
  # Check diet normalization
  row_sums <- rowSums(processed_data$diet_proportions)
  expect_true(all(abs(row_sums - 1) < 0.01))  # Should sum to 1
  
  # Check energy values are positive
  expect_true(all(processed_data$prey_energies > 0))
  expect_true(all(processed_data$predator_energy_density > 0))
  
  # Test error cases
  expect_error(
    process_bioenergetic_data(bio_obj, first_day = 50, last_day = 10),
    "first_day must be less than last_day"
  )
})

# ============================================================================
# INTEGRATION TESTS
# ============================================================================

test_that("Complete workflow integration test", {
  # This test simulates a complete user workflow
  
  # Step 1: Create species parameters
  species_params <- list(
    consumption = list(
      CEQ = 2, CA = 0.303, CB = -0.275, CQ = 2.0, 
      CTM = 25, CTO = 15, CX = 1.5
    ),
    respiration = list(
      REQ = 2, RA = 0.0548, RB = -0.299, RQ = 2.0,
      RTM = 28, RTO = 20, RX = 1.2
    ),
    activity = list(ACT = 1.0),
    sda = list(SDA = 0.1),
    egestion = list(EGEQ = 2, FA = 0.158, FB = -0.222, FG = 0.631),
    excretion = list(EXEQ = 2, UA = 0.0314, UB = 0.58, UG = -0.299),
    predator = list(PREDEDEQ = 2, Alpha1 = 3000, Beta1 = 10, 
                    Alpha2 = 4000, Beta2 = 5, Cutoff = 100)
  )
  
  species_info <- list(
    scientific_name = "Oncorhynchus mykiss",
    common_name = "Rainbow trout",
    family = "Salmonidae",
    life_stage = "juvenile"
  )
  
  # Step 2: Create bioenergetic object
  bio_obj <- Bioenergetic(
    species_params = species_params,
    species_info = species_info
  )
  
  expect_true(is.Bioenergetic(bio_obj))
  
  # Step 3: Add environmental data
  n_days <- 60
  temp_data <- data.frame(
    Day = 1:n_days,
    Temperature = 15 + 8*sin(2*pi*(1:n_days)/365)
  )
  
  diet_data <- data.frame(Day = 1:n_days, fish = 0.6, zooplankton = 0.4)
  energy_data <- data.frame(Day = 1:n_days, fish = 5000, zooplankton = 3000)
  
  bio_obj <- set_environment.Bioenergetic(bio_obj, temp_data)
  bio_obj <- set_diet.Bioenergetic(bio_obj, diet_data, energy_data)
  bio_obj <- set_simulation_settings.Bioenergetic(bio_obj, initial_weight = 20, duration = n_days)
  
  # Time the simulation
  start_time <- Sys.time()
  
  result <- run_fb4.Bioenergetic(
    x = bio_obj,
    fit_to = "p-value",
    fit_value = 0.5,
    output_daily = TRUE
  )
  
  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Performance expectations (adjust based on system capabilities)
  expect_lt(execution_time, 5)  # Should complete within 5 seconds
  expect_true(result$fit_info$fit_successful)
  
  # Test fitting performance (more intensive)
  start_time_fit <- Sys.time()
  
  result_fit <- run_fb4.Bioenergetic(
    x = bio_obj,
    fit_to = "Weight",
    fit_value = 50,
    output_daily = FALSE,  # Faster without daily output
    tolerance = 0.1,
    max_iterations = 15
  )
  
  end_time_fit <- Sys.time()
  execution_time_fit <- as.numeric(difftime(end_time_fit, start_time_fit, units = "secs"))
  
  expect_lt(execution_time_fit, 10)  # Fitting should complete within 10 seconds
  expect_true(result_fit$fit_info$fit_successful)
  expect_lte(result_fit$fit_info$iterations, 15)
})

# ============================================================================
# NUMERICAL STABILITY TESTS
# ============================================================================

test_that("Numerical stability with extreme conditions", {
  # Test with very small fish
  tiny_fish_params <- list(
    consumption = list(CEQ = 1, CA = 0.303, CB = 0.275, CQ = 0.1),
    respiration = list(REQ = 1, RA = 0.0548, RB = -0.299, RQ = 3.5, RTO = 28, RTM = 35, RTL = 0.131, RK1 = 0.812, RK4 = 0.141, RK5 = 0.422),
    activity = list(ACT = 1.0, BACT = 2.12),
    sda = list(SDA = 0.1),
    egestion = list(EGEQ = 1, FA = 0.2),
    excretion = list(EXEQ = 1, UA = 0.1),
    predator = list(PREDEDEQ = 1, ED_ini = 4500, ED_end = 4500)
  )
  
  bio_obj_tiny <- Bioenergetic(species_params = tiny_fish_params)
  
  temp_data <- data.frame(Day = 1:10, Temperature = rep(15, 10))
  diet_data <- data.frame(Day = 1:10, prey = 1.0)
  energy_data <- data.frame(Day = 1:10, prey = 4000)
  
  bio_obj_tiny <- set_environment.Bioenergetic(bio_obj_tiny, temp_data)
  bio_obj_tiny <- set_diet.Bioenergetic(bio_obj_tiny, diet_data, energy_data)
  bio_obj_tiny <- set_simulation_settings.Bioenergetic(bio_obj_tiny, 
                                                       initial_weight = 0.1,  # Very small
                                                       duration = 10)
  
  
   result_tiny <- run_fb4.Bioenergetic(
    x = bio_obj_tiny,
    fit_to = "p-value",
    fit_value = 0.5,
    output_daily = TRUE
  )
  
  expect_true(result_tiny$fit_info$fit_successful)
  expect_gt(result_tiny$summary$final_weight, 0.01)  # Should stay alive
  expect_true(all(is.finite(result_tiny$daily_output$Weight)))
  
  # Test with very large fish
  bio_obj_large <- bio_obj_tiny
  bio_obj_large <- set_simulation_settings.Bioenergetic(bio_obj_large, 
                                                        initial_weight = 100,  # Very large
                                                        duration = 10)
  
  result_large <- run_fb4.Bioenergetic(
    x = bio_obj_large,
    fit_to = "p-value",
    fit_value = 0.1,  # Low feeding to avoid overflow
    output_daily = TRUE
  )
  
  expect_true(result_large$fit_info$fit_successful)
  expect_gt(result_large$summary$final_weight, 100)
  expect_true(all(is.finite(result_large$daily_output$Weight)))
  
  # Test with extreme temperatures
  extreme_temp_data <- data.frame(Day = 1:10, Temperature = c(2, 4, 30, 35, 1, 40, 5, 25, 15, 20))
  bio_obj_temp <- bio_obj_tiny
  bio_obj_temp <- set_environment.Bioenergetic(bio_obj_temp, extreme_temp_data)
  bio_obj_temp <- set_simulation_settings.Bioenergetic(bio_obj_temp, 
                                                       initial_weight = 50, 
                                                       duration = 10)
  
  result_temp <- run_fb4.Bioenergetic(
    x = bio_obj_temp,
    fit_to = "p-value",
    fit_value = 0.3,
    output_daily = TRUE
  )
  
  expect_true(result_temp$fit_info$fit_successful)
  expect_true(all(is.finite(result_temp$daily_output$Weight)))
  expect_true(all(is.finite(result_temp$daily_output$Consumption_gg)))
  expect_true(all(is.finite(result_temp$daily_output$Respiration)))
})

test_that("Mathematical edge cases are handled correctly", {
  # Test division by zero protection
  expect_true(is.na(safe_exp(-Inf)))
  expect_true(is.na(safe_exp(NaN)))
  
  # Test square root protection
  expect_equal(safe_sqrt(-100), 0)
  expect_equal(safe_sqrt(-100, min_val = 5), 5)
  expect_true(is.na(safe_sqrt(NaN)))
  
  # Test clamp with edge values
  expect_equal(clamp(Inf, 0, 1), 1)
  expect_equal(clamp(-Inf, 0, 1), 0)
  expect_true(is.na(clamp(NaN, 0, 1)))
  
  # Test with zero consumption scenarios
  metabolism_zero <- calculate_daily_metabolism(
    consumption_energy = 0,
    current_weight = 100,
    temperature = 15,
    p_value = 0,
    species_params = list(
      respiration = list(REQ = 1, RA = 0.0548, RB = -0.299, RQ = 3.5, RTO = 28, RTM = 35, RTL = 0.131, RK1 = 0.812, RK4 = 0.141, RK5 = 0.422),
      activity = list(ACT = 1.0, BACT = 0.12),
      sda = list(SDA = 0.1),
      egestion = list(EGEQ = 1, FA = 0.2),
      excretion = list(EXEQ = 1, UA = 0.1)
    ),
    oxycal = 13560
  )
  
  expect_equal(metabolism_zero$egestion_energy, 0)
  expect_equal(metabolism_zero$excretion_energy, 0)
  expect_gt(metabolism_zero$respiration_energy, 0)  # Still respires
  expect_lt(metabolism_zero$net_energy, 0)  # Should be negative (starvation)
})

# ============================================================================
# REPRODUCIBILITY TESTS
# ============================================================================

test_that("Results are reproducible with identical inputs", {
  # Create identical test setups
  create_test_bio_obj <- function() {
    species_params <- list(
      consumption = list(CEQ = 1, CA = 0.303, CB = -0.275, CQ = 0.1),
      respiration = list(REQ = 1, RA = 0.0548, RB = -0.299, RQ = 3.5, RTO = 28, RTM = 35, RTL = 0.131, RK1 = 0.812, RK4 = 0.141, RK5 = 0.422),
      activity = list(ACT = 1.0, BACT = 0.12),
      sda = list(SDA = 0.1),
      egestion = list(EGEQ = 1, FA = 0.2),
      excretion = list(EXEQ = 1, UA = 0.1),
      predator = list(PREDEDEQ = 1, ED_ini = 4500, ED_end = 4500)
    )
    
    bio_obj <- Bioenergetic(species_params = species_params)
    
    temp_data <- data.frame(Day = 1:20, Temperature = 15 + 3*sin(1:20/5))
    diet_data <- data.frame(Day = 1:20, prey = 1.0)
    energy_data <- data.frame(Day = 1:20, prey = 4500)
    
    bio_obj <- set_environment.Bioenergetic(bio_obj, temp_data)
    bio_obj <- set_diet.Bioenergetic(bio_obj, diet_data, energy_data)
    bio_obj <- set_simulation_settings.Bioenergetic(bio_obj, initial_weight = 15, duration = 20)
    
    return(bio_obj)
  }
  
  # Run identical simulations
  bio_obj_1 <- create_test_bio_obj()
  bio_obj_2 <- create_test_bio_obj()
  
  result_1 <- run_fb4.Bioenergetic(bio_obj_1, fit_to = "p-value", fit_value = 0.6, output_daily = TRUE)
  result_2 <- run_fb4.Bioenergetic(bio_obj_2, fit_to = "p-value", fit_value = 0.6, output_daily = TRUE)
  
  # Results should be identical
  expect_equal(result_1$summary$final_weight, result_2$summary$final_weight)
  expect_equal(result_1$summary$total_consumption, result_2$summary$total_consumption)
  expect_equal(result_1$daily_output$Weight, result_2$daily_output$Weight)
  expect_equal(result_1$daily_output$Consumption_gg, result_2$daily_output$Consumption_gg)
  
  # Test fitting reproducibility
  result_fit_1 <- run_fb4.Bioenergetic(bio_obj_1, fit_to = "Weight", fit_value = 25, 
                                       tolerance = 0.01, max_iterations = 15)
  result_fit_2 <- run_fb4.Bioenergetic(bio_obj_2, fit_to = "Weight", fit_value = 25, 
                                       tolerance = 0.01, max_iterations = 15)
  
  expect_equal(result_fit_1$fit_info$p_value, result_fit_2$fit_info$p_value, tolerance = 1e-6)
  expect_equal(result_fit_1$summary$final_weight, result_fit_2$summary$final_weight, tolerance = 1e-6)
})

# ============================================================================
# DOCUMENTATION AND EXAMPLE TESTS
# ============================================================================

test_that("Documentation examples work correctly", {
  # Test examples from function documentation
  
  # Example from Bioenergetic constructor documentation
  params <- list(
    consumption = list(CA = 0.303, CB = -0.275, CQ = 3, CEQ = 1),
    respiration = list(RA = 0.0548, RB = -0.299, RQ = 2, REQ = 1),
    predator = list(PREDEDEQ = 1, ED_ini = 4500, ED_end = 4500)
  )
  
  species_info <- list(
    scientific_name = "Salmo salar",
    common_name = "Atlantic salmon",
    life_stage = "juvenile"
  )
  
  bio_obj <- Bioenergetic(
    species_params = params,
    species_info = species_info,
    simulation_settings = list(initial_weight = 10, duration = 365)
  )
  
  expect_true(is.Bioenergetic(bio_obj))
  expect_equal(bio_obj$species_info$scientific_name, "Salmo salar")
  expect_equal(bio_obj$simulation_settings$initial_weight, 10)
  
  # Test utility function examples
  expect_equal(NULL %||% "default", "default")
  expect_equal("value" %||% "default", "value")
  
  expect_equal(safe_sqrt(4), 2)
  expect_equal(safe_sqrt(-1), 0)
  
  expect_equal(clamp(1.5, 0, 1), 1.0)
  expect_equal(clamp(-0.5, 0, 1), 0.0)
  expect_equal(clamp(0.3, 0, 1), 0.3)
})

