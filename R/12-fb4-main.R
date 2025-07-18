# ============================================================================
# MAIN FB4 EXECUTION FUNCTIONS - CLEAN VERSION
# ============================================================================

#' Execute Fish Bioenergetics 4.0 Model with Bioenergetic Object
#'
#' @description
#' Main function to execute Fish Bioenergetics 4.0 model simulations
#' using a Bioenergetic class object that already contains all necessary parameters.
#'
#' @param bio_obj Object of class 'Bioenergetic' with configured parameters and data
#' @param fit_to Fitting method: "Weight", "Consumption", "p-value", "Ration", "Ration_prey"
#' @param fit_value Target value according to the fitting method
#' @param first_day First simulation day (default 1)
#' @param last_day Last simulation day (default uses complete duration)
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param output_daily Logical, whether to include daily output, default TRUE
#' @param tolerance Tolerance for iterative fitting, default 0.001
#' @param max_iterations Maximum number of iterations, default 25
#' @param ... Additional arguments
#'
#' @return Object of class 'fb4_result' with simulation results
#' @export
run_fb4.Bioenergetic <- function(bio_obj,
                                 fit_to = "Weight",
                                 fit_value,
                                 first_day = 1,
                                 last_day = NULL,
                                 oxycal = 13560,
                                 output_daily = TRUE,
                                 tolerance = 0.001,
                                 max_iterations = 25,
                                 ...) {
  
  # Validate Bioenergetic object
  if (!inherits(bio_obj, "Bioenergetic")) {
    stop("bio_obj must be an object of class 'Bioenergetic'")
  }
  
  # Extract model options
  model_options <- bio_obj$model_options %||% list()
  
  # Extract simulation parameters
  initial_weight <- bio_obj$simulation_settings$initial_weight
  if (is.null(last_day)) {
    last_day <- bio_obj$simulation_settings$duration %||% 
      max(bio_obj$environmental_data$temperature$Day)
  }
  
  # Record start time
  start_time <- proc.time()
  
  # Comprehensive validation
  validate_fb4_inputs(bio_obj, fit_to, fit_value, first_day, last_day)
  
  # Process input data from Bioenergetic object
  processed_data <- process_bioenergetic_data(bio_obj, first_day, last_day)
  
  # Execute simulation based on method
  if (fit_to %in% c("Weight", "Consumption")) {
    result <- fit_fb4_binary_search(
      species_params = bio_obj$species_params,
      initial_weight = initial_weight,
      fit_to = tolower(fit_to),
      fit_value = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      tolerance = tolerance,
      max_iterations = max_iterations,
      output_daily = output_daily
    )
    
  } else if (fit_to == "p-value") {
    result <- run_fb4_with_p_value(
      species_params = bio_obj$species_params,
      initial_weight = initial_weight,
      p_value = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      output_daily = output_daily
    )
    
  } else if (fit_to == "Ration") {
    result <- run_fb4_with_ration_percent(
      species_params = bio_obj$species_params,
      initial_weight = initial_weight,
      ration_percent = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      output_daily = output_daily
    )
    
  } else if (fit_to == "Ration_prey") {
    result <- run_fb4_with_ration_grams(
      species_params = bio_obj$species_params,
      initial_weight = initial_weight,
      ration_grams = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      output_daily = output_daily
    )
    
  } else {
    stop("fit_to must be 'Weight', 'Consumption', 'p-value', 'Ration', or 'Ration_prey'")
  }
  
  # Calculate elapsed time
  elapsed_time <- proc.time() - start_time
  
  # Prepare final result
  final_result <- list(
    daily_output = if(output_daily) result$daily_output else NULL,
    summary = list(
      initial_weight = initial_weight,
      final_weight = result$final_weight,
      total_consumption = result$total_consumption,
      simulation_days = processed_data$duration,
      p_value = result$p_value %||% result$fit_info$p_value
    ),
    fit_info = result$fit_info,
    model_info = list(
      version = "1.2.0",
      execution_time = elapsed_time[3],
      timestamp = Sys.time(),
      fit_successful = result$fit_info$fit_successful %||% TRUE
    ),
    bioenergetic_object = bio_obj
  )
  
  class(final_result) <- "fb4_result"
  return(final_result)
}

#' Generic Function for Fish Bioenergetics 4.0 Model Execution
#'
#' @param x Object to use for simulation (e.g., Bioenergetic object)
#' @param ... Additional arguments passed to specific methods
#' @return Object of class 'fb4_result' with simulation results
#' @export
run_fb4 <- function(x, ...) {
  UseMethod("run_fb4")
}

#' Default Method for run_fb4
#'
#' @param x Unsupported object
#' @param ... Additional arguments
#' @return Throws an error with information about supported classes
#' @export
run_fb4.default <- function(x, ...) {
  stop("No run_fb4 method available for objects of class '", 
       paste(class(x), collapse = "', '"), "'.\n",
       "Supported classes: 'Bioenergetic'")
}

# ============================================================================
# SIMULATION EXECUTION FUNCTIONS
# ============================================================================

#' Execute FB4 Simulation with Specific P-Value
#' @keywords internal
run_fb4_with_p_value <- function(species_params, initial_weight, p_value,
                                 processed_data, model_options, oxycal = 13560,
                                 output_daily = TRUE) {
  
  p_value <- clamp(p_value, 0.001, 4.999)
  
  simulation_result <- run_fb4_simulation_complete(
    initial_weight = initial_weight,
    p_value = p_value,
    species_params = species_params,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    output_daily = output_daily
  )
  
  simulation_result$fit_info <- list(
    p_value = p_value,
    fit_successful = TRUE,
    iterations = 1,
    final_error = 0,
    method = "direct_p_value"
  )
  
  return(simulation_result)
}

#' Execute FB4 with Ration as Percentage of Body Weight
#' @keywords internal
run_fb4_with_ration_percent <- function(species_params, initial_weight, ration_percent,
                                        processed_data, model_options, oxycal = 13560,
                                        output_daily = TRUE) {
  
  if (ration_percent < 0 || ration_percent > 100) {
    stop("ration_percent must be between 0 and 100")
  }
  
  simulation_result <- run_fb4_simulation_with_ration(
    initial_weight = initial_weight,
    ration_type = "percent",
    ration_value = ration_percent / 100,
    species_params = species_params,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    output_daily = output_daily
  )
  
  simulation_result$fit_info <- list(
    ration_percent = ration_percent,
    fit_successful = TRUE,
    iterations = 1,
    final_error = 0,
    method = "ration_percent"
  )
  
  return(simulation_result)
}

#' Execute FB4 with Daily Ration in Grams
#' @keywords internal
run_fb4_with_ration_grams <- function(species_params, initial_weight, ration_grams,
                                      processed_data, model_options, oxycal = 13560,
                                      output_daily = TRUE) {
  
  if (ration_grams <= 0) {
    stop("ration_grams must be greater than 0")
  }
  
  simulation_result <- run_fb4_simulation_with_ration(
    initial_weight = initial_weight,
    ration_type = "grams",
    ration_value = ration_grams,
    species_params = species_params,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    output_daily = output_daily
  )
  
  simulation_result$fit_info <- list(
    ration_grams = ration_grams,
    fit_successful = TRUE,
    iterations = 1,
    final_error = 0,
    method = "ration_grams"
  )
  
  return(simulation_result)
}

# ============================================================================
# CORE SIMULATION FUNCTIONS - SIMPLIFIED
# ============================================================================

#' Complete FB4 Simulation
#' @keywords internal
run_fb4_simulation_complete <- function(initial_weight, p_value, species_params,
                                        processed_data, model_options, oxycal = 13560,
                                        output_daily = TRUE) {
  
  # Extract processed data
  temperature <- processed_data$temperature
  diet_proportions <- processed_data$diet_proportions
  prey_energies <- processed_data$prey_energies
  n_days <- processed_data$duration
  prey_indigestible <- processed_data$prey_indigestible
  
  # Initialize variables
  current_weight <- initial_weight
  total_consumption <- 0
  
  # Initialize daily output if required
  if (output_daily) {
    daily_output <- data.frame(
      Day = 1:n_days,
      Weight = numeric(n_days),
      Temperature = temperature,
      Consumption_gg = numeric(n_days),
      Consumption_energy = numeric(n_days),
      Respiration = numeric(n_days),
      Egestion = numeric(n_days),
      Excretion = numeric(n_days),
      SDA = numeric(n_days),
      Net_energy = numeric(n_days),
      P_value = rep(p_value, n_days),
      stringsAsFactors = FALSE
    )
  }
  
  # Day-by-day simulation
  for (day in 1:n_days) {
    
    # 1. Calculate mean prey energy (inline)
    mean_prey_energy <- sum(diet_proportions[day, ] * prey_energies[day, ], na.rm = TRUE)
    
    # 2. Calculate predator energy density
    predator_ed <- calculate_predator_energy_density(
      weight = current_weight,
      day = day,
      predator_params = species_params$predator
    )
    
    # 3. Daily consumption
    consumption_result <- calculate_daily_consumption(
      current_weight = current_weight,
      temperature = temperature[day],
      p_value = p_value,
      method = "p_value",
      consumption_params = species_params$consumption,
      mean_prey_energy = mean_prey_energy
    )
    
    # 4. Daily metabolism
    metabolism_result <- calculate_daily_metabolism(
      consumption_energy = consumption_result$consumption_energy,
      current_weight = current_weight,
      temperature = temperature[day],
      p_value = consumption_result$effective_p,
      species_params = species_params,
      oxycal = oxycal,
      diet_proportions = diet_proportions[day,],
      indigestible_fractions = prey_indigestible[day,]
    )
    
    # 5. Spawning energy (simplified - no reproduction for now)
    spawn_energy <- calculate_spawn_energy(
      day = day,
      current_weight = current_weight, 
      predator_ed = predator_ed,
      model_options = model_options,
      reproduction_data = processed_data$reproduction
    )
    
    # 6. Daily growth (inline calculation)
    net_energy_total <- metabolism_result$net_energy * current_weight - spawn_energy
    weight_change <- net_energy_total / predator_ed
    current_weight <- pmax(0.01, current_weight + weight_change)
    
    # 7. Save daily data
    if (output_daily) {
      daily_output$Weight[day] <- current_weight
      daily_output$Consumption_gg[day] <- consumption_result$consumption_gg
      daily_output$Consumption_energy[day] <- consumption_result$consumption_energy
      daily_output$Respiration[day] <- metabolism_result$respiration_energy
      daily_output$Egestion[day] <- metabolism_result$egestion_energy
      daily_output$Excretion[day] <- metabolism_result$excretion_energy
      daily_output$SDA[day] <- metabolism_result$sda_energy
      daily_output$Net_energy[day] <- metabolism_result$net_energy
    }
    
    # 8. Update totals
    total_consumption <- total_consumption + (consumption_result$consumption_gg * current_weight)
    
    # 9. Validation
    if (current_weight <= 0) {
      warning("Fish died on day ", day)
      break
    }
  }
  
  # Return results
  result <- list(
    final_weight = current_weight,
    initial_weight = initial_weight,
    total_consumption = total_consumption,
    simulation_days = n_days,
    p_value = p_value
  )
  
  if (output_daily) {
    result$daily_output <- daily_output
  }
  
  return(result)
}

#' FB4 Simulation with Specific Ration
#' @keywords internal
run_fb4_simulation_with_ration <- function(initial_weight, ration_type, ration_value,
                                           species_params, processed_data, model_options,
                                           oxycal = 13560, output_daily = TRUE) {
  
  # Extract processed data
  temperature <- processed_data$temperature
  diet_proportions <- processed_data$diet_proportions
  prey_energies <- processed_data$prey_energies
  n_days <- processed_data$duration
  prey_indigestible <- processed_data$prey_indigestible
  
  # Initialize variables
  current_weight <- initial_weight
  total_consumption <- 0
  p_values <- numeric(n_days)
  
  # Initialize daily output
  if (output_daily) {
    daily_output <- data.frame(
      Day = 1:n_days,
      Weight = numeric(n_days),
      Temperature = temperature,
      Consumption_gg = numeric(n_days),
      Consumption_energy = numeric(n_days),
      Respiration = numeric(n_days),
      Egestion = numeric(n_days),
      Excretion = numeric(n_days),
      SDA = numeric(n_days),
      Net_energy = numeric(n_days),
      P_value = numeric(n_days),
      Ration_value = numeric(n_days),
      stringsAsFactors = FALSE
    )
  }
  
  # Day-by-day simulation
  for (day in 1:n_days) {
    
    # 1. Calculate mean prey energy (inline)
    mean_prey_energy <- sum(diet_proportions[day, ] * prey_energies[day, ], na.rm = TRUE)
    
    # 2. Calculate predator energy density
    predator_ed <- calculate_predator_energy_density(
      weight = current_weight,
      day = day,
      predator_params = species_params$predator
    )
    
    # 3. Daily consumption based on ration
    consumption_result <- calculate_daily_consumption(
      current_weight = current_weight,
      temperature = temperature[day],
      ration_percent = if (ration_type == "percent") ration_value * 100 else NULL,
      ration_grams = if (ration_type == "grams") ration_value else NULL,
      method = if (ration_type == "percent") "ration_percent" else "ration_grams",
      consumption_params = species_params$consumption,
      mean_prey_energy = mean_prey_energy
    )
    
    # 4. Daily metabolism
    metabolism_result <- calculate_daily_metabolism(
      consumption_energy = consumption_result$consumption_energy,
      current_weight = current_weight,
      temperature = temperature[day],
      p_value = consumption_result$effective_p,
      species_params = species_params,
      oxycal = oxycal,
      diet_proportions = diet_proportions[day,],
      indigestible_fractions = prey_indigestible[day,]
    )
    
    # 5. Spawning energy
    spawn_energy <- calculate_spawn_energy(
      day = day,
      current_weight = current_weight,
      predator_ed = predator_ed,
      model_options = model_options,
      reproduction_data = processed_data$reproduction
    )
    
    # 6. Daily growth (inline calculation)
    net_energy_total <- metabolism_result$net_energy * current_weight - spawn_energy
    weight_change <- net_energy_total / predator_ed
    current_weight <- pmax(0.01, current_weight + weight_change)
    
    # 6. Save daily data
    p_values[day] <- consumption_result$effective_p
    
    if (output_daily) {
      daily_output$Weight[day] <- current_weight
      daily_output$Consumption_gg[day] <- consumption_result$consumption_gg
      daily_output$Consumption_energy[day] <- consumption_result$consumption_energy
      daily_output$Respiration[day] <- metabolism_result$respiration_energy
      daily_output$Egestion[day] <- metabolism_result$egestion_energy
      daily_output$Excretion[day] <- metabolism_result$excretion_energy
      daily_output$SDA[day] <- metabolism_result$sda_energy
      daily_output$Net_energy[day] <- metabolism_result$net_energy
      daily_output$P_value[day] <- consumption_result$effective_p
      daily_output$Ration_value[day] <- ration_value
    }
    
    # 7. Update totals
    total_consumption <- total_consumption + (consumption_result$consumption_gg * current_weight)
    
    # 8. Validation
    if (current_weight <= 0) {
      warning("Fish died on day ", day)
      break
    }
  }
  
  # Return results
  result <- list(
    final_weight = current_weight,
    initial_weight = initial_weight,
    total_consumption = total_consumption,
    simulation_days = n_days,
    p_value = mean(p_values, na.rm = TRUE),
    ration_type = ration_type,
    ration_value = ration_value
  )
  
  if (output_daily) {
    result$daily_output <- daily_output
  }
  
  return(result)
}