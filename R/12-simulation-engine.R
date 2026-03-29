#' Simulation Engine for FB4 Model
#'
#' @name simulation-engine
#' @aliases simulation-engine
NULL

# ============================================================================
# LOW-LEVEL FUNCTIONS: Daily Calculations
# ============================================================================

#' Calculate daily consumption with multiple methods (Low-level)
#'
#' @description
#' Calculates daily consumption using different input methods (p_value, ration, etc.)
#' and returns standardized consumption metrics.
#'
#' @param current_weight Current fish weight (g)
#' @param temperature Water temperature (°C)
#' @param p_value Proportion of maximum consumption (0-5) - for p_value method
#' @param ration_percent Ration as percentage of body weight - for ration_percent method
#' @param ration_grams Ration in grams per day - for ration_grams method
#' @param method Calculation method ("p_value", "ration_percent", "ration_grams")
#' @param processed_consumption_params Processed consumption parameters
#' @param mean_prey_energy Mean prey energy density (J/g)
#' @return List with specific and energetic consumption plus effective p_value
#' @keywords internal
calculate_daily_consumption <- function(current_weight, temperature, 
                                        p_value = NULL, ration_percent = NULL, 
                                        ration_grams = NULL, method = "p_value",
                                        processed_consumption_params, mean_prey_energy) {
  
  # Calculate maximum consumption (g prey/g fish/day)
  max_consumption_gg <- calculate_consumption(
    temperature = temperature,
    weight = current_weight,
    p_value = 1.0,
    processed_consumption_params = processed_consumption_params,
    method = "maximum"
  )
  
  # Calculate consumption based on method
  if (method == "ration_percent") {
    # Ration as % of body weight
    consumption_gg <- ration_percent / 100  # Convert to g prey/g fish
    consumption_energy <- consumption_gg * mean_prey_energy  # J/g fish
    
    # Calculate equivalent p_value for compatibility
    effective_p <- if (max_consumption_gg > 0) consumption_gg / max_consumption_gg else 0
    effective_p <- clamp(effective_p, 0, 5, param_name = "effective_p")
    
  } else if (method == "ration_grams") {
    # Ration as absolute grams per day
    consumption_gg <- ration_grams / current_weight  # Convert to g prey/g fish
    consumption_energy <- consumption_gg * mean_prey_energy  # J/g fish
    
    # Calculate equivalent p_value
    effective_p <- if (max_consumption_gg > 0) consumption_gg / max_consumption_gg else 0
    effective_p <- clamp(effective_p, 0, 5, param_name = "effective_p")
    
  } else {  # method == "p_value"
    # Use p_value directly with existing consumption function
    effective_p <- clamp(p_value, 0, 5, param_name = "p_value")
    consumption_gg <- calculate_consumption(
      temperature = temperature,
      weight = current_weight,
      p_value = effective_p,
      processed_consumption_params = processed_consumption_params,
      method = "rate"
    )
    consumption_energy <- consumption_gg * mean_prey_energy
  }
  
  return(list(
    consumption_gg = pmax(0, consumption_gg),
    consumption_energy = pmax(0, consumption_energy),
    effective_p = effective_p,
    max_consumption_gg = max_consumption_gg
  ))
}

#' Calculate daily metabolic processes (Low-level)
#'
#' @description
#' Calculates egestion, excretion, respiration, and SDA for a single day.
#' Uses existing mid-level functions from other modules.
#'
#' @param consumption_energy Energy consumption (J/g fish/day)
#' @param current_weight Current weight (g)
#' @param temperature Temperature (°C)
#' @param effective_p Effective p_value for the day
#' @param processed_species_params All processed species parameters
#' @param diet_proportions Diet proportions for the day
#' @param indigestible_fractions Indigestible fractions for the day
#' @param oxycal Oxycalorific coefficient (J/g O2)
#' @return List with all metabolic fluxes
#' @keywords internal
calculate_daily_metabolism <- function(consumption_energy, current_weight, temperature,
                                       effective_p, processed_species_params, 
                                       diet_proportions = NULL, indigestible_fractions = NULL,
                                       oxycal = 13560) {
  
  # Calculate total indigestible fraction if diet data provided
  processed_species_params$egestion$indigestible_fraction <- 0
  if (!is.null(diet_proportions) && !is.null(indigestible_fractions)) {
    processed_species_params$egestion$indigestible_fraction <- sum(diet_proportions * indigestible_fractions, na.rm = TRUE)
  }
  
  # Calculate egestion using existing function
  egestion_energy <- calculate_egestion(
    consumption = consumption_energy,
    temperature = temperature,
    p_value = effective_p,
    processed_egestion_params = processed_species_params$egestion
  )
  
  # Calculate excretion using existing function
  excretion_energy <- calculate_excretion(
    consumption = consumption_energy,
    egestion = egestion_energy,
    temperature = temperature,
    p_value = effective_p,
    processed_excretion_params = processed_species_params$excretion
  )
  
  # Calculate respiration using existing function
  respiration_o2 <- calculate_respiration(
    temperature = temperature,
    weight = current_weight,
    processed_respiration_params = processed_species_params$respiration
  )
  
  # Convert respiration to energy units
  respiration_energy <- convert_respiration_to_energy(respiration_o2, oxycal)
  
  # Calculate SDA using existing function
  sda_energy <- calculate_sda(
    consumption_energy = consumption_energy,
    egestion_energy = egestion_energy,
    SDA_coeff = processed_species_params$respiration$SDA %||% 0.15
  )
  
  # Calculate net energy available for growth
  net_energy <- consumption_energy - egestion_energy - excretion_energy - respiration_energy - sda_energy
  
  return(list(
    egestion_energy = egestion_energy,
    excretion_energy = excretion_energy,
    respiration_energy = respiration_energy,
    respiration_o2 = respiration_o2,
    sda_energy = sda_energy,
    net_energy = net_energy,
    total_indigestible_fraction = processed_species_params$egestion$indigestible_fraction
  ))
}

#' Calculate reproductive energy loss for the day (Low-level)
#'
#' @description
#' Calculates energy lost to reproduction on a given day based on
#' reproduction data and current fish condition.
#'
#' @param day Current simulation day
#' @param current_weight Current fish weight (g)
#' @param predator_ed Current predator energy density (J/g)
#' @param reproduction_data Vector with reproduction fractions by day
#' @return Spawning energy loss (J)
#' @keywords internal
calculate_daily_spawn_energy <- function(day, current_weight, predator_ed, reproduction_data = NULL) {
  
  if (is.null(reproduction_data) || length(reproduction_data) < day) {
    return(0)
  }
  
  spawn_fraction <- reproduction_data[day]
  if (is.na(spawn_fraction) || spawn_fraction <= 0) {
    return(0)
  }
  
  # Use existing function from mortality-reproduction module
  spawn_energy <- calculate_spawn_energy(
    spawn_fraction = spawn_fraction,
    current_weight = current_weight,
    energy_density = predator_ed
  )
  
  return(spawn_energy)
}

#' Calculate daily weight change (Low-level)
#'
#' @description
#' Calculates final weight after growth, using existing predator energy density functions.
#' Ensures biologically realistic weight changes.
#'
#' @param current_weight Initial weight of the day (g)
#' @param net_energy Net energy available (J/g fish/day)
#' @param spawn_energy Energy lost to reproduction (J)
#' @param day Current simulation day
#' @param processed_predator_params Processed predator parameters
#' @return List with final weight and weight change
#' @keywords internal
calculate_daily_weight_change <- function(current_weight, net_energy, spawn_energy, day,
                                          processed_predator_params) {
  
  # Total energy available for the day (J)
  total_energy_gain <- net_energy * current_weight
  
  # Use existing function from predator-energy-density module
  weight_result <- calculate_final_weight_fb4(
    initial_weight = current_weight,
    net_energy = total_energy_gain,
    spawn_energy = spawn_energy,
    processed_predator_params = processed_predator_params,
    day = day
  )
  
  # Ensure minimum viable weight
  final_weight <- pmax(0.01, weight_result$final_weight)
  
  return(list(
    final_weight = final_weight,
    weight_change = weight_result$weight_change,
    energy_density = weight_result$final_energy_density
  ))
}

# ============================================================================
# MID-LEVEL FUNCTIONS: Coordination and Business Logic
# ============================================================================

#' Execute single day simulation (Mid-level)
#'
#' @description
#' Coordinates all daily calculations for one simulation day.
#' Main business logic function that orchestrates low-level calculations.
#'
#' @param day Simulation day number
#' @param current_weight Current fish weight (g)
#' @param consumption_method List with method type and value
#' @param processed_simulation_data Complete processed simulation data
#' @param oxycal Oxycalorific coefficient (J/g O2)
#' @return List with all daily results
#' @keywords internal
execute_daily_simulation <- function(day, current_weight, consumption_method, 
                                     processed_simulation_data, oxycal = 13560) {
  
  # Extract data for this day
  temperature <- processed_simulation_data$temporal_data$temperature[day]
  diet_proportions <- processed_simulation_data$temporal_data$diet_proportions[day, ]
  prey_energies <- processed_simulation_data$temporal_data$prey_energies[day, ]
  indigestible_fractions <- processed_simulation_data$temporal_data$prey_indigestible[day, ]
  reproduction_data <- processed_simulation_data$temporal_data$reproduction
  
  # Calculate mean prey energy
  mean_prey_energy <- sum(diet_proportions * prey_energies, na.rm = TRUE)
  
  # Calculate daily consumption
  consumption_result <- calculate_daily_consumption(
    current_weight = current_weight,
    temperature = temperature,
    p_value = if (consumption_method$type == "p_value") consumption_method$value else NULL,
    ration_percent = if (consumption_method$type == "ration_percent") consumption_method$value else NULL,
    ration_grams = if (consumption_method$type == "ration_grams") consumption_method$value else NULL,
    method = consumption_method$type,
    processed_consumption_params = processed_simulation_data$species_params$consumption,
    mean_prey_energy = mean_prey_energy
  )
  
  # Calculate daily metabolism
  metabolism_result <- calculate_daily_metabolism(
    consumption_energy = consumption_result$consumption_energy,
    current_weight = current_weight,
    temperature = temperature,
    effective_p = consumption_result$effective_p,
    processed_species_params = processed_simulation_data$species_params,
    diet_proportions = diet_proportions,
    indigestible_fractions = indigestible_fractions,
    oxycal = oxycal
  )
  
  # Calculate spawning energy loss
  spawn_energy <- calculate_daily_spawn_energy(
    day = day,
    current_weight = current_weight,
    predator_ed = calculate_predator_energy_density(current_weight, day, 
                                                    processed_simulation_data$species_params$predator),
    reproduction_data = reproduction_data
  )
  
  # Calculate weight change
  growth_result <- calculate_daily_weight_change(
    current_weight = current_weight,
    net_energy = metabolism_result$net_energy,
    spawn_energy = spawn_energy,
    day = day,
    processed_predator_params = processed_simulation_data$species_params$predator
  )
  
  # Combine all daily results
  return(list(
    # Basic metrics
    day = day,
    initial_weight = current_weight,
    final_weight = growth_result$final_weight,
    weight_change = growth_result$weight_change,
    temperature = temperature,
    
    # Consumption
    consumption_gg = consumption_result$consumption_gg,
    consumption_energy = consumption_result$consumption_energy,
    effective_p = consumption_result$effective_p,
    mean_prey_energy = mean_prey_energy,
    
    # Metabolism
    egestion_energy = metabolism_result$egestion_energy,
    excretion_energy = metabolism_result$excretion_energy,
    respiration_energy = metabolism_result$respiration_energy,
    respiration_o2 = metabolism_result$respiration_o2,
    sda_energy = metabolism_result$sda_energy,
    net_energy = metabolism_result$net_energy,
    
    # Additional
    spawn_energy = spawn_energy,
    energy_density = growth_result$energy_density
  ))
}

#' Run complete FB4 simulation (Mid-level - Main function)
#'
#' @description
#' Main simulation function that executes the complete FB4 model day by day.
#' Handles different consumption methods and optional daily output.
#' Performs basic validation of critical parameters.
#'
#' @param consumption_method List with method type and value
#' @param processed_simulation_data Complete processed simulation data from prepare_simulation_data()
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param output_daily Whether to save daily outputs, default TRUE
#' @param verbose Whether to show progress messages, default FALSE
#' @return List with simulation results
#' @export
run_fb4_simulation <- function(consumption_method, processed_simulation_data, 
                               oxycal = 13560, output_daily = TRUE, verbose = FALSE) {
  
  # Basic validation of critical parameters (MID-LEVEL appropriate)
  validate_structure_core(consumption_method, "consumption_method", required_class = "list")
  
  if (!consumption_method$type %in% c("p_value", "ration_percent", "ration_grams")) {
    stop("consumption_method$type must be 'p_value', 'ration_percent', or 'ration_grams'")
  }
  
  # Method-specific validation
  if (consumption_method$type == "p_value") {
    consumption_method$value <- clamp(consumption_method$value, 0.001, 5, param_name = "p_value")
  } else if (consumption_method$type == "ration_percent") {
    consumption_method$value <- clamp(consumption_method$value, 0, 100, param_name = "ration_percent")
  } else if (consumption_method$type == "ration_grams") {
    consumption_method$value <- pmax(0.001, consumption_method$value)
  }
  
  # Extract simulation parameters
  initial_weight <- processed_simulation_data$simulation_settings$initial_weight
  n_days <- processed_simulation_data$temporal_data$duration
  
  # Initialize tracking variables
  current_weight <- initial_weight
  total_consumption_g <- 0
  
  # Initialize daily output if requested
  if (output_daily) {
    daily_results <- vector("list", n_days)
  }
  
  # Progress tracking
  progress_interval <- calculate_progress_interval(n_days)
  
  # Initial message
  if (verbose) {
    message("Starting FB4 simulation: ", n_days, " days, initial weight: ", 
            round(initial_weight, 2), "g")
    message("Method: ", consumption_method$type, " = ", consumption_method$value)
  }
  
  # Main simulation loop
  for (day in 1:n_days) {
    
    # Execute daily simulation
    daily_result <- execute_daily_simulation(
      day = day,
      current_weight = current_weight,
      consumption_method = consumption_method,
      processed_simulation_data = processed_simulation_data,
      oxycal = oxycal
    )
    
    # Update state for next day
    current_weight <- daily_result$final_weight
    total_consumption_g <- total_consumption_g + (daily_result$consumption_gg * daily_result$initial_weight)
    
    # Store daily results if requested
    if (output_daily) {
      daily_results[[day]] <- daily_result
    }
    
    # Progress reporting
    if (verbose && (day %% progress_interval == 0 || day == n_days)) {
      report_simulation_progress(day, n_days, current_weight, initial_weight, 
                                 daily_result$effective_p, daily_result$temperature)
    }
    
    # Check for fish mortality
    if (current_weight <= 0.01) {
      warning("Fish mortality occurred on day ", day, " (weight: ", round(current_weight, 4), "g)")
      break
    }
    
    # Check for unrealistic growth
    if (current_weight > initial_weight * 50) {
      warning("Unrealistic growth detected on day ", day, 
              " (weight: ", round(current_weight, 2), "g, ", 
              round((current_weight/initial_weight - 1) * 100, 1), "% growth)")
    }
  }
  
  # Final summary
  if (verbose) {
    report_simulation_summary(current_weight, initial_weight, total_consumption_g, n_days)
  }
  
  # Prepare results
  results <- list(
    # Summary metrics
    initial_weight = initial_weight,
    final_weight = current_weight,
    weight_change = current_weight - initial_weight,
    relative_growth = (current_weight / initial_weight - 1) * 100,
    total_consumption_g = total_consumption_g,
    total_consumption = total_consumption_g,  # alias for backward compat
    simulation_days = day,  # Actual days simulated (may be less if mortality)
    
    # Method information
    method = consumption_method,
    
    # Simulation metadata
    simulation_completed = (day == n_days),
    mortality_occurred = (current_weight <= 0.01)
  )
  
  # Add daily data if requested
  if (output_daily && length(daily_results) > 0) {
    results$daily_output <- convert_daily_results_to_dataframe(daily_results)
  }
  
  return(results)
}

# ============================================================================
# UTILITY FUNCTIONS: Progress and Output Management
# ============================================================================

#' Calculate appropriate progress reporting interval
#' @keywords internal
calculate_progress_interval <- function(n_days) {
  if (n_days > 1000) return(100)
  if (n_days > 200) return(50)
  if (n_days > 50) return(10)
  if (n_days > 10) return(5)
  return(1)
}

#' Report simulation progress
#' @keywords internal
report_simulation_progress <- function(day, n_days, current_weight, initial_weight, effective_p, temperature) {
  growth_rate <- ((current_weight / initial_weight - 1) * 100)
  message("Day ", day, "/", n_days, ": Weight = ", round(current_weight, 2), 
          "g (", round(growth_rate, 1), "% growth), P = ", 
          round(effective_p, 3), ", Temp = ", round(temperature, 1), "\u00b0C")
}

#' Report final simulation summary
#' @keywords internal
report_simulation_summary <- function(final_weight, initial_weight, total_consumption, n_days) {
  final_growth <- ((final_weight / initial_weight - 1) * 100)
  message("Simulation completed successfully")
  message("Final weight: ", round(final_weight, 2), "g (", round(final_growth, 1), "% growth)")
  message("Total consumption: ", round(total_consumption, 2), "g")
  message("Duration: ", n_days, " days")
}

#' Convert list of daily results to data frame
#' @keywords internal
convert_daily_results_to_dataframe <- function(daily_results) {
  
  # Extract vectors from daily results
  n_days <- length(daily_results)
  
  data.frame(
    Day = vapply(daily_results, `[[`, numeric(1), "day"),
    Weight = vapply(daily_results, `[[`, numeric(1), "final_weight"),
    Weight_change = vapply(daily_results, `[[`, numeric(1), "weight_change"),
    Temperature = vapply(daily_results, `[[`, numeric(1), "temperature"),
    Consumption_gg = vapply(daily_results, `[[`, numeric(1), "consumption_gg"),
    Consumption_energy = vapply(daily_results, `[[`, numeric(1), "consumption_energy"),
    P_value = vapply(daily_results, `[[`, numeric(1), "effective_p"),
    Respiration = vapply(daily_results, `[[`, numeric(1), "respiration_energy"),
    Egestion = vapply(daily_results, `[[`, numeric(1), "egestion_energy"),
    Excretion = vapply(daily_results, `[[`, numeric(1), "excretion_energy"),
    SDA = vapply(daily_results, `[[`, numeric(1), "sda_energy"),
    Net_energy = vapply(daily_results, `[[`, numeric(1), "net_energy"),
    Energy_density = vapply(daily_results, `[[`, numeric(1), "energy_density"),
    stringsAsFactors = FALSE
  )
}