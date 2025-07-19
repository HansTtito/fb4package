# ============================================================================
# MAIN FB4 EXECUTION FUNCTIONS - OPTIMIZED VERSION
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
    
  } else if (fit_to %in% c("p-value", "Ration", "Ration_prey")) {
    # Use unified function for direct methods
    method_map <- list(
      "p-value" = "p_value",
      "Ration" = "ration_percent", 
      "Ration_prey" = "ration_grams"
    )
    
    result <- run_fb4_with_method(
      species_params = bio_obj$species_params,
      initial_weight = initial_weight,
      method = method_map[[fit_to]],
      value = fit_value,
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
  
  # Update fitted status if successful
  if (result$fit_info$fit_successful) {
    bio_obj$fitted <- TRUE
  }
  
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
# UNIFIED SIMULATION EXECUTION FUNCTION
# ============================================================================

#' Execute FB4 Simulation with Unified Method Handler
#' 
#' @description
#' Unified function that handles all direct simulation methods (p-value, ration_percent, ration_grams)
#' to reduce code duplication and improve maintainability.
#'
#' @param species_params Species parameter list
#' @param initial_weight Initial fish weight (g)
#' @param method Simulation method ("p_value", "ration_percent", "ration_grams")
#' @param value Method-specific value
#' @param processed_data Processed environmental and diet data
#' @param model_options Model configuration options
#' @param oxycal Oxycalorific coefficient (J/g O2)
#' @param output_daily Whether to include daily output
#'
#' @return List with simulation results
#' @keywords internal
run_fb4_with_method <- function(species_params, initial_weight, method, value,
                                processed_data, model_options, oxycal = 13560,
                                output_daily = TRUE) {
  
  # Method-specific validation and consumption parameters setup
  if (method == "p_value") {
    if (!is.numeric(value) || length(value) != 1 || value <= 0) {
      stop("p_value must be a positive number")
    }
    value <- clamp(value, 0.001, 4.999)
    consumption_params <- list(type = "p_value", value = value)
    fit_info_base <- list(p_value = value, method = "direct_p_value")
    
  } else if (method == "ration_percent") {
    if (!is.numeric(value) || length(value) != 1 || value < 0 || value > 100) {
      stop("ration_percent must be between 0 and 100")
    }
    consumption_params <- list(type = "ration_percent", value = value / 100)
    fit_info_base <- list(ration_percent = value, method = "ration_percent")
    
  } else if (method == "ration_grams") {
    if (!is.numeric(value) || length(value) != 1 || value <= 0) {
      stop("ration_grams must be greater than 0")
    }
    consumption_params <- list(type = "ration_grams", value = value)
    fit_info_base <- list(ration_grams = value, method = "ration_grams")
    
  } else {
    stop("Unknown method: ", method, ". Must be 'p_value', 'ration_percent', or 'ration_grams'")
  }
  
  # Execute unified simulation
  simulation_result <- run_fb4_simulation_unified(
    initial_weight = initial_weight,
    consumption_params = consumption_params,
    species_params = species_params,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    output_daily = output_daily
  )
  
  # Add fit info
  fit_info_base$fit_successful <- TRUE
  fit_info_base$iterations <- 1
  fit_info_base$final_error <- 0
  simulation_result$fit_info <- fit_info_base
  
  return(simulation_result)
}

# ============================================================================
# CORE UNIFIED SIMULATION FUNCTION
# ============================================================================

#' Unified FB4 Simulation Engine
#' 
#' @description
#' Core simulation function that runs the complete fish bioenergetics model
#' with flexible consumption parameter handling to eliminate code duplication.
#'
#' @param initial_weight Initial fish weight (g)
#' @param consumption_params List with consumption method and value
#' @param species_params Species parameter list
#' @param processed_data Processed environmental and diet data
#' @param model_options Model configuration options
#' @param oxycal Oxycalorific coefficient (J/g O2)
#' @param output_daily Whether to save daily outputs
#'
#' @return List with simulation results
#' @keywords internal
run_fb4_simulation_unified <- function(initial_weight, consumption_params, species_params,
                                       processed_data, model_options, oxycal = 13560,
                                       output_daily = TRUE) {
  
  # Extract processed data
  temperature <- processed_data$temperature
  diet_proportions <- processed_data$diet_proportions
  prey_energies <- processed_data$prey_energies
  n_days <- processed_data$duration
  prey_indigestible <- processed_data$prey_indigestible
  
  # Initialize simulation variables
  current_weight <- initial_weight
  total_consumption <- 0
  p_values <- numeric(n_days)
  
  # Initialize daily output structure if required
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
      stringsAsFactors = FALSE
    )
    
    # Add method-specific columns
    if (consumption_params$type %in% c("ration_percent", "ration_grams")) {
      daily_output$Ration_value <- numeric(n_days)
    }
  }
  
  # Main simulation loop - day by day
  for (day in 1:n_days) {
    
    # Calculate mean prey energy for this day
    mean_prey_energy <- sum(diet_proportions[day, ] * prey_energies[day, ], na.rm = TRUE)
    
    # Calculate predator energy density for current weight and day
    predator_ed <- calculate_predator_energy_density(
      weight = current_weight,
      day = day,
      predator_params = species_params$predator
    )
    
    # Calculate daily consumption using existing function
    consumption_result <- calculate_daily_consumption(
      current_weight = current_weight,
      temperature = temperature[day],
      p_value = if (consumption_params$type == "p_value") consumption_params$value else NULL,
      ration_percent = if (consumption_params$type == "ration_percent") consumption_params$value * 100 else NULL,
      ration_grams = if (consumption_params$type == "ration_grams") consumption_params$value else NULL,
      method = consumption_params$type,
      consumption_params = species_params$consumption,
      mean_prey_energy = mean_prey_energy
    )
    
    # Calculate daily metabolism using existing function
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
    
    # Calculate spawning energy loss (if reproduction is enabled)
    spawn_energy <- calculate_spawn_energy(
      day = day,
      current_weight = current_weight, 
      predator_ed = predator_ed,
      model_options = model_options,
      reproduction_data = processed_data$reproduction
    )
    
    # Calculate daily growth using existing function
    growth_result <- calculate_daily_growth(
      current_weight = current_weight,
      net_energy = metabolism_result$net_energy,
      spawn_energy = spawn_energy,
      predator_energy_density = predator_ed
    )
    current_weight <- growth_result$final_weight
    
    # Store daily values
    p_values[day] <- consumption_result$effective_p
    
    # Store daily results if requested
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
      
      # Add method-specific values
      if (consumption_params$type %in% c("ration_percent", "ration_grams")) {
        daily_output$Ration_value[day] <- consumption_params$value
      }
    }
    
    # Update cumulative consumption
    total_consumption <- total_consumption + (consumption_result$consumption_gg * current_weight)
    
    # Check for fish mortality
    if (current_weight <= 0) {
      warning("Fish mortality occurred on day ", day)
      break
    }
  }
  
  # Prepare return results
  result <- list(
    final_weight = current_weight,
    initial_weight = initial_weight,
    total_consumption = total_consumption,
    simulation_days = n_days,
    p_value = if (consumption_params$type == "p_value") consumption_params$value else mean(p_values, na.rm = TRUE)
  )
  
  # Add method-specific results
  if (consumption_params$type %in% c("ration_percent", "ration_grams")) {
    result$ration_type <- if (consumption_params$type == "ration_percent") "percent" else "grams"
    result$ration_value <- consumption_params$value
  }
  
  if (output_daily) {
    result$daily_output <- daily_output
  }
  
  return(result)
}