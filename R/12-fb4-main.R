#' Execute Fish Bioenergetics 4.0 Model with Bioenergetic Object
#'
#' @description
#' Main function to execute Fish Bioenergetics 4.0 model simulations
#' using a Bioenergetic class object that already contains all necessary parameters.
#' This method provides a streamlined interface for running bioenergetic simulations
#' with pre-configured species parameters, environmental data, and diet information.
#'
#' @param bio_obj Object of class 'Bioenergetic' with configured parameters and data
#' @param fit_to Fitting method: "Weight", "Consumption", "p-value", "Ration", "Ration_prey"
#' @param fit_value Target value according to the fitting method
#' @param first_day First simulation day (default 1)
#' @param last_day Last simulation day (default uses complete duration)
#' @param oxycal Oxycalorific coefficient (J/g O2), default 13560
#' @param population_size Initial population size, default 1
#' @param output_daily Logical, whether to include daily output, default TRUE
#' @param tolerance Tolerance for iterative fitting, default 0.001
#' @param max_iterations Maximum number of iterations, default 25
#' @param progress_callback Progress callback function (optional)
#' @param ... Additional arguments
#'
#' @return Object of class 'fb4_result' with simulation results
#'
#' @details
#' This function uses a Bioenergetic object that must contain:
#' \itemize{
#'   \item{species_params: }{Species parameters organized by categories (consumption, metabolism, predator)}
#'   \item{environmental_data: }{Temperature data with Day and Temperature columns}
#'   \item{diet_data: }{Diet proportions and prey energy densities}
#'   \item{simulation_settings: }{Initial configuration including initial weight and duration}
#' }
#'
#' Available fitting methods:
#' \itemize{
#'   \item{"Weight": }{Fit to achieve target final weight (g)}
#'   \item{"Consumption": }{Fit to achieve target total consumption (g)}
#'   \item{"p-value": }{Run with fixed proportion of maximum consumption (0-1)}
#'   \item{"Ration": }{Run with fixed ration as percentage of body weight (0-100)}
#'   \item{"Ration_prey": }{Run with fixed daily ration in grams}
#' }
#'
#' The simulation uses a daily time step and accounts for:
#' temperature-dependent metabolism, size-dependent consumption rates,
#' egestion, excretion, specific dynamic action (SDA), respiration,
#' and optional spawning energy losses.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create bioenergetic object
#' bio_obj <- new_bioenergetic(
#'   species = "Oncorhynchus tshawytscha",
#'   initial_weight = 10,
#'   duration = 365,
#'   temperature_avg = 15,
#'   diet_composition = list(anchoveta = 0.4, sardina = 0.6)
#' )
#'
#' # Execute simulation fitting to final weight
#' result <- run_fb4(
#'   bio_obj = bio_obj,
#'   fit_to = "Weight",
#'   fit_value = 50
#' )
#'
#' # Execute simulation with fixed p-value
#' result <- run_fb4(
#'   bio_obj = bio_obj,
#'   fit_to = "p-value",
#'   fit_value = 0.5
#' )
#'
#' # Execute simulation with percentage ration
#' result <- run_fb4(
#'   bio_obj = bio_obj,
#'   fit_to = "Ration",
#'   fit_value = 3.0  # 3% of body weight per day
#' )
#' }
#'
#' @seealso \code{\link{new_bioenergetic}}, \code{\link{validate_bioenergetic_object}}
#'
#' @references
#' Hanson, P.C., T.B. Johnson, D.E. Schindler, and J.F. Kitchell. 1997.
#' Fish Bioenergetics 3.0. University of Wisconsin Sea Grant Institute,
#' Madison, WI.
run_fb4.Bioenergetic <- function(bio_obj,
                                 fit_to = "Weight",
                                 fit_value,
                                 first_day = 1,
                                 last_day = NULL,
                                 oxycal = 13560,
                                 population_size = 1,
                                 output_daily = TRUE,
                                 tolerance = 0.001,
                                 max_iterations = 25,
                                 progress_callback = NULL,
                                 ...) {
  
  # Validate Bioenergetic object
  if (!inherits(bio_obj, "Bioenergetic")) {
    stop("bio_obj must be an object of class 'Bioenergetic'")
  }
  
  # Validate required components
  validate_bioenergetic_object(bio_obj)
  
  # Extract data from object
  initial_weight <- bio_obj$simulation_settings$initial_weight
  if (is.null(last_day)) {
    last_day <- bio_obj$simulation_settings$duration %||% 
      max(bio_obj$environmental_data$temperature$Day)
  }
  
  # Record start time
  start_time <- proc.time()
  
  # Input validation using object data
  validate_fb4_bioenergetic_inputs(bio_obj, fit_to, fit_value, first_day, last_day)
  
  # Process input data from Bioenergetic object
  processed_data <- process_bioenergetic_data(bio_obj, first_day, last_day)
  
  # Extract species parameters (already organized in object)
  species_params <- bio_obj$species_params
  
  # Configure model options
  model_options <- setup_model_options_from_bioenergetic(bio_obj, ...)
  
  # Execute main simulation
  if (fit_to %in% c("Weight", "Consumption")) {
    # Use fitting algorithm
    result <- fit_fb4_binary_search(
      species_params = species_params,
      initial_weight = initial_weight,
      fit_to = tolower(fit_to),
      fit_value = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      population_size = population_size,
      output_daily = output_daily,
      tolerance = tolerance,
      max_iterations = max_iterations
    )
    
  } else if (fit_to == "p-value") {
    # Execute directly without fitting
    result <- run_fb4_with_p_value(
      species_params = species_params,
      initial_weight = initial_weight,
      p_value = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      population_size = population_size,
      output_daily = output_daily
    )
    
  } else if (fit_to == "Ration") {
    # Execute with ration as percentage
    result <- run_fb4_with_ration_percent(
      species_params = species_params,
      initial_weight = initial_weight,
      ration_percent = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      population_size = population_size,
      output_daily = output_daily
    )
    
  } else if (fit_to == "Ration_prey") {
    # Execute with daily ration in grams
    result <- run_fb4_with_ration_grams(
      species_params = species_params,
      initial_weight = initial_weight,
      ration_grams = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      population_size = population_size,
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
    summary = result$summary,
    parameters_used = list(
      species = species_params$species_info$scientific_name %||% "Unknown",
      species_params = species_params,
      model_options = model_options,
      initial_conditions = list(
        initial_weight = initial_weight,
        fit_to = fit_to,
        fit_value = fit_value,
        first_day = first_day,
        last_day = last_day,
        population_size = population_size,
        oxycal = oxycal
      )
    ),
    energy_balance = result$energy_balance,
    model_info = list(
      version = "1.1.4b",
      execution_time = elapsed_time[3],
      timestamp = Sys.time(),
      fit_successful = result$fit_successful,
      p_value = result$p_value
    ),
    bioenergetic_object = bio_obj  # Reference to original object
  )
  
  class(final_result) <- "fb4_result"
  return(final_result)
}

#' Generic Function for Fish Bioenergetics 4.0 Model Execution
#'
#' @description
#' Generic function that allows executing Fish Bioenergetics 4.0 model simulations
#' with different types of input objects. This provides a flexible interface
#' that can be extended to support various data input formats.
#'
#' @param x Object to use for simulation (e.g., Bioenergetic object)
#' @param ... Additional arguments passed to specific methods
#'
#' @return Object of class 'fb4_result' with simulation results
#'
#' @details
#' This generic function dispatches to specific methods based on object class:
#' \itemize{
#'   \item{Bioenergetic: }{Uses run_fb4.Bioenergetic()}
#'   \item{Others: }{Future methods for other classes}
#' }
#'
#' The function provides a consistent interface regardless of the input object type,
#' making it easier to integrate with different data workflows and analysis pipelines.
#'
#' @export
#' @examples
#' \dontrun{
#' # With Bioenergetic object
#' bio_obj <- new_bioenergetic(
#'   species = "Salmo salar",
#'   initial_weight = 15,
#'   duration = 200,
#'   temperature_avg = 12
#' )
#' result <- run_fb4(bio_obj, fit_to = "Weight", fit_value = 50)
#'
#' # Check result structure
#' str(result)
#' plot(result)
#' }
#'
#' @seealso \code{\link{run_fb4.Bioenergetic}}, \code{\link{new_bioenergetic}}
run_fb4 <- function(x, ...) {
  UseMethod("run_fb4")
}

#' Default Method for run_fb4
#'
#' @description
#' Default method for run_fb4 generic function. This method is called
#' when no specific method exists for the given object class.
#'
#' @param x Unsupported object
#' @param ... Additional arguments
#'
#' @return Throws an error with information about supported classes
#'
#' @details
#' This method provides helpful error messages when users attempt to use
#' run_fb4 with unsupported object types, guiding them toward supported
#' alternatives.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # This will throw an error
#' run_fb4(data.frame(x = 1:10))
#' }
run_fb4.default <- function(x, ...) {
  stop("No run_fb4 method available for objects of class '", 
       paste(class(x), collapse = "', '"), "'.\n",
       "Supported classes: 'Bioenergetic'")
}

#' Execute FB4 with Daily Ration in Grams
#'
#' @description
#' Executes the simulation using a specific daily ration in grams.
#' Does not require iterative fitting as the consumption is fixed.
#' The p-value is calculated dynamically based on the ration and fish weight.
#'
#' @param species_params List with species parameters
#' @param initial_weight Initial weight in grams
#' @param ration_grams Daily ration in grams of prey
#' @param processed_data Processed input data
#' @param model_options Model options
#' @param oxycal Oxycalorific coefficient, default 13560
#' @param population_size Population size, default 1
#' @param output_daily Logical, whether to include daily output
#'
#' @return List with simulation results
#'
#' @details
#' The ration is constant in absolute grams each day, regardless of fish weight.
#' The p-value is calculated dynamically based on: p = ration_grams / (weight * Cmax)
#' where Cmax is the maximum consumption rate at the given temperature.
#'
#' This approach is useful when you have specific feeding data or want to
#' simulate controlled feeding experiments.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal function - not called directly by users
#' result <- run_fb4_with_ration_grams(
#'   species_params = my_params,
#'   initial_weight = 10,
#'   ration_grams = 0.5,
#'   processed_data = my_data,
#'   model_options = my_options
#' )
#' }
run_fb4_with_ration_grams <- function(species_params, initial_weight, ration_grams,
                                      processed_data, model_options, oxycal = 13560,
                                      population_size = 1, output_daily = TRUE) {
  
  # Validate ration
  if (ration_grams <= 0) {
    stop("ration_grams must be greater than 0")
  }
  
  # Execute simulation with fixed ration
  simulation_result <- run_fb4_simulation_with_ration(
    initial_weight = initial_weight,
    ration_type = "grams",
    ration_value = ration_grams,
    species_params = species_params,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    population_size = population_size,
    output_daily = output_daily
  )
  
  # Calculate average p-value for reporting
  if (output_daily && !is.null(simulation_result$daily_output)) {
    avg_p_value <- mean(simulation_result$daily_output$P.Value, na.rm = TRUE)
  } else {
    avg_p_value <- NA
  }
  
  result <- simulation_result
  result$fit_info <- list(
    ration_grams = ration_grams,
    avg_p_value = avg_p_value,
    fit_successful = TRUE,
    iterations = 1,
    final_error = 0,
    target_value = ration_grams,
    achieved_value = ration_grams,
    warnings = character(),
    method = "ration_grams"
  )
  
  return(result)
}


#' Execute FB4 with Ration as Percentage of Body Weight
#'
#' @description
#' Executes the simulation using a specific ration as percentage
#' of daily body weight. Does not require iterative fitting.
#' The actual ration varies with fish weight throughout the simulation.
#'
#' @param species_params List with species parameters
#' @param initial_weight Initial weight in grams
#' @param ration_percent Ration as percentage of body weight (0-100)
#' @param processed_data Processed input data
#' @param model_options Model options
#' @param oxycal Oxycalorific coefficient, default 13560
#' @param population_size Population size, default 1
#' @param output_daily Logical, whether to include daily output
#'
#' @return List with simulation results
#'
#' @details
#' The ration is calculated as: daily_ration = (ration_percent/100) * current_weight
#' The p-value is calculated dynamically each day based on this ration.
#' This approach is common in aquaculture and experimental studies where
#' feeding rates are expressed as percentage of body weight.
#'
#' As the fish grows, the absolute amount of food consumed increases
#' proportionally, which is more realistic than fixed gram amounts.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal function - not called directly by users
#' result <- run_fb4_with_ration_percent(
#'   species_params = my_params,
#'   initial_weight = 10,
#'   ration_percent = 3.0,  # 3% of body weight
#'   processed_data = my_data,
#'   model_options = my_options
#' )
#' }
run_fb4_with_ration_percent <- function(species_params, initial_weight, ration_percent,
                                        processed_data, model_options, oxycal = 13560,
                                        population_size = 1, output_daily = TRUE) {
  
  # Validate ration
  if (ration_percent < 0 || ration_percent > 100) {
    stop("ration_percent must be between 0 and 100")
  }
  
  ration_fraction <- ration_percent / 100
  
  # Execute simulation with variable ration
  simulation_result <- run_fb4_simulation_with_ration(
    initial_weight = initial_weight,
    ration_type = "percent",
    ration_value = ration_fraction,
    species_params = species_params,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    population_size = population_size,
    output_daily = output_daily
  )
  
  # Calculate average p-value for reporting
  if (output_daily && !is.null(simulation_result$daily_output)) {
    avg_p_value <- mean(simulation_result$daily_output$P.Value, na.rm = TRUE)
  } else {
    avg_p_value <- NA
  }
  
  result <- simulation_result
  result$fit_info <- list(
    ration_percent = ration_percent,
    avg_p_value = avg_p_value,
    fit_successful = TRUE,
    iterations = 1,
    final_error = 0,
    target_value = ration_percent,
    achieved_value = ration_percent,
    warnings = character(),
    method = "ration_percent"
  )
  
  return(result)
}

#' Execute FB4 Simulation with Specific P-Value
#'
#' @description
#' Executes the Fish Bioenergetics 4.0 simulation using a fixed p-value
#' (proportion of maximum consumption). This is the most direct approach
#' when you know the desired feeding level.
#'
#' @param species_params Species parameters
#' @param initial_weight Initial weight in grams
#' @param p_value Specific p-value (proportion of maximum consumption, 0-5)
#' @param processed_data Processed data
#' @param model_options Model options
#' @param oxycal Oxycalorific coefficient, default 13560
#' @param population_size Number of individuals, default 1
#' @param output_daily Return daily output, default TRUE
#'
#' @return List with simulation results
#'
#' @details
#' The p-value represents the proportion of maximum consumption and typically
#' ranges from 0.1 to 1.0 for realistic scenarios, though values up to 5.0
#' are technically possible. Values are automatically clamped to the range
#' 0.001 to 4.999 to ensure numerical stability.
#'
#' This method is useful when:
#' \itemize{
#'   \item{You have empirical data on feeding rates}
#'   \item{You want to explore different feeding scenarios}
#'   \item{You need to compare with other bioenergetic studies}
#' }
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal function - not called directly by users
#' result <- run_fb4_with_p_value(
#'   species_params = my_params,
#'   initial_weight = 15,
#'   p_value = 0.6,
#'   processed_data = my_data,
#'   model_options = my_options
#' )
#' }
run_fb4_with_p_value <- function(species_params, initial_weight, p_value,
                                 processed_data, model_options, oxycal = 13560,
                                 population_size = 1, output_daily = TRUE) {
  
  # Validate and clamp p-value
  p_value <- clamp(p_value, 0.001, 4.999)
  
  # Execute complete simulation
  simulation_result <- run_fb4_simulation_complete(
    initial_weight = initial_weight,
    p_value = p_value,
    species_params = species_params,
    processed_data = processed_data,
    model_options = model_options,
    oxycal = oxycal,
    population_size = population_size,
    output_daily = output_daily
  )
  
  # Add information that no fitting was performed
  result <- simulation_result
  result$fit_info <- list(
    p_value = p_value,
    fit_successful = TRUE,
    iterations = 1,
    final_error = 0,
    target_value = NA,
    achieved_value = simulation_result$final_weight,
    warnings = character(),
    method = "direct_p_value"
  )
  
  return(result)
}

#' Complete FB4 Simulation
#'
#' @description
#' Improved version of the complete Fish Bioenergetics 4.0 simulation
#' using modular functions for metabolism and growth calculations.
#' This is the core simulation engine that processes day-by-day calculations.
#'
#' @param initial_weight Initial weight in grams
#' @param p_value Proportion of maximum consumption
#' @param species_params Species parameters
#' @param processed_data Processed environmental data
#' @param model_options Model options
#' @param oxycal Oxycalorific coefficient, default 13560
#' @param population_size Number of individuals, default 1
#' @param output_daily Return daily output, default TRUE
#'
#' @return List with complete simulation results
#'
#' @details
#' This function performs the core bioenergetic calculations on a daily basis:
#' 
#' \strong{Daily Calculations:}
#' \itemize{
#'   \item{Consumption: }{Based on temperature, weight, and p-value}
#'   \item{Egestion: }{Waste energy from consumed food}
#'   \item{Excretion: }{Nitrogenous waste energy}
#'   \item{Respiration: }{Metabolic energy costs}
#'   \item{SDA: }{Specific Dynamic Action (cost of digestion)}
#'   \item{Reproduction: }{Spawning energy losses (if applicable)}
#'   \item{Growth: }{Energy allocated to somatic growth}
#' }
#'
#' \strong{Energy Balance:}
#' Growth = Consumption - Egestion - Excretion - Respiration - SDA - Reproduction
#'
#' The simulation uses temperature-dependent rates and allometric scaling
#' relationships to account for size and environmental effects.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal function - not called directly by users
#' result <- run_fb4_simulation_complete(
#'   initial_weight = 10,
#'   p_value = 0.5,
#'   species_params = my_params,
#'   processed_data = my_data,
#'   model_options = my_options
#' )
#' }
run_fb4_simulation_complete <- function(initial_weight, p_value, species_params,
                                        processed_data, model_options, oxycal = 13560,
                                        population_size = 1, output_daily = TRUE) {
  
  # Extract processed data
  temperature <- processed_data$temperature
  diet_proportions <- processed_data$diet_proportions
  prey_energies <- processed_data$prey_energies
  n_days <- processed_data$duration
  first_day <- processed_data$first_day
  last_day <- processed_data$last_day
  day_indigestible <- processed_data$prey_indigestible
  
  # Interpolate predator energy density
  energy_data <- interpolate_predator_energy_density(
    species_params$predator, first_day, last_day
  )
  
  # Initialize variables
  current_weight <- initial_weight
  total_consumption <- 0
  total_spawn_energy <- 0
  
  # Initialize daily output if required
  if (output_daily) {
    daily_output <- initialize_daily_output(n_days, temperature, p_value)
  }
  
  # ============================================================================
  # DAY-BY-DAY SIMULATION (USING MODULAR FUNCTIONS)
  # ============================================================================
  
  for (day in 1:n_days) {
    
    # 1. Calculate mean prey energy for this day
    mean_prey_energy <- calculate_daily_mean_prey_energy(
      diet_proportions[day, ], prey_energies[day, ]
    )
    
    # 2. Calculate predator energy density
    predator_ed <- calculate_predator_energy_density(
      current_weight, day, species_params$predator, energy_data
    )
    
    # 3. DAILY CONSUMPTION
    consumption_result <- calculate_daily_consumption(
      current_weight = current_weight,
      temperature = temperature[day],
      p_value = p_value,
      method = "p_value",
      consumption_params = species_params$consumption,
      mean_prey_energy = mean_prey_energy
    )
    
    # 4. DAILY METABOLISM (egestion, excretion, respiration, SDA)
    metabolism_result <- calculate_daily_metabolism(
      consumption_energy = consumption_result$consumption_energy,
      current_weight = current_weight,
      temperature = temperature[day],
      p_value = consumption_result$effective_p,
      species_params = species_params,
      oxycal = oxycal,
      diet_proportions = diet_proportions[day,],          
      indigestible_fractions = day_indigestible[day,]   
    )
    
    # 5. REPRODUCTION LOSSES
    spawn_energy <- calculate_daily_spawn_energy(
      day, current_weight, predator_ed, processed_data, model_options
    )
    
    # 6. DAILY GROWTH
    growth_result <- calculate_final_weight_fb4(
      initial_weight = current_weight,
      net_energy = metabolism_result$net_energy * current_weight,  # Convertir a J totales
      spawn_energy = spawn_energy,
      species_params_predator = species_params$predator,
      day = day,
      energy_data = energy_data
    )
    
    # 7. VALIDATION
    if (growth_result$final_weight <= 0) {
      warning("Negative weight on day ", day, ". Terminating simulation.")
      break
    }
    
    # 8. SAVE DAILY DATA
    if (output_daily) {
      daily_output <- update_daily_output(
        daily_output, day, current_weight, consumption_result,
        metabolism_result, growth_result, spawn_energy, predator_ed
      )
    }
    
    # 9. UPDATE FOR NEXT DAY
    total_consumption <- total_consumption + (consumption_result$consumption_gg * current_weight)
    total_spawn_energy <- total_spawn_energy + spawn_energy
    current_weight <- growth_result$final_weight
  }
  
  # ============================================================================
  # PREPARE FINAL RESULT
  # ============================================================================
  
  result <- list(
    final_weight = current_weight,
    initial_weight = initial_weight,
    total_consumption = total_consumption,
    total_spawn_energy = total_spawn_energy,
    simulation_days = n_days,
    p_value = p_value
  )
  
  if (output_daily) {
    result$daily_output <- daily_output
  }
  
  return(result)
}

#' FB4 Simulation with Specific Ration
#'
#' @description
#' Function that executes daily simulation when a specific ration
#' is specified instead of a fixed p-value. Uses modular functions
#' for improved clarity and consistency.
#'
#' @param initial_weight Initial weight in grams
#' @param ration_type Type of ration: "percent" or "grams"
#' @param ration_value Ration value
#' @param species_params List with species parameters
#' @param processed_data Processed input data
#' @param model_options Model options
#' @param oxycal Oxycalorific coefficient, default 13560
#' @param population_size Population size, default 1
#' @param output_daily Logical, whether to include daily output
#'
#' @return List with simulation results
#'
#' @details
#' This function dynamically calculates the p-value each day based on the
#' specified ration and current fish weight. Uses modular functions
#' for greater clarity and consistency.
#'
#' \strong{Ration Types:}
#' \itemize{
#'   \item{"percent": }{Ration as percentage of body weight (variable absolute amount)}
#'   \item{"grams": }{Fixed daily ration in grams (constant absolute amount)}
#' }
#'
#' The function automatically handles the conversion between ration specifications
#' and p-values, which are used internally by the bioenergetic calculations.
#' This provides a more intuitive interface for users familiar with
#' aquaculture or experimental feeding protocols.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Internal function - not called directly by users
#' result <- run_fb4_simulation_with_ration(
#'   initial_weight = 20,
#'   ration_type = "percent",
#'   ration_value = 0.03,  # 3% of body weight
#'   species_params = my_params,
#'   processed_data = my_data,
#'   model_options = my_options
#' )
#' }
run_fb4_simulation_with_ration <- function(initial_weight, ration_type, ration_value,
                                           species_params, processed_data, model_options,
                                           oxycal = 13560, population_size = 1, output_daily = TRUE) {
  
  # Extract processed data
  temperature <- processed_data$temperature
  diet_proportions <- processed_data$diet_proportions
  prey_energies <- processed_data$prey_energies
  n_days <- processed_data$duration
  first_day <- processed_data$first_day
  last_day <- processed_data$last_day
  
  # Interpolate predator energy density
  energy_data <- interpolate_predator_energy_density(
    species_params$predator, first_day, last_day
  )
  
  # Initialize variables
  current_weight <- initial_weight
  total_consumption <- 0
  total_spawn_energy <- 0
  
  # Initialize daily output (with additional columns for ration)
  if (output_daily) {
    daily_output <- initialize_daily_output_ration(n_days, temperature)
  }
  
  # ============================================================================
  # DAY-BY-DAY SIMULATION (WITH VARIABLE RATION)
  # ============================================================================
  
  for (day in 1:n_days) {
    
    temp <- temperature[day]
    day_diet_props <- processed_data$diet_proportions[day, ]
    day_indigestible <- processed_data$indigestible_fractions[day, ]
    
    # 1. Calculate mean prey energy for this day
    mean_prey_energy <- calculate_daily_mean_prey_energy(
      diet_proportions[day, ], prey_energies[day, ]
    )
    
    # 2. Calculate predator energy density
    predator_ed <- calculate_predator_energy_density(
      current_weight, day, species_params$predator, energy_data
    )
    
    # 3. DAILY CONSUMPTION BASED ON RATION
    consumption_result <- calculate_daily_consumption(
      current_weight = current_weight,
      temperature = temp,
      ration_percent = if (ration_type == "percent") ration_value * 100 else NULL,
      ration_grams = if (ration_type == "grams") ration_value else NULL,
      method = if (ration_type == "percent") "ration_percent" else "ration_grams",
      consumption_params = species_params$consumption,
      mean_prey_energy = mean_prey_energy
    )
    
    # 4. DAILY METABOLISM
    metabolism_result <- calculate_daily_metabolism(
      consumption_energy = consumption_result$consumption_energy,
      current_weight = current_weight,
      temperature = temp,
      p_value = consumption_result$effective_p,
      species_params = species_params,
      oxycal = oxycal,
      diet_proportions = day_diet_props,          
      indigestible_fractions = day_indigestible   
    )
    
    # 5. REPRODUCTION LOSSES
    spawn_energy <- calculate_daily_spawn_energy(
      day, current_weight, predator_ed, processed_data, model_options
    )
    
    # 6. DAILY GROWTH
    growth_result <- calculate_daily_growth(
      current_weight = current_weight,
      net_energy = metabolism_result$net_energy,
      spawn_energy = spawn_energy,
      predator_energy_density = predator_ed
    )
    
    # 7. VALIDATION
    if (growth_result$final_weight <= 0) {
      warning("Negative weight on day ", day, ". Terminating simulation.")
      break
    }
    
    # 8. SAVE DAILY DATA (with ration information)
    if (output_daily) {
      daily_output <- update_daily_output_ration(
        daily_output, day, current_weight, consumption_result,
        metabolism_result, growth_result, spawn_energy, predator_ed,
        ration_value, ration_type
      )
    }
    
    # 9. UPDATE FOR NEXT DAY
    total_consumption <- total_consumption + (consumption_result$consumption_gg * current_weight)
    total_spawn_energy <- total_spawn_energy + spawn_energy
    current_weight <- growth_result$final_weight
  }
  
  # ============================================================================
  # PREPARE FINAL RESULT
  # ============================================================================
  
  result <- list(
    final_weight = current_weight,
    initial_weight = initial_weight,
    total_consumption = total_consumption,
    total_spawn_energy = total_spawn_energy,
    simulation_days = n_days,
    p_value = if (output_daily) mean(daily_output$P.Value, na.rm = TRUE) else NA,
    ration_type = ration_type,
    ration_value = ration_value
  )
  
  if (output_daily) {
    result$daily_output <- daily_output
  }
  
  return(result)
}