# FB4 Package Demo

This comprehensive demo demonstrates how to use the FB4 package to simulate fish bioenergetic growth models with automatic parameter fitting.

## Quick Start Example

### 1. Load Package and Setup

```r
library(fb4package)

# Define simulation parameters
initial_weight <- 11115.358    # Initial weight (g)
target_weight <- 14883.695     # Target final weight (g) 
simulation_days <- 365         # Simulation duration (days)

cat("=== FB4 CHINOOK SALMON SIMULATION ===\n")
cat("Initial weight:", initial_weight, "g\n")
cat("Target weight:", target_weight, "g\n")
cat("Duration:", simulation_days, "days\n\n")
```

### 2. Environmental Data Setup

```r
# Create temperature profile (seasonal variation)
monthly_temps <- c(4, 3, 4, 7, 10, 12, 14, 16, 15, 11, 8, 6, 4)
base_days <- seq(0, 12, length.out = length(monthly_temps))

# Interpolate to daily temperatures
temperature_interp <- approx(
  x = base_days, 
  y = monthly_temps, 
  xout = seq(0, 12, length.out = simulation_days),
  method = "linear", 
  rule = 2
)$y

temperature_data <- data.frame(
  Day = 1:simulation_days,
  Temperature = temperature_interp
)

cat("Temperature range:", round(min(temperature_data$Temperature), 1), 
    "to", round(max(temperature_data$Temperature), 1), "°C\n")
```

### 3. Diet Composition Setup

```r
# Define diet proportions
diet_data <- data.frame(
  Day = 1:simulation_days,
  anchoveta = 0.37,  # 37% anchoveta
  sardina = 0.63     # 63% sardina
)

# Energy density of prey items (J/g)
prey_energy_data <- data.frame(
  Day = 1:simulation_days,
  anchoveta = 5553,
  sardina = 5000
)

# Indigestible fractions for realistic egestion
indigestible_data <- data.frame(
  Day = 1:simulation_days,
  anchoveta = 0.05,
  sardina = 0.05
)

cat("Diet composition:\n")
cat("- Anchoveta:", round(mean(diet_data$anchoveta) * 100, 1), "%\n")
cat("- Sardina:", round(mean(diet_data$sardina) * 100, 1), "%\n")
```

### 4. Load Species Parameters

```r
# Load built-in fish parameters database
data("fish4_parameters", package = "fb4package")

# Select Chinook salmon adult parameters
species_name <- "Oncorhynchus tshawytscha"
life_stage <- "adult"

chinook_data <- fish4_parameters[[species_name]]
chinook_params <- chinook_data$life_stages[[life_stage]]
chinook_params$species_info <- chinook_data$species_info

cat("Species parameters loaded:\n")
cat("- Species:", species_name, "\n")
cat("- Life stage:", life_stage, "\n")
cat("- Consumption equation:", chinook_params$consumption$CEQ, "\n")
cat("- Respiration equation:", chinook_params$respiration$REQ, "\n")
```

### 5. Create Bioenergetic Object

```r
# Create comprehensive bioenergetic model object
bio_obj <- Bioenergetic(
  species_params = chinook_params,
  environmental_data = list(
    temperature = temperature_data
  ),
  diet_data = list(
    proportions = diet_data,
    energies = prey_energy_data,
    indigestible = indigestible_data,
    prey_names = names(diet_data)[-1]
  ),
  simulation_settings = list(
    initial_weight = initial_weight,
    duration = simulation_days
  )
)

# Set energy density parameters for realistic growth
bio_obj$species_params <- set_parameter_value(bio_obj$species_params, "ED_ini", 6308.570)
bio_obj$species_params <- set_parameter_value(bio_obj$species_params, "ED_end", 6320.743)

cat("Bioenergetic object created successfully!\n")
```

### 6. Run Simulation with Automatic Fitting

```r
# Run simulation with automatic parameter fitting to reach target weight
results <- run_fb4(
  bio_obj, 
  fit_to = "Weight", 
  fit_value = target_weight, 
  max_iterations = 30
)

# Display results
cat("\n=== SIMULATION RESULTS ===\n")
cat("Converged:", results$converged, "\n")
cat("Final p-value:", round(results$best_p_value, 6), "\n")
cat("Final weight:", round(results$final_weight, 2), "g\n")
cat("Target weight:", target_weight, "g\n")
cat("Error:", round(abs(results$final_weight - target_weight), 3), "g\n")
cat("Iterations:", results$iterations, "\n")
```

### 7. Analyze Results

```r
# Extract daily simulation data
daily_data <- results$daily_output

# View first few days
head(daily_data[, c("Day", "Temperature.C", "Weight.g", 
                   "Consumption.J.g.d", "Respiration.J.g.d", 
                   "Egestion.J.g.d", "Net.Energy.J.g.d")])

# Summary statistics
cat("\n=== METABOLIC SUMMARY ===\n")
cat("Average daily consumption:", round(mean(daily_data$Consumption.J.g.d), 2), "J/g/d\n")
cat("Average daily respiration:", round(mean(daily_data$Respiration.J.g.d), 2), "J/g/d\n")
cat("Average daily egestion:", round(mean(daily_data$Egestion.J.g.d), 2), "J/g/d\n")
cat("Total weight gain:", round(results$final_weight - initial_weight, 2), "g\n")
cat("Growth rate:", round((results$final_weight/initial_weight - 1) * 100, 1), "%\n")
```

### 8. Visualization (Optional)

```r
# Plot weight trajectory
plot(daily_data$Day, daily_data$Weight.g, 
     type = "l", lwd = 2, col = "blue",
     xlab = "Day", ylab = "Weight (g)",
     main = "Chinook Salmon Growth Simulation")
abline(h = target_weight, col = "red", lty = 2)
legend("bottomright", 
       legend = c("Simulated Growth", "Target Weight"), 
       col = c("blue", "red"), 
       lty = c(1, 2), lwd = c(2, 1))

# Plot temperature profile
plot(daily_data$Day, daily_data$Temperature.C, 
     type = "l", lwd = 2, col = "orange",
     xlab = "Day", ylab = "Temperature (°C)",
     main = "Environmental Temperature")

# Plot energy components
plot(daily_data$Day, daily_data$Consumption.J.g.d, 
     type = "l", lwd = 2, col = "green",
     xlab = "Day", ylab = "Energy (J/g/d)",
     main = "Daily Energy Budget", ylim = c(0, max(daily_data$Consumption.J.g.d)))
lines(daily_data$Day, daily_data$Respiration.J.g.d, col = "red", lwd = 2)
lines(daily_data$Day, daily_data$Egestion.J.g.d, col = "brown", lwd = 2)
legend("topright", 
       legend = c("Consumption", "Respiration", "Egestion"), 
       col = c("green", "red", "brown"), 
       lty = 1, lwd = 2)
```

## Key Features Demonstrated

- **Automatic Parameter Fitting**: The model automatically adjusts the p-value to reach the target weight
- **Comprehensive Energy Budget**: Tracks consumption, respiration, egestion, excretion, and SDA
- **Multi-prey Diet Support**: Handles complex diet compositions with multiple prey species
- **Environmental Integration**: Incorporates temperature effects on all metabolic processes  
- **Realistic Physiology**: Uses species-specific parameters from built-in database
- **Flexible Input Data**: Supports daily varying environmental and diet data

## Advanced Usage

For more complex scenarios, you can:

- Use different fish species from the built-in database
- Modify individual physiological parameters
- Include spawning energy costs
- Add predator energy density dynamics
- Implement custom temperature or diet scenarios
- Fit to different target metrics (consumption, growth rate, etc.)

See the package documentation for detailed parameter descriptions and additional examples.