# fb4package

## About

The **fb4package** provides a modern R-based implementation of fish bioenergetics modeling, built upon the foundation of Fish Bioenergetics 4.0 developed by Deslauriers et al. (2017). This package enables comprehensive energy budget modeling for fish growth, consumption, and metabolic processes.

**For detailed background, scientific foundation, and development history, see:** `vignette("fb4-introduction")`

## Key Features

- **Comprehensive Energy Budget Modeling**: Complete simulation of consumption, metabolism, excretion, egestion, and growth
- **Multi-Species Support**: Built-in parameters for 105+ fish species models across different life stages
- **Automatic Parameter Fitting**: Iterative optimization to reach target weights or consumption rates using binary search algorithms
- **Environmental Integration**: Temperature effects and habitat-dependent functions (dissolved oxygen, salinity)
- **Flexible Diet Modeling**: Multi-prey diet compositions with daily variation and indigestible fractions
- **Temporal Dynamics**: Daily resolution with seasonal environmental variation support
- **Research Reproducibility**: Scriptable, version-controlled bioenergetics analyses

## Installation

```r
# Install from GitHub (development version)
devtools::install_github("HansTtito/fb4package")

```

## Quick Start

### Basic Simulation Example

```r
library(fb4package)

# Load built-in species parameters
data("fish4_parameters", package = "fb4package")
chinook_params <- fish4_parameters[["Oncorhynchus tshawytscha"]]$life_stages$adult

# Create temperature data (seasonal variation)
temperature_data <- data.frame(
  Day = 1:365,
  Temperature = 4 + 6 * sin(2 * pi * (1:365) / 365)  # 4°C ± 6°C seasonal cycle
)

# Define diet composition
diet_data <- data.frame(
  Day = 1:365,
  anchoveta = 0.37,  # 37% anchoveta
  sardina = 0.63     # 63% sardina
)

# Prey energy densities (J/g)
prey_energy_data <- data.frame(
  Day = 1:365,
  anchoveta = 5553,
  sardina = 5000
)

# Indigestible fractions
indigestible_data <- data.frame(
  Day = 1:365,
  anchoveta = 0.05,
  sardina = 0.05
)

# Create bioenergetic object
bio_obj <- Bioenergetic(
  species_params = chinook_params,
  environmental_data = list(temperature = temperature_data),
  diet_data = list(
    proportions = diet_data,
    energies = prey_energy_data,
    indigestible = indigestible_data,
    prey_names = c("anchoveta", "sardina")
  ),
  simulation_settings = list(
    initial_weight = 11115.358,  # Initial weight (g)
    duration = 365               # Simulation days
  )
)

# Set predator energy density parameters
bio_obj$species_params <- set_parameter_value(bio_obj$species_params, "ED_ini", 6308.570)
bio_obj$species_params <- set_parameter_value(bio_obj$species_params, "ED_end", 6320.776)


# Run simulation with automatic fitting to target weight
results <- run_fb4(
  bio_obj,
  fit_to = "Weight",
  fit_value = 14883.695,  # Target final weight (g)
  max_iterations = 25
)

# View results
print(paste("Final weight:", round(results$summary$final_weight, 2), "g"))
print(paste("Optimal p-value:", round(results$summary$p_value, 6)))
print(paste("Converged:", results$fit_info$fit_successful))
```

### Visualization

```r
# Extract daily data
daily_data <- results$daily_output

# Plot growth trajectory
plot(daily_data$Day, daily_data$Weight.g, 
     type = "l", lwd = 2, col = "blue",
     xlab = "Day", ylab = "Weight (g)",
     main = "Fish Growth Simulation")

# Plot energy budget components
plot(daily_data$Day, daily_data$Consumption.J.g.d, 
     type = "l", lwd = 2, col = "green",
     xlab = "Day", ylab = "Energy (J/g/d)",
     main = "Daily Energy Budget")
lines(daily_data$Day, daily_data$Respiration.J.g.d, col = "red", lwd = 2)
lines(daily_data$Day, daily_data$Egestion.J.g.d, col = "brown", lwd = 2)
legend("topright", 
       legend = c("Consumption", "Respiration", "Egestion"),
       col = c("green", "red", "brown"), lty = 1, lwd = 2)
```

## Advanced Usage

### Custom Species Parameters

```r
# Modify specific physiological parameters
bio_obj$species_params <- set_parameter_value(bio_obj$species_params, "CA", 0.303)
bio_obj$species_params <- set_parameter_value(bio_obj$species_params, "CB", -0.275)

# Set custom energy density parameters
bio_obj$species_params <- set_parameter_value(bio_obj$species_params, "ED_ini", 6308.570)
bio_obj$species_params <- set_parameter_value(bio_obj$species_params, "ED_end", 6320.743)
```

### Different Fitting Options

```r
# Fit to consumption instead of weight
results_consumption <- run_fb4(
  bio_obj,
  fit_to = "Consumption",
  fit_value = 0.02,  # Target consumption rate (g/g/d)
  max_iterations = 25
)

# Fit with custom tolerance
results_precise <- run_fb4(
  bio_obj,
  fit_to = "Weight",
  fit_value = 14883.695,
  max_iterations = 50,
  tolerance = 0.0001  # Higher precision
)
```

## Package Structure

- **Core Functions**: `Bioenergetic()`, `run_fb4()`, `run_fb4_simulation_complete()`
- **Parameter Management**: `set_parameter_value()`, `get_parameter_value()`
- **Built-in Data**: `fish4_parameters` - comprehensive species database
- **Validation Tools**: Model checking and parameter validation functions
- **Visualization**: Built-in plotting and analysis utilities

## Scientific Applications

This package supports research in:

- **Fish Biology**: Growth rate estimation, metabolic rate analysis, feeding ecology
- **Population Ecology**: Individual-based modeling, population dynamics, life history studies  
- **Fisheries Management**: Stock assessment, habitat evaluation, climate change impacts
- **Aquaculture**: Feed optimization, growth prediction, production planning
- **Conservation Biology**: Species responses to environmental change, habitat restoration

## Contributing

We welcome contributions from the bioenergetics modeling community! Whether you're interested in:

- Adding new species parameters
- Improving existing functions
- Developing new modeling capabilities  
- Reporting bugs or suggesting features
- Improving documentation or examples

**Please see:** `vignette("fb4-introduction")` for detailed information about the project's scientific foundation and how to get involved.

### Development Guidelines

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Documentation

- **Introduction**: `vignette("fb4-introduction")` - Scientific background and package overview
- **Function Reference**: `help(package = "fb4package")` - Complete function documentation ## Developing
- **Examples**: `vignette("fb4-examples")` - Additional usage examples and case studies ## Developing

## References

**Primary Reference:**
Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A., and Madenjian, C.P. (2017). Fish Bioenergetics 4.0: An R-Based Modeling Application. *Fisheries*, 42(11), 586-596.

**Original FB4 Application:**
- Website: [fishbioenergetics.org](http://fishbioenergetics.org)
- GitHub: [jim-breck/FB4](https://github.com/jim-breck/FB4)

## License

This package is licensed under the MIT License. See `LICENSE` file for details.

## Support

- **Bug Reports**: [GitHub Issues](https://github.com/HansTtito/fb4package/issues)
- **Questions**: [GitHub Discussions](https://github.com/HansTtito/fb4package/discussions)
- **Email**: kvttitos@gmail.com

---
