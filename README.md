# fb4package

An R implementation of Fish Bioenergetics 4.0 (Deslauriers et al. 2017). The
model partitions consumed energy among metabolism, waste, and growth to produce
daily estimates of fish consumption and weight gain.

## Installation

```r
# Development version from GitHub
devtools::install_github("HansTtito/fb4package")
```

## Usage

```r
library(fb4package)

data(fish4_parameters)
chinook <- fish4_parameters[["Oncorhynchus tshawytscha"]]

bio <- Bioenergetic(
  species_params     = chinook$life_stages$adult,
  species_info       = chinook$species_info,
  environmental_data = list(
    temperature = data.frame(Day = 1:100,
                             Temperature = 10 + 5 * sin(2 * pi * (1:100) / 365))
  ),
  diet_data = list(
    proportions = data.frame(Day = 1:100, Alewife = 0.7, Shrimp = 0.3),
    prey_names  = c("Alewife", "Shrimp"),
    energies    = data.frame(Day = 1:100, Alewife = 4900, Shrimp = 3200)
  ),
  simulation_settings = list(initial_weight = 1800, duration = 100)
)

# Fit p to a target final weight
res <- run_fb4(bio, strategy = "binary_search", fit_to = "Weight", fit_value = 3000)
print(res)
```

Full documentation, vignettes, and function reference are available at
<https://hansttito.github.io/fb4package/>.

## Documentation

- [Introduction](https://hansttito.github.io/fb4package/articles/fb4-introduction.html)
- [Statistical estimation strategies](https://hansttito.github.io/fb4package/articles/fb4-statistical-estimation.html)
- [Case study: Chinook salmon](https://hansttito.github.io/fb4package/articles/fb4-case-study-chinook.html)
- [Species database](https://hansttito.github.io/fb4package/articles/fb4-species-database.html)
- [Temperature and climate](https://hansttito.github.io/fb4package/articles/fb4-temperature-climate.html)
- [Function reference](https://hansttito.github.io/fb4package/reference/index.html)

## Reference

Deslauriers D, Chipps SR, Breck JE, Rice JA, Madenjian CP (2017). Fish
Bioenergetics 4.0: An R-Based Modeling Application. *Fisheries* 42(11):586–596.
<https://doi.org/10.1080/03632415.2017.1377558>

## Code of Conduct

Please note that fb4package is released with a [Contributor Code of Conduct](https://github.com/HansTtito/fb4package/blob/main/CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.

## License

MIT
