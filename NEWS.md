# fb4package 2.0.0 (2026-03-28)

This is a major rewrite. The entire codebase was reorganised from a monolithic
structure (7 files) into a modular, strategy-based architecture (33 files),
and an optional TMB/C++ backend was added for statistical estimation methods.

### New features

#### TMB backend
* Added optional **TMB (Template Model Builder)** backend for maximum likelihood
  and hierarchical estimation, providing 10–50× speedup over the pure-R
  implementation (`src/fb4_main.cpp`).
* New C++ headers for consumption, respiration, egestion/excretion, body
  composition, and validation (`src/includes/`).
* Added `Rcpp`, `TMB (>= 1.7.0)`, `RcppEigen` as dependencies.
  `R (>= 4.0.0)` is now required (was `>= 3.5.0`).

#### New estimation strategies via `run_fb4(strategy = ...)`
* `"optim"` — gradient-based fitting with `stats::optim()`.
* `"mle"` — maximum likelihood estimation of *p*-value with SE and 95% CI,
  using observed final weights (`observed_weights`).
* `"bootstrap"` — non-parametric uncertainty quantification via resampling
  of observed final weights. Supports parallel execution (`future`/`furrr`).
* `"hierarchical"` — population-level mixed-effects model estimating mean and
  SD of *p* across individuals (TMB required).

#### Architecture
* Simulation engine extracted into `R/12-simulation-engine.R`.
* Fitting algorithms split into individual strategy files
  (`14.2.1` through `14.2.6`), a shared commons layer
  (`14.0.1-strategy-commons.R`), and a strategy interface (`14.1`).
* Validation split into four specialised modules
  (`11.0-core`, `11.1-basic`, `11.2-parameter`, `11.3-data`, `11.4-main`).
* `Bioenergetic` class and methods separated into
  `13.0-bioenergetic-classes.R` and `13.1-bioenergetic-methods.R`.
* Analysis layer split into five focused modules (`15.0`–`15.4`).
* Plot layer split into four modules (`16.1`–`16.4`) covering core helpers,
  daily output plots, analysis plots, and bioenergetic-object plots.
* Result builders centralised in `14.3-result-builders.R` with consistent
  field names (`p_value`, `total_consumption_g`, `converged`) across all
  strategies.

#### New analysis functions
`analyze_growth_patterns()`, `analyze_feeding_performance()`,
`analyze_energy_budget()`, `analyze_growth_temperature_sensitivity()`,
`analyze_population_variation()`, `comprehensive_nutritional_analysis()`.

#### New plot types
`plot.fb4_result(type = "growth" | "consumption" | "temperature" | "energy" |
"uncertainty" | "dashboard")` and
`plot.Bioenergetic(type = "temperature" | "diet" | "energy_density" |
"sensitivity")`.

### Internal changes
* `R/00-utils.R` (v0.1.0) replaced by `R/utils.R` with additional helpers
  (`z_score()`, `safe_exp()`, `safe_sqrt()`, `clamp()`, `%||%`).
* `R/09-fitting-algorithms.R` replaced by the strategy layer (`14.x`).
* `R/10-main-growth-model.R` and `R/12-fb4-main.R` replaced by
  `R/12-simulation-engine.R` and `R/14.0-run-fb4-orchestrator.R`.
* `R/13-visualization-analysis.R` split into `15.x` (analysis) and `16.x`
  (plots).
* `R/14-bioenergetic-classes.R` split into `13.0` and `13.1`.

---

# fb4package 0.1.0

* Initial release. Pure-R implementation of Fish Bioenergetics 4.0
  (Deslauriers et al. 2017).
* Binary search fitting (`strategy = "binary_search"`) and direct *p*-value
  simulation (`strategy = "direct"`).
* `Bioenergetic` S3 class with `print()`, `summary()`, `plot()`, and
  `run_fb4()` methods.
* Species parameter database for 105 models (73 species).
* No compiled code; depends only on base R packages.
