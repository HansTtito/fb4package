---
title: 'fb4package: An R Package for Fish Bioenergetics Modeling with High-Performance TMB Backend'
tags:
  - R
  - fish bioenergetics
  - fisheries science
  - energy budget modeling
  - TMB
  - aquaculture
authors:
  - name: Hans Ttito
    orcid: 0000-0003-3732-9419
    affiliation: "1, 2, 3"
affiliations:
  - name: Programa de Magíster en Ciencias con Mención en Pesquerías, Facultad de Ciencias Naturales y Oceanográficas, Universidad de Concepción, Concepción, Chile
    index: 1
  - name: Genomics in Ecology, Evolution and Conservation Lab (GEECLAB), Departamento de Zoología, Facultad de Ciencias Naturales y Oceanográficas, Universidad de Concepción, Concepción, Chile
    index: 2
  - name: National Center of Investigation in Rivers, Invasions & Systems, Concepción, Chile
    index: 3
date: 28 March 2026
bibliography: paper.bib
---

# Summary

`fb4package` is an R package that provides a comprehensive, programmatic implementation of fish bioenergetics modeling based on Fish Bioenergetics 4.0 (FB4) [@deslauriers2017]. Fish bioenergetics models partition consumed energy into three fundamental components—metabolism, wastes, and growth—providing a mechanistic framework for estimating fish consumption rates and growth under varying environmental conditions. Unlike the original Shiny-based GUI application, `fb4package` exposes the full modeling workflow through a scriptable R interface, enabling reproducible analyses, automated parameter fitting, and integration into larger analytical pipelines. The package features an optional Template Model Builder (TMB) backend [@kristensen2016] for maximum likelihood estimation and uncertainty quantification.

# Statement of Need

Fish bioenergetics models have been applied for decades to quantify energy budgets, estimate foraging rates of free-ranging fish, assess habitat quality, evaluate climate change impacts on fish populations, and support aquaculture production planning [@hanson1997; @chipps2008]. The original Wisconsin bioenergetics model [@hewett1992] and its successors, including Fish Bioenergetics 3.0 and 4.0 [@deslauriers2017], made these tools widely accessible through graphical user interfaces. However, GUI-based tools present practical limitations: analyses conducted through point-and-click interfaces are difficult to document and reproduce; running simulations across many species, environmental scenarios, or bootstrap replicates requires scripting; embedding bioenergetics calculations within broader analytical workflows (population models, stock assessments) demands programmatic access; and formal statistical estimation of parameter uncertainty requires optimization frameworks that are not easily exposed through a GUI.

While the original FB4 [@deslauriers2017] source code is available and includes an R Shiny interface, it was not designed as a reusable R package. `fb4package` addresses this gap by providing a fully documented, object-oriented R package that faithfully implements the FB4 equations while extending the modeling capabilities with modern statistical methods and a high-performance computational backend.

# Mathematical Framework

The FB4 model expresses the daily energy balance of an individual fish as:

$$C = (R + S + F + U) + \Delta B$$

where $C$ is daily consumption (J g⁻¹ day⁻¹), $R$ is standard or active respiration, $S$ is the specific dynamic action (SDA), $F$ is egestion (fecal losses), $U$ is excretion (urinary losses), and $\Delta B$ is the change in somatic energy density (growth or loss). Each component is modeled as a temperature- and weight-dependent function using species-specific parameters.

Maximum consumption is described by:

$$C_{max} = C_A \cdot W^{C_B} \cdot f(T)$$

where $C_A$ and $C_B$ are allometric parameters, $W$ is fish body mass (g), and $f(T)$ is a temperature-dependence function (Kitchell–Lessem, Elliott, or polynomial form, selected via the `CEQ` parameter). Actual daily consumption is then:

$$C = p \cdot C_{max}$$

where $p \in (0, 5]$ is the proportion of maximum consumption realized by the fish — the central parameter estimated by `fb4package`. Respiration follows a similar allometric structure with activity and dissolved oxygen corrections [@stewart1991].

The key inferential contribution of `fb4package` is the formal estimation of $p$ from observed weight data via maximum likelihood. Observed final weights are modelled as log-normally distributed around the model-predicted weight:

$$\ln W_{obs,i} \sim \mathcal{N}(\ln W_{pred},\ \sigma^2)$$

yielding the negative log-likelihood:

$$\mathcal{L}(p, \sigma) = n \ln \sigma + \frac{1}{2\sigma^2} \sum_{i=1}^{n} \left(\ln W_{obs,i} - \ln W_{pred}\right)^2$$

This objective function is minimized jointly over $p$ and $\sigma$ using TMB [@kristensen2016], which provides analytical gradients via automatic differentiation and Hessian-based standard errors via the Laplace approximation.

# Key Features

**Species parameter database.** The package includes the complete FB4 species database with parameters for over 105 models covering 73 fish species, spanning salmonids, centrarchids, clupeids, and others, organized by species and life stage.

**Flexible consumption fitting.** The `run_fb4()` function accepts multiple fitting strategies: a direct simulation at a fixed $p$ value, binary-search fitting to a target final weight or consumption rate, non-linear optimization via `optim()`, maximum likelihood estimation (MLE), bootstrap resampling for uncertainty intervals, and a hierarchical approach for multi-individual data.

**TMB backend for MLE.** When the `backend = "tmb"` option is selected, the negative log-likelihood and its gradient are evaluated in compiled C++ code using TMB [@kristensen2016], enabling gradient-based optimization with automatic differentiation. This yields accurate Hessian-based standard errors and reduces computation time by an order of magnitude for the MLE and bootstrap strategies.

**Multi-prey diet modeling.** Diet composition is specified as time-varying proportions across any number of prey types, each with its own energy density and indigestible fraction. Daily mean prey energy density is computed as a weighted average, enabling realistic seasonal diet shifts.

**Environmental drivers.** Daily water temperature is the primary environmental driver, supplied as a time series that governs all temperature-dependent submodels (consumption, respiration, egestion, excretion). This allows simulations to be forced directly by field-collected or reanalysis temperature data.

**Analysis and visualization.** Post-simulation analysis functions decompose energy budgets, characterize growth efficiency, and assess diet quality. A comprehensive `plot()` S3 method generates publication-ready figures including growth trajectories, energy budget panels, temperature-sensitivity surfaces, and uncertainty ribbons.

# Comparison with Existing Tools

The original FB4 Shiny application [@deslauriers2017] is the primary reference implementation of the FB4 equations and remains the standard for point-and-click use. `fb4package` complements it by exposing the full modeling workflow programmatically: all simulation steps are accessible through a documented S3 class interface, formal MLE and bootstrap uncertainty quantification are available out of the box, a compiled TMB backend handles computationally intensive workflows, and the complete FB4 species parameter database is bundled with the package.

# Example Usage

``` r
library(fb4package)

# Load species database and extract Chinook salmon parameters
data("fish4_parameters", package = "fb4package")
chinook <- fish4_parameters[["Oncorhynchus tshawytscha"]]

# Build a Bioenergetic object
bio_obj <- Bioenergetic(
  species_info   = chinook$species_info,
  species_params = chinook$life_stages$adult,
  environmental_data = list(
    temperature = data.frame(Day = 1:365,
                             Temperature = 10 + 6 * sin(2 * pi * (1:365) / 365))
  ),
  diet_data = list(
    proportions  = data.frame(Day = 1:365, Prey1 = 0.37, Prey2 = 0.63),
    energies     = data.frame(Day = 1:365, Prey1 = 5553, Prey2 = 5000),
    indigestible = data.frame(Day = 1:365, Prey1 = 0.05, Prey2 = 0.05),
    prey_names   = c("Prey1", "Prey2")
  ),
  simulation_settings = list(initial_weight = 11115, duration = 365)
)

bio_obj$species_params$predator$ED_ini <- 6308.570
bio_obj$species_params$predator$ED_end <- 6320.776

# Fit p to reach a target final weight
results <- run_fb4(bio_obj, strategy = "binary_search", fit_to = "Weight", fit_value = 14884)

# Visualize results
plot(results, type = "dashboard")
```

# Acknowledgements

The bioenergetics equations implemented in this package follow those published by @deslauriers2017 and the original Wisconsin bioenergetics model lineage [@kitchell1977; @hewett1992; @hanson1997]. The author thanks the developers of the original FB4 Shiny application for making the source code publicly available, and the TMB development team [@kristensen2016] for the automatic differentiation framework that enables the high-performance backend.

# References