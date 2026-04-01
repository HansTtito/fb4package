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

While the original FB4 [@deslauriers2017] source code is available and includes an R Shiny interface, it was not designed as a reusable R package. `fb4package` addresses this gap by providing a fully documented, tested, object-oriented R package that faithfully implements the FB4 equations while extending the modeling capabilities with modern statistical methods and a high-performance computational backend. The package is publicly available on GitHub with a full pkgdown documentation site and a test suite covering the core simulation and fitting routines.

# State of the Field

Fish bioenergetics modeling has a long history in fisheries science, originating with the Wisconsin bioenergetics framework formalized by @kitchell1977 for yellow perch and walleye. This approach describes the daily energy balance of a fish as a function of consumption, metabolic losses (respiration, egestion, excretion, and specific dynamic action), and somatic growth, with each term governed by species-specific allometric and temperature-dependent parameters. The framework proved broadly applicable and was subsequently extended and packaged into a series of accessible software tools: Fish Bioenergetics 2.0 [@hewett1992], Bioenergetics 3.0 [@hanson1997], and most recently Fish Bioenergetics 4.0 [@deslauriers2017].

Bioenergetics 3.0 [@hanson1997] was a landmark in making these models accessible to a wide audience through a Windows GUI, and it was used extensively for estimating fish consumption in lakes and rivers. @chipps2008 reviewed the subsequent two decades of application and identified persistent challenges: difficulty in formal uncertainty quantification, the need for batch processing across scenarios and species, and the lack of integration with statistical modeling environments. FB4 [@deslauriers2017] addressed several biological limitations of its predecessor—including updated temperature-response equations and an expanded species database—and migrated the interface to R Shiny, making it platform-independent. However, it retained the GUI paradigm and was not structured as a reusable R package, limiting its use in scripted workflows.

Within the broader R ecosystem, no existing package provides a complete, self-contained implementation of the FB4 equations with formal statistical fitting capabilities. `fb4package` fills this niche by building on the FB4 biological framework while providing a documented S3 class interface, multiple fitting strategies including maximum likelihood estimation via TMB [@kristensen2016], and a bundled species parameter database. This positions `fb4package` as a complement to the original FB4 Shiny application: the GUI remains the preferred tool for interactive, single-run analyses, while `fb4package` targets reproducible batch analyses, uncertainty quantification, and integration into larger modeling workflows.

# Software Design

## Architecture

`fb4package` is structured around an S3 class hierarchy. The central object is the `Bioenergetic` class, which encapsulates all inputs required for a simulation: species identity and life-stage parameters (drawn from the bundled FB4 species database or supplied by the user), environmental drivers (primarily a daily water temperature time series), diet composition (time-varying prey proportions, energy densities, and indigestible fractions), and simulation settings (initial body mass, simulation duration). This design separates model specification from model execution, facilitating reproducibility and enabling batch analyses by iterating over a list of `Bioenergetic` objects.

The primary computational entry point is `run_fb4()`, which dispatches to one of several fitting strategies controlled by the `strategy` argument. Results are returned as an `fb4_result` S3 object, for which `print()`, `summary()`, and `plot()` methods are provided.

## Mathematical Framework

The FB4 model expresses the daily energy balance of an individual fish as [@kitchell1977; @deslauriers2017]:

$$C = (R + S + F + U) + \Delta B$$

where $C$ is consumption, $R$ respiration, $S$ specific dynamic action, $F$ egestion, $U$ excretion, and $\Delta B$ the change in somatic energy (all in J g⁻¹ day⁻¹). Each term is modeled as a temperature- and weight-dependent function using species-specific parameters.

Actual daily consumption is computed as:

$$C = p \cdot C_A \cdot W^{C_B} \cdot f(T)$$

where $p \in (0, 5]$ is the proportion of maximum consumption realized by the fish, $C_A$ and $C_B$ are allometric parameters, $W$ is body mass (g), and $f(T)$ is a temperature-dependence function (multiple forms supported via the `CEQ` parameter). Estimating $p$ from observed growth data is supported through several fitting approaches: binary search, maximum likelihood estimation via TMB [@kristensen2016], and bootstrap resampling.

## Key Features

**Species parameter database.** The package includes the complete FB4 species database with parameters for over 105 models covering 73 fish species, spanning salmonids, centrarchids, clupeids, and others, organized by species and life stage.

**Flexible consumption fitting.** The `run_fb4()` function accepts multiple fitting strategies: a direct simulation at a fixed $p$ value, binary-search fitting to a target final weight or consumption rate, non-linear optimization via `optim()`, maximum likelihood estimation (MLE), bootstrap resampling for uncertainty intervals, and a hierarchical approach for multi-individual data.

**TMB backend for MLE.** When the `backend = "tmb"` option is selected, the negative log-likelihood and its gradient are evaluated in compiled C++ code using TMB [@kristensen2016], enabling gradient-based optimization with automatic differentiation. This yields accurate Hessian-based standard errors and reduces computation time by an order of magnitude for the MLE and bootstrap strategies.

**Multi-prey diet modeling.** Diet composition is specified as time-varying proportions across any number of prey types, each with its own energy density and indigestible fraction. Daily mean prey energy density is computed as a weighted average, enabling realistic seasonal diet shifts.

**Environmental drivers.** Daily water temperature is the primary environmental driver, supplied as a time series that governs all temperature-dependent submodels (consumption, respiration, egestion, excretion). This allows simulations to be forced directly by field-collected or reanalysis temperature data.

**Analysis and visualization.** Post-simulation analysis functions decompose energy budgets, characterize growth efficiency, and assess diet quality. A comprehensive `plot()` S3 method generates publication-ready figures including growth trajectories, energy budget panels, temperature-sensitivity surfaces, and uncertainty ribbons.

# Research Impact Statement

`fb4package` is intended to lower the barrier for reproducible bioenergetics research and to extend the range of scientific questions that can be addressed with FB4-style models. By exposing the full modeling workflow through a documented R interface, the package enables researchers to (1) archive complete analysis scripts alongside manuscripts, supporting open and reproducible science; (2) run large-scale batch simulations across species, life stages, or climate scenarios without manual GUI interaction; (3) embed bioenergetics submodels within stock assessment or individual-based population models; and (4) perform formal statistical inference on consumption parameters using maximum likelihood estimation and bootstrap uncertainty intervals.

Anticipated user communities include fisheries ecologists estimating predator consumption in lakes and rivers [@stewart1991; @madenjian2004], aquaculture managers optimizing feeding schedules, and conservation biologists projecting fish growth under future temperature regimes. The package is also suitable for teaching bioenergetics modeling in graduate fisheries and ecology courses, where the R interface facilitates hands-on exploration of model sensitivity and parameter uncertainty in ways that GUI tools do not readily support.

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

# AI Usage Disclosure

Claude (Anthropic) was the primary AI tool used during the development of this software and manuscript. Claude was used to assist with code structuring, generation of boilerplate and repetitive code patterns, and writing parts of the manuscript, including grammar and spelling corrections. The package was developed using Positron as the primary IDE. All code and text generated or modified with AI assistance was reviewed by the author. AI-generated code was extensively validated against expected model outputs before inclusion in the package.

# Acknowledgements

The bioenergetics equations implemented in this package follow those published by @deslauriers2017 and the original Wisconsin bioenergetics model lineage [@kitchell1977; @hewett1992; @hanson1997]. This work builds on the original FB4 framework but focuses on providing a reproducible and extensible software implementation rather than new biological theory. The author thanks the developers of the original FB4 Shiny application for making the source code publicly available, and the TMB development team [@kristensen2016] for the automatic differentiation framework that enables the high-performance backend.

# References
