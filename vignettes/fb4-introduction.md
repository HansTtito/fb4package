# About FB4 Package

## Overview

The **FB4 Package** is an R-based implementation of fish bioenergetics modeling, built upon the foundation of the Fish Bioenergetics 4.0 (FB4) application developed by Deslauriers et al. (2017). This package provides a comprehensive computational framework for modeling energy allocation in fish by partitioning consumed energy into three fundamental components: metabolism, wastes, and growth.

## Scientific Foundation

Fish bioenergetics modeling has been a widely used tool in fisheries management and research for decades. The approach provides a sound, theoretical tool for quantifying energy allocation in fishes by partitioning consumed energy into three basic components: (1) metabolism, (2) wastes, and (3) growth. The models are particularly valuable for estimating growth or food consumption rates and are especially attractive for estimating feeding rates of free-ranging fishes.

## Evolution of Fish Bioenergetics Software

The development of this package addresses the need for modern, maintainable bioenergetics modeling software. Although popular, currently available software (i.e., Fish Bioenergetics 3.0) has not been updated in over 20 years and is incompatible with newer operating systems (i.e., 64-bit). Moreover, since the release of Fish Bioenergetics 3.0 in 1997, the number of published bioenergetics models has increased appreciably from 56 to 105 models representing 73 species.

## Technical Implementation

Building on the foundation of the original FB4 application, this R package provides:

- **Modern R-based Implementation**: Unlike the original Shiny-based GUI application, this package offers programmatic access to bioenergetics modeling capabilities
- **Comprehensive Parameter Database**: Includes species-specific parameters for 105 models covering 73 species as of 2017
- **Flexible Architecture**: Supports custom modifications and extensions not available in the default package
- **Reproducible Research**: Enables scripted, version-controlled bioenergetics analyses

## Key Features

### Core Modeling Capabilities
- **Energy Budget Simulation**: Complete modeling of consumption, metabolism, excretion, egestion, and growth
- **Multi-Species Support**: Built-in parameters for numerous fish species across different life stages
- **Environmental Integration**: Temperature, dissolved oxygen, and other habitat-dependent functions
- **Diet Flexibility**: Support for complex, multi-prey diet compositions

### Advanced Functionality
- **Automatic Parameter Fitting**: Iterative adjustment to reach target growth or consumption values
- **Temporal Dynamics**: Daily resolution with seasonal environmental variation
- **Uncertainty Analysis**: Tools for sensitivity analysis and parameter uncertainty assessment
- **Validation Support**: Comparison with empirical data and model validation workflows

## Research Applications

This package facilitates bioenergetics modeling applications across a wide spectrum of questions in:

- **Fish Biology**: Growth rate estimation, metabolic rate analysis, feeding ecology
- **Population Ecology**: Individual-based modeling, population dynamics, life history studies
- **Fisheries Management**: Stock assessment, habitat evaluation, climate change impacts
- **Aquaculture**: Feed optimization, growth prediction, production planning
- **Conservation Biology**: Species responses to environmental change, habitat restoration

## Scientific Reference

The theoretical and methodological foundation for this package is described in:

**Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A., and Madenjian, C.P. (2017).** Fish Bioenergetics 4.0: An R-Based Modeling Application. *Fisheries*, 42(11), 586-596.

## Original FB4 Application

This package is inspired by and builds upon the Fish Bioenergetics 4.0 application originally developed by James E. Breck and colleagues. The original FB4 application, with its Shiny-based graphical interface, is available at [fishbioenergetics.org](http://fishbioenergetics.org) and [GitHub](https://github.com/jim-breck/FB4).

## Development Philosophy

While including the same capabilities as previous versions, Fish Bioenergetics 4.0 allows for timely updates and bug fixes and can be continuously improved based on feedback from users. This R package extends that philosophy by providing:

- **Open Source Development**: Transparent, community-driven development process
- **Modular Design**: Extensible architecture for custom applications
- **Integration Ready**: Compatible with modern R workflows and data science pipelines
- **Research Reproducibility**: Version-controlled, scriptable analyses

## Contributing

We welcome contributions from the bioenergetics modeling community. Whether you're interested in adding new species parameters, improving existing functions, or developing new modeling capabilities, your input helps advance the field of fish bioenergetics modeling.

## Acknowledgments

We acknowledge the foundational work of David Deslauriers, Steven R. Chipps, James E. Breck, James A. Rice, Charles P. Madenjian, and the broader community of bioenergetics researchers who have contributed to the development and validation of fish bioenergetics models over the past several decades.