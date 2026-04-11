#' Hierarchical Estimation Strategy for FB4 Model
#'
#' @description
#' Implements the \code{"hierarchical"} FB4 fitting strategy, which estimates
#' population-level and individual-level \emph{p}-values using a hierarchical
#' mixed-effects model compiled with TMB. Individual p_values are treated as
#' random effects (\code{log_p_individual}), while the population mean
#' (\code{mu_p}) and standard deviation (\code{sigma_p}) are fixed-effect
#' hyperparameters. Optional covariates can be supplied to explain variation in
#' \code{mu_p} across individuals.
#'
#' @references
#' Deslauriers, D., Chipps, S.R., Breck, J.E., Rice, J.A. and Madenjian, C.P.
#' (2017). Fish Bioenergetics 4.0: An R-based modeling application.
#' \emph{Fisheries}, 42(11), 586–596. \doi{10.1080/03632415.2017.1377558}
#'
#' Kristensen, K., Nielsen, A., Berg, C.W., Skaug, H. and Bell, B.M. (2016).
#' TMB: Automatic differentiation and Laplace approximation.
#' \emph{Journal of Statistical Software}, 70(5), 1–21.
#' \doi{10.18637/jss.v070.i05}
#'
#' @return No return value; this page documents the hierarchical estimation strategy functions. See individual function documentation for return values.
#' @name strategy-hierarchical
#' @aliases strategy-hierarchical
NULL

# ============================================================================
# HIERARCHICAL STRATEGY (USING SHARED COMMONS)
# ============================================================================

#' Create Hierarchical Strategy
#' @param execution_plan Execution plan with parameters
#' @return Strategy object implementing FB4Strategy interface
#' @keywords internal
create_hierarchical_strategy <- function(execution_plan) {
  
  strategy <- list(
    
    execute = function(plan) {
      
      if (plan$verbose) {
        message("Executing hierarchical strategy")
      }
      
      # Hierarchical ALWAYS uses TMB backend
      if (plan$backend != "tmb") {
        stop("Hierarchical strategy requires TMB backend")
      }
      
      # Call TMB hierarchical interface using shared data preparation
      result <- execute_hierarchical_tmb(plan)
      
      # Add strategy metadata using shared function
      strategy_info <- list(
        strategy_type = "hierarchical",
        additional_metadata = list(
          backend = "tmb"
        )
      )
      
      result <- add_strategy_metadata(result, strategy_info, plan)
      
      return(result)
    },
    
    validate_plan = function(plan) {
      # Use shared validation for hierarchical data
      validate_common_strategy_inputs(
        observed_weights = plan$observed_weights,
        strategy_type = "hierarchical"
      )
      
      # Hierarchical-specific validation
      if (!is.data.frame(plan$observed_weights) && 
          !(is.list(plan$observed_weights) && !is.null(names(plan$observed_weights)))) {
        stop("Hierarchical strategy requires data.frame or named list format for observed_weights")
      }
      
      if (plan$backend != "tmb") {
        stop("Hierarchical strategy only supports TMB backend")
      }
      
      return(TRUE)
    },
    
    get_strategy_info = function() {
      return(list(
        name = "Hierarchical Model",
        type = "hierarchical_fitting", 
        supports_backends = "tmb",
        supports_fit_types = "Weight",
        description = "Hierarchical individual p_value estimation using TMB"
      ))
    }
  )
  
  class(strategy) <- c("FB4HierarchicalStrategy", "FB4Strategy")
  return(strategy)
}

# ============================================================================
# HIERARCHICAL EXECUTION
# ============================================================================

#' Execute hierarchical TMB using unified architecture and shared commons
#' @keywords internal
execute_hierarchical_tmb <- function(plan) {
  
  if (plan$verbose) {
    message("Using TMB backend for hierarchical model estimation")
  }
  
  # Convert individual data format using existing function
  individual_data <- convert_to_individual_data(plan$observed_weights, plan$verbose)
  
  # Use shared data preparation for hierarchical TMB format
  processed_data <- prepare_simulation_data(
    bio_obj = plan$bio_obj,
    strategy = "hierarchical",
    first_day = plan$first_day,
    last_day = plan$last_day,
    output_format = "tmb_hierarchical",
    observed_weights = individual_data,
    covariates = plan$covariates,
    validate_inputs = FALSE
  )
  
  # Extract hierarchical-specific parameters using shared function
  params <- extract_strategy_parameters(
    execution_plan = plan,
    required_params = c("confidence_level"),
    default_values = list(
      confidence_level = 0.95,
      compute_profile = FALSE,
      profile_parameter = "mu_p",
      profile_grid_size = 20
    )
  )
  
  # Create TMB objective function for hierarchical model
  initial_params <- list(
    betas = numeric(processed_data$n_covariates) %||% numeric(0),
    log_mu_p = log(1.0),
    log_sigma_p = log(0.3),
    log_p_individual = rep(log(1.0), processed_data$n_individuals),
    log_sigma_obs = log(0.1)
  )

  obj <- TMB::MakeADFun(
    data = processed_data,
    parameters = initial_params,
    DLL = "fb4package",
    random = "log_p_individual",  # Individual p_values are random effects
    silent = !plan$verbose
  )

  # Optimize using shared functions from TMB module
  opt_result <- run_robust_optimization(
    obj = obj,
    method = "nlminb",
    verbose = plan$verbose
  )

  # Extract results using shared TMB functions
  result <- extract_tmb_results(
    obj = obj,
    opt_result = opt_result,
    model_type = "hierarchical",
    confidence_level = params$confidence_level,
    verbose = plan$verbose
  )

  # Generate daily output using population mean p_value
  if (result$converged && !is.na(result$mu_p_estimate)) {
    # Recreate standard format data from original bio_obj
    final_simulation <- run_fb4(
        x = plan$bio_obj,
        fit_to = "p_value",
        fit_value = result$mu_p_estimate,
        strategy = "direct_p_value",
        first_day = plan$first_day,
        last_day = plan$last_day
    )
    
    if (!is.null(final_simulation)) {
      result$daily_output <- final_simulation$daily_output
    }
  }

  if (plan$verbose) {
    message("Hierarchical model execution completed")
    if (result$converged) {
      message("Population mean p_value: ", round(result$mu_p_estimate, 4))
      message("Population SD p_value: ", round(result$sigma_p_estimate, 4))
      message("Number of individuals: ", result$n_individuals)
    }
  }

  return(result)
}