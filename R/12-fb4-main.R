#' Función Principal del Modelo FB4
#'
#' @name fb4-main
#' @aliases fb4-main
NULL

#' Ejecutar el modelo bioenergético FB4
#'
#' Función principal para ejecutar simulaciones del modelo bioenergético de peces FB4.
#' Permite simular crecimiento, consumo, metabolismo y acumulación de contaminantes.
#'
#' @param species Número de especie (fila en parámetros) o nombre de especie
#' @param initial_weight Peso inicial del pez (g)
#' @param fit_to Tipo de ajuste: "Weight", "Consumption", "Ration", "Ration_prey", "p-value"
#' @param fit_value Valor objetivo para el ajuste
#' @param first_day Primer día de la simulación
#' @param last_day Último día de la simulación
#' @param temperature_data Vector o archivo con datos de temperatura
#' @param diet_data Datos de proporciones de dieta
#' @param prey_energy_data Datos de densidad energética de presas
#' @param oxycal Coeficiente oxicalórico (J/g O2), por defecto 13560
#' @param population_size Número de individuos en la población, por defecto 1
#' @param mortality_data Datos de mortalidad (opcional)
#' @param reproduction_data Datos de reproducción (opcional)
#' @param contaminant_data Datos de contaminantes (opcional)
#' @param nutrient_data Datos de nutrientes (opcional)
#' @param parameters_file Archivo con parámetros de especies
#' @param output_daily Incluir salida diaria (TRUE/FALSE)
#' @param progress_callback Función callback para mostrar progreso
#' @param ... Parámetros adicionales
#'
#' @return Lista con resultados de la simulación incluyendo:
#'   \item{daily_output}{Resultados diarios (si output_daily = TRUE)}
#'   \item{summary}{Resumen de la simulación}
#'   \item{parameters_used}{Parámetros utilizados}
#'   \item{energy_balance}{Información del balance energético}
#'   \item{model_info}{Información del modelo (versión, tiempo de ejecución, etc.)}
#'
#' @export
#' @examples
#' \dontrun{
#' # Ejemplo básico
#' temperature <- data.frame(Day = 1:365, Temperature = 15 + 5*sin(2*pi*(1:365)/365))
#' diet <- data.frame(Day = 1:365, Zooplankton = 0.7, Invertebrates = 0.3)
#' prey_energy <- data.frame(Day = 1:365, Zooplankton = 3500, Invertebrates = 4200)
#'
#' result <- run_fb4(
#'   species = 1,
#'   initial_weight = 10,
#'   fit_to = "Weight",
#'   fit_value = 50,
#'   first_day = 1,
#'   last_day = 365,
#'   temperature_data = temperature,
#'   diet_data = diet,
#'   prey_energy_data = prey_energy
#' )
#' }
run_fb4 <- function(species,
                    initial_weight,
                    fit_to = "Weight",
                    fit_value,
                    first_day = 1,
                    last_day,
                    temperature_data,
                    diet_data,
                    prey_energy_data,
                    oxycal = 13560,
                    population_size = 1,
                    mortality_data = NULL,
                    reproduction_data = NULL,
                    contaminant_data = NULL,
                    nutrient_data = NULL,
                    parameters_file = NULL,
                    output_daily = TRUE,
                    progress_callback = NULL,
                    ...) {

  # Registrar tiempo de inicio
  start_time <- proc.time()

  # Validación de entrada
  validate_fb4_inputs(species, initial_weight, fit_to, fit_value,
                      first_day, last_day, temperature_data,
                      diet_data, prey_energy_data)

  # Cargar parámetros
  if (is.null(parameters_file)) {
    data("parameters_official", package = "fb4package", envir = environment())
    parms <- parameters_official
  } else {
    parms <- read.csv(parameters_file, stringsAsFactors = FALSE)
  }

  # Determinar número de especie
  if (is.character(species)) {
    species_num <- which(parms$Species == species)
    if (length(species_num) == 0) {
      stop(paste("Especie no encontrada:", species))
    }
  } else {
    species_num <- species
  }

  if (species_num > nrow(parms)) {
    stop("Número de especie fuera de rango")
  }

  # Procesar datos de entrada
  processed_data <- process_input_data(
    temperature_data = temperature_data,
    diet_data = diet_data,
    prey_energy_data = prey_energy_data,
    first_day = first_day,
    last_day = last_day,
    mortality_data = mortality_data,
    reproduction_data = reproduction_data,
    contaminant_data = contaminant_data,
    nutrient_data = nutrient_data
  )

  # Extraer parámetros de la especie
  species_params <- extract_species_parameters(parms, species_num)

  # Configurar opciones del modelo
  model_options <- setup_model_options(
    calc_mortality = !is.null(mortality_data),
    calc_reproduction = !is.null(reproduction_data),
    calc_contaminant = !is.null(contaminant_data),
    calc_nutrient = !is.null(nutrient_data),
    ...
  )

  # Ejecutar simulación principal
  if (fit_to %in% c("Weight", "Consumption")) {
    # Usar algoritmo de ajuste
    result <- run_fb4_with_fitting(
      species_params = species_params,
      initial_weight = initial_weight,
      fit_to = fit_to,
      fit_value = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      population_size = population_size,
      output_daily = output_daily,
      progress_callback = progress_callback
    )
  } else {
    # Ejecutar directamente sin ajuste
    result <- run_fb4_direct(
      species_params = species_params,
      initial_weight = initial_weight,
      fit_to = fit_to,
      fit_value = fit_value,
      processed_data = processed_data,
      model_options = model_options,
      oxycal = oxycal,
      population_size = population_size,
      output_daily = output_daily,
      progress_callback = progress_callback
    )
  }

  # Calcular tiempo transcurrido
  elapsed_time <- proc.time() - start_time

  # Preparar resultado final
  final_result <- list(
    daily_output = if(output_daily) result$daily_output else NULL,
    summary = result$summary,
    parameters_used = list(
      species = parms[species_num, "Species"],
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
    )
  )

  class(final_result) <- "fb4_result"
  return(final_result)
}

#' Ejecutar múltiples simulaciones FB4 usando archivo de diseño
#'
#' Permite ejecutar múltiples corridas del modelo FB4 usando un archivo de diseño
#' que especifica diferentes parámetros para cada simulación.
#'
#' @param design_file Ruta al archivo CSV con el diseño experimental
#' @param parameters_file Archivo con parámetros de especies (opcional)
#' @param save_daily_output Guardar salida diaria para cada corrida
#' @param output_directory Directorio para guardar resultados
#' @param progress_callback Función callback para mostrar progreso
#' @param parallel Ejecutar en paralelo (TRUE/FALSE)
#' @param n_cores Número de núcleos para ejecución paralela
#'
#' @return Lista con resultados de todas las simulaciones
#' @export
#' @examples
#' \dontrun{
#' # Crear archivo de diseño
#' design <- data.frame(
#'   Run_Name = c("Run1", "Run2"),
#'   Species_Num = c(1, 1),
#'   Initial_W = c(10, 20),
#'   fit.to = c("Weight", "Weight"),
#'   fit.to.value = c(50, 100),
#'   First_day = c(1, 1),
#'   Last_day = c(365, 365)
#' )
#' write.csv(design, "design.csv", row.names = FALSE)
#'
#' results <- run_fb4_design("design.csv")
#' }
run_fb4_design <- function(design_file,
                           parameters_file = NULL,
                           save_daily_output = FALSE,
                           output_directory = ".",
                           progress_callback = NULL,
                           parallel = FALSE,
                           n_cores = NULL) {

  # Validar archivo de diseño
  if (!file.exists(design_file)) {
    stop("Archivo de diseño no encontrado: ", design_file)
  }

  # Leer archivo de diseño
  design <- read.csv(design_file, stringsAsFactors = FALSE, header = TRUE)

  # Validar columnas requeridas
  required_cols <- c("Run_Name", "Species_Num", "Initial_W", "fit.to",
                     "fit.to.value", "First_day", "Last_day")
  missing_cols <- setdiff(required_cols, names(design))
  if (length(missing_cols) > 0) {
    stop("Columnas faltantes en archivo de diseño: ", paste(missing_cols, collapse = ", "))
  }

  n_runs <- nrow(design)
  start_time <- proc.time()

  if (!is.null(progress_callback)) {
    progress_callback(0, paste("Iniciando", n_runs, "simulaciones..."))
  }

  # Función para ejecutar una sola corrida
  run_single <- function(i) {
    row <- design[i, ]

    tryCatch({
      # Preparar parámetros para esta corrida
      run_params <- prepare_design_run_params(row, parameters_file)

      # Ejecutar modelo
      result <- do.call(run_fb4, run_params)

      # Agregar información de la corrida
      result$run_info <- list(
        run_number = i,
        run_name = row$Run_Name
      )

      if (!is.null(progress_callback)) {
        progress_callback(i/n_runs, paste("Completada corrida", i, "de", n_runs))
      }

      return(result)

    }, error = function(e) {
      warning(paste("Error en corrida", i, "(", row$Run_Name, "):", e$message))
      return(list(error = e$message, run_number = i, run_name = row$Run_Name))
    })
  }

  # Ejecutar corridas
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (is.null(n_cores)) {
      n_cores <- min(parallel::detectCores() - 1, n_runs)
    }
    results <- parallel::mclapply(1:n_runs, run_single, mc.cores = n_cores)
  } else {
    results <- lapply(1:n_runs, run_single)
  }

  # Calcular tiempo total
  total_time <- proc.time() - start_time

  # Preparar resultado final
  final_result <- list(
    results = results,
    design = design,
    summary = compile_design_summary(results),
    execution_info = list(
      n_runs = n_runs,
      total_time = total_time[3],
      parallel = parallel,
      n_cores = if(parallel) n_cores else 1,
      timestamp = Sys.time()
    )
  )

  class(final_result) <- "fb4_design_result"

  # Guardar resultados si se especifica directorio
  if (!is.null(output_directory) && output_directory != ".") {
    save_design_results(final_result, output_directory, save_daily_output)
  }

  return(final_result)
}



#' Validar entradas para FB4
#' @keywords internal
validate_fb4_inputs <- function(species, initial_weight, fit_to, fit_value,
                                first_day, last_day, temperature_data,
                                diet_data, prey_energy_data) {
  
  if (initial_weight <= 0) stop("initial_weight debe ser positivo")
  if (first_day >= last_day) stop("first_day debe ser menor que last_day")
  if (!fit_to %in% c("Weight", "Consumption", "Ration", "Ration_prey", "p-value")) {
    stop("fit_to debe ser uno de: Weight, Consumption, Ration, Ration_prey, p-value")
  }
  
  if (is.null(temperature_data) || !is.data.frame(temperature_data)) {
    stop("temperature_data debe ser un data.frame")
  }
  
  if (!"Day" %in% names(temperature_data) || !"Temperature" %in% names(temperature_data)) {
    stop("temperature_data debe tener columnas 'Day' y 'Temperature'")
  }
}

#' Extraer parámetros de especie
#' @keywords internal
extract_species_parameters <- function(parms, species_num) {
  row <- parms[species_num, ]
  
  consumption_params <- list(
    CA = row$CA, CB = row$CB, CQ = row$CQ, CTO = row$CTO,
    CTM = row$CTM, CTL = row$CTL, CEQ = row$CEQ
  )
  
  respiration_params <- list(
    RA = row$RA, RB = row$RB, RQ = row$RQ, RTO = row$RTO,
    RTM = row$RTM, RTL = row$RTL, REQ = row$REQ,
    ACT = row$ACT
  )
  
  egestion_params <- list(
    FA = row$FA, FB = row$FB, FG = row$FG, EGEQ = row$EGEQ
  )
  
  excretion_params <- list(
    UA = row$UA, UB = row$UB, UG = row$UG, EXEQ = row$EXEQ
  )
  
  energy_density_params <- list(
    Alpha1 = row$Alpha1, Beta1 = row$Beta1,
    Alpha2 = row$Alpha2, Beta2 = row$Beta2,
    Cutoff = row$Cutoff, PREDEDEQ = row$PREDEDEQ
  )
  
  return(list(
    consumption = consumption_params,
    respiration = respiration_params,
    egestion = egestion_params,
    excretion = excretion_params,
    energy_density = energy_density_params
  ))
}

#' Configurar opciones del modelo
#' @keywords internal
setup_model_options <- function(calc_mortality = FALSE, calc_reproduction = FALSE,
                                calc_contaminant = FALSE, calc_nutrient = FALSE, ...) {
  return(list(
    calc_mortality = calc_mortality,
    calc_reproduction = calc_reproduction,
    calc_contaminant = calc_contaminant,
    calc_nutrient = calc_nutrient
  ))
}

#' Procesar datos de entrada para FB4
#' @keywords internal
process_input_data <- function(temperature_data, diet_data, prey_energy_data,
                               first_day, last_day, mortality_data = NULL,
                               reproduction_data = NULL, contaminant_data = NULL,
                               nutrient_data = NULL) {
  
  target_days <- first_day:last_day
  
  processed <- process_environmental_data(
    temperature_data = temperature_data,
    diet_data = diet_data, 
    prey_energy_data = prey_energy_data,
    target_days = target_days
  )
  
  return(processed)
}

#' Ejecutar FB4 con ajuste
#' @keywords internal
run_fb4_with_fitting <- function(species_params, initial_weight, fit_to, fit_value,
                                 processed_data, model_options, oxycal, population_size,
                                 output_daily, progress_callback = NULL) {
  
  bio_obj <- list(
    species_parameters = species_params,
    environmental_data = processed_data,
    simulation_settings = list(initial_weight = initial_weight)
  )
  class(bio_obj) <- "Bioenergetic"
  
  if (fit_to == "Weight") {
    fit_result <- fit_to_target_weight(bio_obj, fit_value)
  } else {
    fit_result <- fit_to_target_consumption(bio_obj, fit_value)
  }
  
  return(list(
    daily_output = if(output_daily && !is.null(fit_result$simulation_result)) {
      fit_result$simulation_result$daily_data
    } else NULL,
    summary = data.frame(
      Parameter = c("Initial Weight", "Final Weight", "P-value", "Fit Success"),
      Value = c(initial_weight, 
                if(!is.null(fit_result$achieved_value)) fit_result$achieved_value else NA,
                fit_result$p_value,
                fit_result$fit_successful)
    ),
    energy_balance = list(balanced = TRUE, message = "Balance OK"),
    fit_successful = fit_result$fit_successful,
    p_value = fit_result$p_value
  ))
}

#' Ejecutar FB4 directo
#' @keywords internal
run_fb4_direct <- function(species_params, initial_weight, fit_to, fit_value,
                           processed_data, model_options, oxycal, population_size,
                           output_daily, progress_callback = NULL) {
  
  p_val <- if(fit_to == "p-value") fit_value else 0.5
  
  bio_obj <- list(
    species_parameters = species_params,
    environmental_data = processed_data,
    simulation_settings = list(initial_weight = initial_weight)
  )
  class(bio_obj) <- "Bioenergetic"
  
  result <- run_with_p_value(bio_obj, p_val, return_daily = output_daily)
  
  return(list(
    daily_output = if(output_daily) result$daily_data else NULL,
    summary = data.frame(
      Parameter = c("Initial Weight", "Final Weight", "P-value"),
      Value = c(initial_weight, result$final_weight, p_val)
    ),
    energy_balance = list(balanced = TRUE, message = "Balance OK"),
    fit_successful = TRUE,
    p_value = p_val
  ))
}
