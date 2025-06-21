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
    data("parameters_official", package = "FB4", envir = environment())
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

#' Función simplificada para usuarios básicos
#'
#' Interfaz simplificada para ejecutar el modelo FB4 con configuración mínima
#'
#' @param species_name Nombre de la especie
#' @param initial_weight Peso inicial (g)
#' @param target_weight Peso objetivo (g)
#' @param duration_days Duración en días
#' @param temperature_avg Temperatura promedio (°C)
#' @param temperature_variation Variación de temperatura (°C)
#' @param diet_composition Lista con composición de dieta (ej: list(Zooplankton = 0.7, Invertebrates = 0.3))
#'
#' @return Resultado simplificado de la simulación
#' @export
#' @examples
#' \dontrun{
#' result <- run_fb4_simple(
#'   species_name = "Bluegill",
#'   initial_weight = 10,
#'   target_weight = 50,
#'   duration_days = 365,
#'   temperature_avg = 20,
#'   temperature_variation = 8,
#'   diet_composition = list(Zooplankton = 0.6, Invertebrates = 0.4)
#' )
#' }
run_fb4_simple <- function(species_name,
                           initial_weight,
                           target_weight,
                           duration_days,
                           temperature_avg = 20,
                           temperature_variation = 5,
                           diet_composition = list(Zooplankton = 1.0)) {

  # Generar datos sintéticos
  days <- 1:duration_days

  # Temperatura estacional
  temperature <- temperature_avg + temperature_variation * sin(2 * pi * days / 365)
  temperature_data <- data.frame(Day = days, Temperature = temperature)

  # Datos de dieta
  diet_names <- names(diet_composition)
  diet_props <- as.numeric(diet_composition)
  diet_data <- data.frame(Day = days)
  for (i in seq_along(diet_names)) {
    diet_data[[diet_names[i]]] <- diet_props[i]
  }

  # Densidades energéticas típicas (J/g)
  typical_energies <- list(
    Zooplankton = 3500,
    Invertebrates = 4200,
    Fish = 5500,
    Detritus = 2000
  )

  # Datos de energía de presas
  prey_energy_data <- data.frame(Day = days)
  for (name in diet_names) {
    energy <- typical_energies[[name]]
    if (is.null(energy)) energy <- 3500  # valor por defecto
    prey_energy_data[[name]] <- energy
  }

  # Ejecutar modelo
  result <- run_fb4(
    species = species_name,
    initial_weight = initial_weight,
    fit_to = "Weight",
    fit_value = target_weight,
    first_day = 1,
    last_day = duration_days,
    temperature_data = temperature_data,
    diet_data = diet_data,
    prey_energy_data = prey_energy_data,
    output_daily = TRUE
  )

  # Preparar resultado simplificado
  simplified_result <- list(
    success = result$model_info$fit_successful,
    final_weight = tail(result$daily_output$Weight.g, 1),
    growth_rate = (tail(result$daily_output$Weight.g, 1) - initial_weight) / duration_days,
    total_consumption = tail(result$daily_output$Cum.Cons.g, 1),
    p_value = result$model_info$p_value,
    execution_time = result$model_info$execution_time,
    daily_data = result$daily_output[, c("Day", "Temperature.C", "Weight.g",
                                         "Consumption.g", "Specific.Growth.Rate.g.g.d")],
    summary = result$summary
  )

  class(simplified_result) <- "fb4_simple_result"
  return(simplified_result)
}
