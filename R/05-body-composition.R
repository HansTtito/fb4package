#' Funciones de Composición Corporal para el Modelo FB4
#'
#' @name body-composition
#' @aliases body-composition
NULL

# ============================================================================
# FUNCIONES PRINCIPALES DE COMPOSICIÓN CORPORAL
# ============================================================================

#' Estimar contenido de proteína a partir del agua
#'
#' Estima gramos de proteína basado en el contenido de agua usando la
#' regresión de Breck (2014)
#'
#' @param H2O Contenido de agua (g)
#' @return Contenido de proteína (g)
#' @export
#' @references
#' Breck, J.E. 2014. Body composition in fishes: body size matters.
#' Aquaculture 433:40-49.
#' @examples
#' # Para un pez de 100g con 72% de agua
#' water_content <- 100 * 0.72
#' protein_content <- Protein(water_content)
#' print(protein_content)
Protein <- function(H2O) {

  # Validar entrada
  if (any(H2O <= 0, na.rm = TRUE)) {
    warning("Contenido de agua debe ser positivo")
    H2O[H2O <= 0] <- 0.1  # Valor mínimo
  }

  # Regresión de Breck (2014), N = 101, r² = 0.9917
  # log10(Protein) = -0.8068 + 1.0750 * log10(H2O)
  Pro <- 10^(-0.8068 + 1.0750 * log10(H2O))

  return(Pro)
}

#' Estimar contenido de ceniza a partir del agua
#'
#' Estima gramos de ceniza basado en el contenido de agua usando la
#' regresión de Breck (2014)
#'
#' @param H2O Contenido de agua (g)
#' @return Contenido de ceniza (g)
#' @export
#' @references
#' Breck, J.E. 2014. Body composition in fishes: body size matters.
#' Aquaculture 433:40-49.
#' @examples
#' # Para un pez de 100g con 72% de agua
#' water_content <- 100 * 0.72
#' ash_content <- Ash(water_content)
#' print(ash_content)
Ash <- function(H2O) {

  # Validar entrada
  if (any(H2O <= 0, na.rm = TRUE)) {
    warning("Contenido de agua debe ser positivo")
    H2O[H2O <= 0] <- 0.1  # Valor mínimo
  }

  # Regresión de Breck (2014), N = 101, r² = 0.9932
  # log10(Ash) = -1.6765 + 1.0384 * log10(H2O)
  A.g <- 10^(-1.6765 + 1.0384 * log10(H2O))

  return(A.g)
}

#' Estimar contenido de grasa por sustracción
#'
#' Calcula gramos de grasa restando agua, proteína y ceniza del peso total
#'
#' @param W Peso total húmedo (g)
#' @param H2O Contenido de agua (g)
#' @param Pro Contenido de proteína (g)
#' @param Ash Contenido de ceniza (g)
#' @return Contenido de grasa (g)
#' @export
#' @examples
#' W <- 100
#' H2O <- 72
#' Pro <- Protein(H2O)
#' Ash_content <- Ash(H2O)
#' Fat_content <- Fat(W, H2O, Pro, Ash_content)
#' print(Fat_content)
Fat <- function(W, H2O, Pro, Ash) {

  # Validar entrada
  if (any(W <= 0, na.rm = TRUE)) {
    stop("Peso total debe ser positivo")
  }

  # Calcular grasa por sustracción
  F.g <- W - H2O - Pro - Ash

  # Validar que la grasa no sea negativa
  if (any(F.g < 0, na.rm = TRUE)) {
    warning("Contenido de grasa negativo calculado, estableciendo a 0")
    F.g[F.g < 0] <- 0
  }

  # Validar que no exceda límites biológicos (~30% máximo)
  max_fat <- W * 0.35  # 35% como límite superior
  if (any(F.g > max_fat, na.rm = TRUE)) {
    warning("Contenido de grasa muy alto (>35% del peso)")
  }

  return(F.g)
}

#' Estimar densidad energética a partir de grasa y proteína
#'
#' Calcula densidad energética basada en contenido de grasa y proteína
#'
#' @param Fat_g Contenido de grasa (g)
#' @param Pro_g Contenido de proteína (g)
#' @param W Peso total (g)
#' @param fat_energy Energía por gramo de grasa (J/g), por defecto 39300
#' @param protein_energy Energía por gramo de proteína (J/g), por defecto 23600
#' @return Densidad energética (J/g peso húmedo)
#' @export
#' @examples
#' W <- 100
#' H2O <- 72
#' Pro <- Protein(H2O)
#' Ash_content <- Ash(H2O)
#' Fat_content <- Fat(W, H2O, Pro, Ash_content)
#' ED <- EnDen(Fat_content, Pro, W)
#' print(ED)
EnDen <- function(Fat_g, Pro_g, W, fat_energy = 39300, protein_energy = 23600) {

  # Validar entrada
  if (any(W <= 0, na.rm = TRUE)) {
    stop("Peso debe ser positivo")
  }

  if (any(Fat_g < 0 | Pro_g < 0, na.rm = TRUE)) {
    warning("Contenido de grasa o proteína negativo")
  }

  # Calcular densidad energética
  # Valores estándar: grasa = 39,300 J/g (9.4 kcal/g)
  #                   proteína = 23,600 J/g (5.65 kcal/g)
  ED <- (Fat_g * fat_energy + Pro_g * protein_energy) / W

  # Validar rango biológico (típicamente 2000-8000 J/g)
  if (any(ED < 1000 | ED > 10000, na.rm = TRUE)) {
    warning("Densidad energética fuera de rango típico (1000-10000 J/g)")
  }

  return(ED)
}

# ============================================================================
# FUNCIÓN INTEGRAL DE COMPOSICIÓN CORPORAL
# ============================================================================

#' Calcular composición corporal completa
#'
#' Función que calcula todos los componentes de la composición corporal
#' y la densidad energética a partir del peso y fracción de agua
#'
#' @param W Peso total húmedo (g)
#' @param H2O_fraction Fracción de agua (0-1), por defecto 0.728
#' @param fat_energy Energía por gramo de grasa (J/g)
#' @param protein_energy Energía por gramo de proteína (J/g)
#' @return Lista con composición corporal completa
#' @export
#' @examples
#' # Composición para un pez de 100g
#' composition <- calculate_body_composition(W = 100, H2O_fraction = 0.72)
#' print(composition)
#'
#' # Composición para múltiples pesos
#' weights <- c(10, 50, 100, 200)
#' compositions <- lapply(weights, calculate_body_composition)
#' names(compositions) <- paste0("Fish_", weights, "g")
calculate_body_composition <- function(W,
                                       H2O_fraction = 0.728,
                                       fat_energy = 39300,
                                       protein_energy = 23600) {

  # Validar entrada
  if (W <= 0) {
    stop("Peso debe ser positivo")
  }

  if (H2O_fraction < 0.4 || H2O_fraction > 0.9) {
    warning("Fracción de agua fuera de rango típico (0.4-0.9)")
  }

  # Calcular contenido de agua
  H2O_g <- H2O_fraction * W

  # Calcular otros componentes
  Pro_g <- Protein(H2O_g)
  Ash_g <- Ash(H2O_g)
  Fat_g <- Fat(W, H2O_g, Pro_g, Ash_g)

  # Calcular densidad energética
  ED <- EnDen(Fat_g, Pro_g, W, fat_energy, protein_energy)

  # Calcular fracciones
  H2O_fr <- H2O_g / W
  Pro_fr <- Pro_g / W
  Ash_fr <- Ash_g / W
  Fat_fr <- Fat_g / W

  # Verificar que las fracciones sumen ~1
  total_fraction <- H2O_fr + Pro_fr + Ash_fr + Fat_fr

  if (abs(total_fraction - 1) > 0.01) {
    warning("Las fracciones no suman 1.0: ", round(total_fraction, 3))
  }

  # Calcular fracción no lipídica (para modelos de contaminantes)
  ProAsh_fr <- Pro_fr + Ash_fr

  return(list(
    # Información básica
    total_weight = W,

    # Contenidos absolutos (g)
    water_g = H2O_g,
    protein_g = Pro_g,
    ash_g = Ash_g,
    fat_g = Fat_g,

    # Fracciones
    water_fraction = H2O_fr,
    protein_fraction = Pro_fr,
    ash_fraction = Ash_fr,
    fat_fraction = Fat_fr,
    protein_ash_fraction = ProAsh_fr,  # Para modelos de contaminantes

    # Energía
    energy_density = ED,
    total_energy = ED * W,
    fat_energy_contribution = Fat_g * fat_energy,
    protein_energy_contribution = Pro_g * protein_energy,

    # Validación
    total_fraction = total_fraction,
    fraction_balance = abs(total_fraction - 1) < 0.01
  ))
}

# ============================================================================
# FUNCIONES DE ANÁLISIS Y COMPARACIÓN
# ============================================================================

#' Analizar composición corporal por tamaño
#'
#' Analiza cómo cambia la composición corporal con el tamaño del pez
#'
#' @param weight_range Rango de pesos a analizar (vector de 2 elementos)
#' @param n_points Número de puntos para analizar
#' @param H2O_fraction Fracción de agua constante o función del peso
#' @return Data frame con análisis de composición por tamaño
#' @export
analyze_composition_by_size <- function(weight_range = c(1, 500),
                                        n_points = 50,
                                        H2O_fraction = 0.728) {

  # Crear secuencia de pesos
  weights <- seq(weight_range[1], weight_range[2], length.out = n_points)

  # Calcular composición para cada peso
  results <- data.frame(
    Weight = weights,
    Water_g = numeric(n_points),
    Protein_g = numeric(n_points),
    Ash_g = numeric(n_points),
    Fat_g = numeric(n_points),
    Water_fraction = numeric(n_points),
    Protein_fraction = numeric(n_points),
    Ash_fraction = numeric(n_points),
    Fat_fraction = numeric(n_points),
    Energy_density = numeric(n_points)
  )

  for (i in seq_along(weights)) {
    # Permitir fracción de agua variable con el peso si es función
    if (is.function(H2O_fraction)) {
      h2o_frac <- H2O_fraction(weights[i])
    } else {
      h2o_frac <- H2O_fraction
    }

    comp <- calculate_body_composition(weights[i], h2o_frac)

    results$Water_g[i] <- comp$water_g
    results$Protein_g[i] <- comp$protein_g
    results$Ash_g[i] <- comp$ash_g
    results$Fat_g[i] <- comp$fat_g
    results$Water_fraction[i] <- comp$water_fraction
    results$Protein_fraction[i] <- comp$protein_fraction
    results$Ash_fraction[i] <- comp$ash_fraction
    results$Fat_fraction[i] <- comp$fat_fraction
    results$Energy_density[i] <- comp$energy_density
  }

  return(results)
}

#' Comparar composición corporal entre especies/condiciones
#'
#' Compara composición corporal bajo diferentes condiciones
#'
#' @param conditions Lista con condiciones a comparar
#' @param weight Peso de referencia para comparación
#' @return Data frame con comparación de composiciones
#' @export
compare_body_compositions <- function(conditions, weight = 100) {

  results <- data.frame(
    Condition = character(),
    Water_fraction = numeric(),
    Protein_fraction = numeric(),
    Ash_fraction = numeric(),
    Fat_fraction = numeric(),
    Energy_density = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(conditions)) {
    condition_name <- names(conditions)[i]
    if (is.null(condition_name)) condition_name <- paste("Condition", i)

    params <- conditions[[i]]
    h2o_frac <- params$H2O_fraction %||% 0.728

    comp <- calculate_body_composition(
      W = weight,
      H2O_fraction = h2o_frac,
      fat_energy = params$fat_energy %||% 39300,
      protein_energy = params$protein_energy %||% 23600
    )

    results <- rbind(results, data.frame(
      Condition = condition_name,
      Water_fraction = comp$water_fraction,
      Protein_fraction = comp$protein_fraction,
      Ash_fraction = comp$ash_fraction,
      Fat_fraction = comp$fat_fraction,
      Energy_density = comp$energy_density,
      stringsAsFactors = FALSE
    ))
  }

  return(results)
}

# ============================================================================
# FUNCIONES DE VALIDACIÓN
# ============================================================================

#' Validar composición corporal
#'
#' Verifica que la composición corporal esté en rangos biológicamente realistas
#'
#' @param composition Lista de composición corporal (output de calculate_body_composition)
#' @return Lista con resultados de validación
#' @export
validate_body_composition <- function(composition) {

  warnings <- character()
  errors <- character()
  valid <- TRUE

  # Rangos típicos para peces (fracciones)
  typical_ranges <- list(
    water = c(0.65, 0.85),
    protein = c(0.10, 0.25),
    ash = c(0.02, 0.08),
    fat = c(0.02, 0.25),
    energy_density = c(2000, 8000)
  )

  # Validar agua
  if (composition$water_fraction < typical_ranges$water[1] ||
      composition$water_fraction > typical_ranges$water[2]) {
    warnings <- c(warnings,
                  paste("Fracción de agua fuera de rango típico:",
                        round(composition$water_fraction, 3)))
  }

  # Validar proteína
  if (composition$protein_fraction < typical_ranges$protein[1] ||
      composition$protein_fraction > typical_ranges$protein[2]) {
    warnings <- c(warnings,
                  paste("Fracción de proteína fuera de rango típico:",
                        round(composition$protein_fraction, 3)))
  }

  # Validar ceniza
  if (composition$ash_fraction < typical_ranges$ash[1] ||
      composition$ash_fraction > typical_ranges$ash[2]) {
    warnings <- c(warnings,
                  paste("Fracción de ceniza fuera de rango típico:",
                        round(composition$ash_fraction, 3)))
  }

  # Validar grasa
  if (composition$fat_fraction < typical_ranges$fat[1] ||
      composition$fat_fraction > typical_ranges$fat[2]) {
    warnings <- c(warnings,
                  paste("Fracción de grasa fuera de rango típico:",
                        round(composition$fat_fraction, 3)))
  }

  # Validar densidad energética
  if (composition$energy_density < typical_ranges$energy_density[1] ||
      composition$energy_density > typical_ranges$energy_density[2]) {
    warnings <- c(warnings,
                  paste("Densidad energética fuera de rango típico:",
                        round(composition$energy_density, 0), "J/g"))
  }

  # Validar balance de fracciones
  if (!composition$fraction_balance) {
    errors <- c(errors, "Las fracciones no suman 1.0")
    valid <- FALSE
  }

  # Validar valores negativos
  if (any(c(composition$water_g, composition$protein_g,
            composition$ash_g, composition$fat_g) < 0)) {
    errors <- c(errors, "Componentes negativos detectados")
    valid <- FALSE
  }

  return(list(
    valid = valid,
    warnings = warnings,
    errors = errors,
    n_warnings = length(warnings),
    n_errors = length(errors)
  ))
}

# ============================================================================
# FUNCIONES DE VISUALIZACIÓN
# ============================================================================

#' Graficar composición corporal por tamaño
#'
#' Crea gráficos de cómo cambia la composición con el tamaño
#'
#' @param analysis_data Output de analyze_composition_by_size
#' @param plot_type Tipo de gráfico ("fractions", "absolute", "energy")
#' @return Gráfico de composición corporal
#' @export
plot_body_composition_by_size <- function(analysis_data,
                                          plot_type = "fractions") {

  if (plot_type == "fractions") {
    # Gráfico de fracciones
    par(mfrow = c(2, 2))

    plot(analysis_data$Weight, analysis_data$Water_fraction,
         type = "l", lwd = 2, col = "blue",
         xlab = "Peso (g)", ylab = "Fracción de Agua",
         main = "Fracción de Agua vs Peso")
    grid()

    plot(analysis_data$Weight, analysis_data$Protein_fraction,
         type = "l", lwd = 2, col = "red",
         xlab = "Peso (g)", ylab = "Fracción de Proteína",
         main = "Fracción de Proteína vs Peso")
    grid()

    plot(analysis_data$Weight, analysis_data$Fat_fraction,
         type = "l", lwd = 2, col = "orange",
         xlab = "Peso (g)", ylab = "Fracción de Grasa",
         main = "Fracción de Grasa vs Peso")
    grid()

    plot(analysis_data$Weight, analysis_data$Ash_fraction,
         type = "l", lwd = 2, col = "gray",
         xlab = "Peso (g)", ylab = "Fracción de Ceniza",
         main = "Fracción de Ceniza vs Peso")
    grid()

    par(mfrow = c(1, 1))

  } else if (plot_type == "absolute") {
    # Gráfico de valores absolutos
    par(mfrow = c(2, 2))

    plot(analysis_data$Weight, analysis_data$Water_g,
         type = "l", lwd = 2, col = "blue",
         xlab = "Peso (g)", ylab = "Agua (g)",
         main = "Contenido de Agua vs Peso")
    grid()

    plot(analysis_data$Weight, analysis_data$Protein_g,
         type = "l", lwd = 2, col = "red",
         xlab = "Peso (g)", ylab = "Proteína (g)",
         main = "Contenido de Proteína vs Peso")
    grid()

    plot(analysis_data$Weight, analysis_data$Fat_g,
         type = "l", lwd = 2, col = "orange",
         xlab = "Peso (g)", ylab = "Grasa (g)",
         main = "Contenido de Grasa vs Peso")
    grid()

    plot(analysis_data$Weight, analysis_data$Ash_g,
         type = "l", lwd = 2, col = "gray",
         xlab = "Peso (g)", ylab = "Ceniza (g)",
         main = "Contenido de Ceniza vs Peso")
    grid()

    par(mfrow = c(1, 1))

  } else if (plot_type == "energy") {
    # Gráfico de densidad energética
    plot(analysis_data$Weight, analysis_data$Energy_density,
         type = "l", lwd = 2, col = "darkgreen",
         xlab = "Peso (g)", ylab = "Densidad Energética (J/g)",
         main = "Densidad Energética vs Peso")
    grid()
  }
}

#' Crear gráfico de barras de composición corporal
#'
#' Gráfico de barras apiladas mostrando composición corporal
#'
#' @param compositions Lista de composiciones o output de compare_body_compositions
#' @param labels Etiquetas para cada composición
#' @return Gráfico de barras apiladas
#' @export
plot_composition_barplot <- function(compositions, labels = NULL) {

  if (is.data.frame(compositions)) {
    # Datos ya en formato de comparación
    comp_data <- compositions
    if (is.null(labels)) {
      labels <- comp_data$Condition
    }
  } else {
    # Lista de composiciones individuales
    if (is.null(labels)) {
      labels <- names(compositions) %||% paste("Sample", 1:length(compositions))
    }

    # Convertir a data frame
    comp_data <- data.frame(
      Condition = labels,
      Water_fraction = sapply(compositions, function(x) x$water_fraction),
      Protein_fraction = sapply(compositions, function(x) x$protein_fraction),
      Fat_fraction = sapply(compositions, function(x) x$fat_fraction),
      Ash_fraction = sapply(compositions, function(x) x$ash_fraction)
    )
  }

  # Preparar matriz para barplot
  comp_matrix <- t(as.matrix(comp_data[, c("Water_fraction", "Protein_fraction",
                                           "Fat_fraction", "Ash_fraction")]))
  rownames(comp_matrix) <- c("Agua", "Proteína", "Grasa", "Ceniza")
  colnames(comp_matrix) <- labels

  # Crear gráfico de barras apiladas
  barplot(comp_matrix,
          col = c("lightblue", "red", "orange", "gray"),
          main = "Composición Corporal",
          ylab = "Fracción",
          legend.text = TRUE,
          args.legend = list(x = "topright", cex = 0.8))

  invisible(comp_matrix)
}

# Operador %||% para valores por defecto
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
