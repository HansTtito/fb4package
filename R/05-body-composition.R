#' Funciones de Composición Corporal para el Modelo FB4
#'
#' @name body-composition
#' @aliases body-composition
NULL

# ============================================================================
# FUNCIONES CORE DE COMPOSICIÓN CORPORAL
# ============================================================================

#' Estimar contenido de proteína a partir del agua
#'
#' Estima gramos de proteína basado en el contenido de agua usando la
#' regresión de Breck (2014)
#'
#' @param water_content Contenido de agua (g)
#' @return Contenido de proteína (g)
#' @keywords internal
#' @references Breck, J.E. 2014. Body composition in fishes: body size matters. Aquaculture 433:40-49.
calculate_protein_from_water <- function(water_content) {
  if (is.na(water_content) || water_content <= 0) {
    return(0)
  }
  
  # Regresión de Breck (2014), N = 101, r² = 0.9917
  # log10(Protein) = -0.8068 + 1.0750 * log10(H2O)
  protein <- 10^(-0.8068 + 1.0750 * log10(water_content))
  
  return(pmax(0, protein))
}

#' Estimar contenido de ceniza a partir del agua
#'
#' Estima gramos de ceniza basado en el contenido de agua usando la
#' regresión de Breck (2014)
#'
#' @param water_content Contenido de agua (g)
#' @return Contenido de ceniza (g)
#' @keywords internal
#' @references Breck, J.E. 2014. Body composition in fishes: body size matters. Aquaculture 433:40-49.
calculate_ash_from_water <- function(water_content) {
  if (is.na(water_content) || water_content <= 0) {
    return(0)
  }
  
  # Regresión de Breck (2014), N = 101, r² = 0.9932
  # log10(Ash) = -1.6765 + 1.0384 * log10(H2O)
  ash <- 10^(-1.6765 + 1.0384 * log10(water_content))
  
  return(pmax(0, ash))
}

#' Calcular contenido de grasa por sustracción
#'
#' Calcula gramos de grasa restando agua, proteína y ceniza del peso total
#'
#' @param total_weight Peso total húmedo (g)
#' @param water_content Contenido de agua (g)
#' @param protein_content Contenido de proteína (g)
#' @param ash_content Contenido de ceniza (g)
#' @return Contenido de grasa (g)
#' @keywords internal
calculate_fat_by_subtraction <- function(total_weight, water_content, protein_content, ash_content) {
  
  fat_content <- total_weight - water_content - protein_content - ash_content
  
  # Validar que la grasa no sea negativa
  if (fat_content < 0) {
    fat_content <- 0
  }
  
  # Validar límites biológicos (máximo ~35% del peso)
  max_fat <- total_weight * 0.35
  if (fat_content > max_fat) {
    fat_content <- max_fat
  }
  
  return(fat_content)
}

#' Calcular densidad energética a partir de grasa y proteína
#'
#' @param fat_content Contenido de grasa (g)
#' @param protein_content Contenido de proteína (g)
#' @param total_weight Peso total (g)
#' @param fat_energy Energía por gramo de grasa (J/g)
#' @param protein_energy Energía por gramo de proteína (J/g)
#' @return Densidad energética (J/g peso húmedo)
#' @keywords internal
calculate_energy_density <- function(fat_content, protein_content, total_weight, 
                                     fat_energy = 36200, protein_energy = 23600) {
  
  if (total_weight <= 0) return(4500)  # Valor por defecto
  
  # Calcular densidad energética
  energy_density <- (fat_content * fat_energy + protein_content * protein_energy) / total_weight
  
  # Limitar a rango biológico típico
  energy_density <- clamp(energy_density, 2000, 10000)
  
  return(energy_density)
}

# ============================================================================
# FUNCIÓN PRINCIPAL DE COMPOSICIÓN CORPORAL
# ============================================================================

#' Calcular composición corporal completa
#'
#' Función principal que calcula todos los componentes de la composición corporal
#' y la densidad energética a partir del peso y fracción de agua
#'
#' @param weight Peso total húmedo (g)
#' @param water_fraction Fracción de agua (0-1), por defecto 0.728
#' @param fat_energy Energía por gramo de grasa (J/g)
#' @param protein_energy Energía por gramo de proteína (J/g)
#' @return Lista con composición corporal completa
#' @export
calculate_body_composition <- function(weight, 
                                       water_fraction = 0.728, 
                                       fat_energy = 39300, 
                                       protein_energy = 23600) {
  
  # Validar entradas
  weight <- check_numeric_value(weight, "weight", min_val = 0.001)
  water_fraction <- check_numeric_value(water_fraction, "water_fraction", min_val = 0.4, max_val = 0.9)
  
  # Calcular contenido de agua
  water_content <- water_fraction * weight
  
  # Calcular otros componentes usando regresiones de Breck (2014)
  protein_content <- calculate_protein_from_water(water_content)
  ash_content <- calculate_ash_from_water(water_content)
  fat_content <- calculate_fat_by_subtraction(weight, water_content, protein_content, ash_content)
  
  # Calcular densidad energética
  energy_density <- calculate_energy_density(fat_content, protein_content, weight, fat_energy, protein_energy)
  
  # Calcular fracciones
  water_frac <- water_content / weight
  protein_frac <- protein_content / weight
  ash_frac <- ash_content / weight
  fat_frac <- fat_content / weight
  
  # Verificar balance
  total_fraction <- water_frac + protein_frac + ash_frac + fat_frac
  balanced <- abs(total_fraction - 1) < 0.05
  
  return(list(
    # Información básica
    total_weight = weight,
    
    # Contenidos absolutos (g)
    water_g = water_content,
    protein_g = protein_content,
    ash_g = ash_content,
    fat_g = fat_content,
    
    # Fracciones
    water_fraction = water_frac,
    protein_fraction = protein_frac,
    ash_fraction = ash_frac,
    fat_fraction = fat_frac,
    
    # Energía
    energy_density = energy_density,
    total_energy = energy_density * weight,
    
    # Validación
    total_fraction = total_fraction,
    balanced = balanced
  ))
}

# ============================================================================
# FUNCIONES DE ANÁLISIS
# ============================================================================

#' Analizar composición corporal por rango de tamaños
#'
#' @param weight_range Rango de pesos a analizar (vector de 2 elementos)
#' @param n_points Número de puntos para analizar
#' @param water_fraction Fracción de agua (constante o función del peso)
#' @return Data frame con análisis de composición por tamaño
#' @export
analyze_composition_by_size <- function(weight_range = c(1, 500), 
                                        n_points = 50, 
                                        water_fraction = 0.728) {
  
  # Crear secuencia de pesos
  weights <- seq(weight_range[1], weight_range[2], length.out = n_points)
  
  # Permitir fracción de agua variable si es función
  if (is.function(water_fraction)) {
    water_fractions <- sapply(weights, water_fraction)
  } else {
    water_fractions <- rep(water_fraction, n_points)
  }
  
  # Calcular composiciones
  compositions <- mapply(
    calculate_body_composition,
    weight = weights,
    water_fraction = water_fractions,
    SIMPLIFY = FALSE
  )
  
  # Convertir a data frame
  result_df <- data.frame(
    Weight = weights,
    Water_g = sapply(compositions, function(x) x$water_g),
    Protein_g = sapply(compositions, function(x) x$protein_g),
    Ash_g = sapply(compositions, function(x) x$ash_g),
    Fat_g = sapply(compositions, function(x) x$fat_g),
    Water_fraction = sapply(compositions, function(x) x$water_fraction),
    Protein_fraction = sapply(compositions, function(x) x$protein_fraction),
    Ash_fraction = sapply(compositions, function(x) x$ash_fraction),
    Fat_fraction = sapply(compositions, function(x) x$fat_fraction),
    Energy_density = sapply(compositions, function(x) x$energy_density)
  )
  
  return(result_df)
}


# ============================================================================
# FUNCIONES DE VALIDACIÓN
# ============================================================================

#' Validar composición corporal
#'
#' @param composition Lista de composición corporal
#' @return Lista con resultados de validación
#' @export
validate_body_composition <- function(composition) {
  
  validation <- list(
    valid = TRUE,
    warnings = character(),
    errors = character()
  )
  
  # Rangos típicos para peces (fracciones)
  typical_ranges <- list(
    water = c(0.65, 0.85),
    protein = c(0.10, 0.25),
    ash = c(0.02, 0.08),
    fat = c(0.02, 0.25),
    energy_density = c(2000, 8000)
  )
  
  # Validar rangos
  if (composition$water_fraction < typical_ranges$water[1] || 
      composition$water_fraction > typical_ranges$water[2]) {
    validation$warnings <- c(validation$warnings,
                             paste("Fracción de agua fuera de rango típico:",
                                   round(composition$water_fraction, 3)))
  }
  
  if (composition$protein_fraction < typical_ranges$protein[1] || 
      composition$protein_fraction > typical_ranges$protein[2]) {
    validation$warnings <- c(validation$warnings,
                             paste("Fracción de proteína fuera de rango típico:",
                                   round(composition$protein_fraction, 3)))
  }
  
  if (composition$energy_density < typical_ranges$energy_density[1] || 
      composition$energy_density > typical_ranges$energy_density[2]) {
    validation$warnings <- c(validation$warnings,
                             paste("Densidad energética fuera de rango típico:",
                                   round(composition$energy_density, 0), "J/g"))
  }
  
  # Validar balance de fracciones
  if (!composition$balanced) {
    validation$errors <- c(validation$errors, 
                           paste("Las fracciones no suman ~1.0:",
                                 round(composition$total_fraction, 3)))
    validation$valid <- FALSE
  }
  
  # Validar valores negativos
  if (any(c(composition$water_g, composition$protein_g, 
            composition$ash_g, composition$fat_g) < 0)) {
    validation$errors <- c(validation$errors, "Componentes negativos detectados")
    validation$valid <- FALSE
  }
  
  return(validation)
}


# ============================================================================
# FUNCIONES ESPECÍFICAS PARA SIMULACIONES FB4
# ============================================================================

#' Actualizar composición corporal durante simulación
#'
#' Actualiza la composición corporal conforme el pez crece o cambia de condición
#'
#' @param old_weight Peso anterior (g)
#' @param new_weight Peso nuevo (g)
#' @param old_composition Composición anterior (opcional)
#' @param water_fraction_new Fracción de agua para nuevo peso
#' @return Nueva composición corporal
#' @export
update_body_composition <- function(old_weight, new_weight, old_composition = NULL, 
                                    water_fraction_new = 0.728) {
  
  # Calcular nueva composición
  new_composition <- calculate_body_composition(new_weight, water_fraction_new)
  
  # Si tenemos composición anterior, calcular cambios
  if (!is.null(old_composition)) {
    changes <- list(
      weight_change = new_weight - old_weight,
      water_change = new_composition$water_g - old_composition$water_g,
      protein_change = new_composition$protein_g - old_composition$protein_g,
      fat_change = new_composition$fat_g - old_composition$fat_g,
      energy_density_change = new_composition$energy_density - old_composition$energy_density
    )
    
    new_composition$changes <- changes
  }
  
  return(new_composition)
}


