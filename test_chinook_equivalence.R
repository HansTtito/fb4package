library(fb4package)

# ==============================================================================
# 1. PARÁMETROS — especie 21 (Chinook salmon adult) desde Parameters_official.csv
# ==============================================================================
parms <- read.csv(
  "c:/Users/hkev2/OneDrive/Escritorio/packages/FB4/Parameters_official.csv",
  stringsAsFactors = FALSE
)
Sp <- 21
sp <- parms[Sp, ]
cat("Especie:", sp$Species, "\n")

# Cargar Pred_E.csv para PREDEDEQ = 1
base       <- "c:/Users/hkev2/OneDrive/Escritorio/packages/FB4/Main Inputs/"
pred_e_raw <- read.csv(paste0(base, "Pred_E.csv"))
pred_ED_vec <- approx(pred_e_raw[,1], pred_e_raw[,2], n = 366, method = "linear")$y

species_params <- list(
  consumption = list(
    CEQ = sp$CEQ, CA = sp$CA, CB = sp$CB,
    CQ  = sp$CQ,  CTO = sp$CTO, CTM = sp$CTM,
    CTL = sp$CTL, CK1 = sp$CK1, CK4 = sp$CK4
  ),
  respiration = list(
    REQ = sp$REQ, RA = sp$RA,  RB  = sp$RB,
    RQ  = sp$RQ,  RTO = sp$RTO, RTM = sp$RTM,
    RTL = sp$RTL, RK1 = sp$RK1, RK4 = sp$RK4, RK5 = sp$RK5
  ),
  activity  = list(ACT = sp$ACT, BACT = sp$BACT),
  sda       = list(SDA = sp$SDA),
  egestion  = list(EGEQ = sp$EGEQ, FA = sp$FA, FB = sp$FB, FG = sp$FG),
  excretion = list(EXEQ = sp$EXEQ, UA = sp$UA, UB = sp$UB, UG = sp$UG),
  predator  = list(
    PREDEDEQ = sp$PREDEDEQ,
    ED_data  = pred_ED_vec,   # serie completa de 366 valores
    Alpha1   = sp$Alpha1, Beta1 = sp$Beta1,
    Alpha2   = sp$Alpha2, Beta2 = sp$Beta2, Cutoff = sp$Cutoff
  )
)

# ==============================================================================
# 2. DATOS DE ENTRADA
# ==============================================================================
temp   <- read.csv(paste0(base, "Temperature.csv"))
diet   <- read.csv(paste0(base, "Diet_prop.csv"))
prey_e <- read.csv(paste0(base, "Prey_E.csv"))

days <- 1:365

temp <- data.frame(
  Day         = temp[,1],
  Temperature = temp[,2]
)

diet <- data.frame(
  Day     = days,
  Anchovy = diet$Anchovy[1],
  Sardine = diet$Sardine[1]
)

prey_e <- data.frame(
  Day     = days,
  Anchovy = prey_e$Anchovy[1],
  Sardine = prey_e$Sardine[1]
)

# Indigestible = 0% — igual que FB4 cuando no hay archivo
indigestible <- data.frame(
  Day     = days,
  Anchovy = 0,
  Sardine = 0
)

# ==============================================================================
# 3. OBJETO BIOENERGETIC
# ==============================================================================
bio <- Bioenergetic(
  species_params = species_params,
  species_info   = list(
    scientific_name = "Oncorhynchus tshawytscha",
    common_name     = "Chinook salmon",
    life_stage      = "adult"
  ),
  environmental_data = list(
    temperature = temp
  ),
  diet_data = list(
    proportions  = diet,
    prey_names   = c("Anchovy", "Sardine"),
    energies     = prey_e,
    indigestible = indigestible
  ),
  simulation_settings = list(initial_weight = 1800, duration = 365)
)

# ==============================================================================
# 4. FITTING — binary search para peso final = 3290.01 g
# ==============================================================================
resultado_pkg <- run_fb4(bio,
                         strategy  = "binary_search",
                         fit_to    = "Weight",
                         fit_value = 3290.01)

cat("\n=== RESULTADOS PAQUETE ===\n")
cat("p-value estimado:", round(resultado_pkg$summary$p_value, 6), "\n")
cat("Consumo total:   ", round(resultado_pkg$summary$total_consumption, 3), "g\n")
cat("Peso final:      ", round(resultado_pkg$summary$final_weight, 3), "g\n")
