

setClass(
  "Bioenergetic",
  parameters = list(
    Species = "character",
    Scientific_name = 'character',
    initial_day = 'numeric',
    final_day = 'numeric',
    initial_weight = "numeric",
    oxycalorific_coefficient = 'numeric',
    fit_to = 'character',
    extra_value = 'numeric',
    consumption_equation = 'numeric',
    CA = 'numeric',
    CB = 'numeric',
    CQ = 'numeric',
    CTO = 'numeric',
    CTM = 'numeric',
    CTL = 'numeric',
    CK1 = 'numeric',
    CK4 = 'numeric',
    respiration_equation = 'numeric',
    RA = 'numeric',
    RB = 'numeric',
    RQ = 'numeric',
    RTO = 'numeric',
    RTM = 'numeric',
    RTL = 'numeric',
    RK1 = 'numeric',
    RK4 = 'numeric',
    RK5 = 'numeric',
    ACT = 'numeric',
    BACT = 'numeric',
    SDA = 'numeric',
    egestion_equation = 'numeric',
    FA = 'numeric',
    FB = 'numeric',
    FG = 'numeric',
    excretion_equation = 'numeric',
    UA = 'numeric',
    UB = 'numeric',
    UG = 'numeric',
    predator_equation = 'numeric',
    energy_density = 'numeric',
    alpha_1 = 'numeric',
    beta_1 = 'numeric',
    cutoff = 'numeric',
    alpha_2 = 'numeric',
    beta_2 = 'numeric'
  )
)



# Constructor
crear_bioenergetic <- function(
    Species, Scientific_name, initial_day, final_day, initial_weight, oxycalorific_coefficient,
    fit_to, extra_value, consumption_equation, CA, CB, CQ, CTO, CTM, CTL, CK1, CK4,
    respiration_equation, RA, RB, RQ, RTO, RTM, RTL, RK1, RK4, RK5, ACT, BACT, SDA,
    egestion_equation, FA, FB, FG, excretion_equation, UA, UB, UG, predator_equation,
    energy_density, alpha_1, beta_1, cutoff, alpha_2, beta_2
) {

  if (initial_day >= final_day) {
    stop("final day must be greater than initial day")
  }

  if (!(consumption_equation %in% 1:4)) {
    stop("consumption equation must be between 1 and 4")
  }

  if (!(respiration_equation %in% 1:4)) {
    stop("respiration equation must be between 1 and 4")
  }

  if (!(egestion_equation %in% 1:4)) {
    stop("egestion equation must be between 1 and 4")
  }

  if (!(excretion_equation %in% 1:4)) {
    stop("excretion equation must be between 1 and 4")
  }

  if (!(predator_equation %in% 1:4)) {
    stop("predator equation must be between 1 and 4")
  }


  new("Bioenergetic",
      Species = Species,
      Scientific_name = Scientific_name,
      initial_day = initial_day,
      final_day = final_day,
      initial_weight = initial_weight,
      oxycalorific_coefficient = oxycalorific_coefficient,
      fit_to = fit_to,
      extra_value = extra_value,
      consumption_equation = consumption_equation,
      CA = CA,
      CB = CB,
      CQ = CQ,
      CTO = CTO,
      CTM = CTM,
      CTL = CTL,
      CK1 = CK1,
      CK4 = CK4,
      respiration_equation = respiration_equation,
      RA = RA,
      RB = RB,
      RQ = RQ,
      RTO = RTO,
      RTM = RTM,
      RTL = RTL,
      RK1 = RK1,
      RK4 = RK4,
      RK5 = RK5,
      ACT = ACT,
      BACT = BACT,
      SDA = SDA,
      egestion_equation = egestion_equation,
      FA = FA,
      FB = FB,
      FG = FG,
      excretion_equation = excretion_equation,
      UA = UA,
      UB = UB,
      UG = UG,
      predator_equation = predator_equation,
      energy_density = energy_density,
      alpha_1 = alpha_1,
      beta_1 = beta_1,
      cutoff = cutoff,
      alpha_2 = alpha_2,
      beta_2 = beta_2
  )
}




