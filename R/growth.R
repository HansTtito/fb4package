########################################################################
### Growth Function using p-value
########################################################################

grow <- function(Temperature, W, p, outpt, globalout_Prey, globalout_Prey_E) { # Growth as a function of temperature, weight, p-value, prey proportion and energy density and predator energy density
  TotSpawnE <- 0  # Initialize sum of energy lost in spawning; JEB
  TotConsG  <- 0  # Initialize sum of grams of prey consumed; used for fitting total g consumed; JEB
  #TotEgain  <- 0  # Initialize sum of energy gained in consumption; JEB
  if(outpt != "End") {  # Daily values not needed if only fitting final weight or consumption
    globalout <- data.frame(matrix(NA, nrow = Fin, ncol= (53+ncol(globalout_Prey)*4))) # Create a blank dataframe to store outputs
    colnames(globalout) <- c("Day",
                             "Temperature.C",
                             "Starting.Weight",
                             "Weight.g",
                             "Population.Number",
                             "Population.Biomass.g",
                             "Specific.Growth.Rate.J.g.d",
                             "Specific.Consumption.Rate.J.g.d",
                             "Specific.Egestion.Rate.J.g.d",
                             "Specific.Excretion.Rate.J.g.d",
                             "Specific.Respiration.Rate.J.g.d",
                             "Specific.SDA.Rate.J.g.d",
                             "Specific.Consumption.Rate.g.g.d",
                             "Specific.Growth.Rate.g.g.d",
                             "Initial.Predator.Energy.Density.J.g",
                             "Final.Predator.Energy.Density.J.g",
                             "Mean.Prey.Energy.Density.J.g",
                             "Gross.Production.g",
                             "Gross.Production.J",
                             "Cum.Gross.Production.g",
                             "Cum.Gross.Production.J",
                             "Gametic.Production.g",
                             "Cum.Gametic.Production.J", # need total spawning E; JEB
                             "Net.Production.g",
                             "Net.Production.J",
                             "Cum.Net.Production.g",
                             "Cum.Net.Production.J",
                             "Consumption.g",
                             "Consumption.J",
                             "Cum.Cons.g",
                             "Cum.Cons.J",
                             "Cons.Pop.g",
                             "Cons.Pop.J",
                             "Cum.Cons.Pop.g",
                             "Cum.Cons.Pop.J",
                             "Mortality.number",
                             "Mortality.g",
                             "Nitrogen.Egestion.g",
                             "Phosphorous.Egestion.g",
                             "N.to.P.Egestion",
                             "Nitrogen.Excretion.g",
                             "Phosphorous.Excretion.g",
                             "N.to.P.Excretion",
                             "Nitrogen.Consumption.g",
                             "Phosphorous.Consumption.g",
                             "N.to.P.Consumption",
                             "Nitrogen.Growth.g",
                             "Phosphorous.Growth.g",
                             "N.to.P.Growth",
                             "Contaminant.Clearance.Rate.ug.d",
                             "Contaminant.Uptake.ug",
                             "Contaminant.Burden.ug",
                             "Contaminant.Predator.Concentration.ug.g",
                             paste("Cons",colnames(globalout_Prey),"J", sep = " "),
                             paste("Cons",colnames(globalout_Prey),"g", sep = " "),
                             paste("Cons Pop",colnames(globalout_Prey),"J", sep = " "),
                             paste("Cons Pop",colnames(globalout_Prey),"g", sep = " "))
  }  # end of if(outpt != "End"); JEB
  #
  for(i in 1:Fin) { # Create a loop that estimates growth for the duration of the simulation (Fin)

    if(calc.pop_mort==TRUE){
      Ind2 <- globalout_individuals[i,2] # Population mortality
    }else{
      Ind2 <- 1
      Ind <- 1
    }

    Pred_E_i <- pred_En_D(W=W,day=i,PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    Pred_E_iplusone <- pred_En_D(W=W,day=(i+1),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g); only correct for PREDEDEQ==1
    Prey_ED_i <- globalout_Prey[i,]*globalout_Prey_E[i,] # vector of Prey energy densities (J/g) on day i
    mean_prey_ED <- sum(Prey_ED_i) # weighted mean prey energy density (J/g) on day i; JEB
    # Calculate consumption differently if specifying "Ration" or "Ration_prey":
    if(fit.to == "Ration") {
      # Calculate Consumption based on Ration, then use that to find the corresponding p-value for this day
      Cons.gg <- Ration  # units (g prey/g fish) per day
      Cons <- Ration*mean_prey_ED  # (J/g); Ration has units of (g prey)/(g fish); mean_prey_ED has units (J/g prey)
      Cons_p1 <- consumption(Temperature=Temperature[i], W=W, p=1, CEQ=CEQ)*mean_prey_ED # Consumption in (J/g)
      p <- Cons/Cons_p1  # Calculated daily p-value for this Ration
    } else if(fit.to == "Ration_prey") {
      # Calculate Consumption based on Ration_prey, then use that to find the corresponding p-value for this day
      Cons.gg <- Ration_prey/W  # units (g food/g fish), where Ration_prey is (g food) per day.
      Cons <- (Ration_prey/W)*mean_prey_ED  # (J/g) = ((g prey)/(g fish))*(J/g prey)
      Cons_p1 <- consumption(Temperature=Temperature[i], W=W, p=1, CEQ=CEQ)*mean_prey_ED # Consumption in (J/g)
      p <- Cons/Cons_p1  # Calculated daily p-value for this Ration_prey
    } else {
      # Now, with p-value determined or specified, calculate Cons for this day
      #Cons.gg is the consumption (g prey/g fish), using T on day i, p-value, Weight, and the specified Consumption Equation number.
      Cons.gg <- consumption(Temperature=Temperature[i], p=p, W=W, CEQ=CEQ)  # (g prey/g fish); only call once with these params; JEB
      # Cons <- consumption(Temperature=Temperature[i],p=p, W=W, CEQ=CEQ)*mean_prey_ED # Consumption in (J/g)
      Cons <- Cons.gg*mean_prey_ED # Cons = Consumption in (J/g) ; units: (J/g) = (g prey/g fish)*(J/g prey)  # JEB
    }
    # record daily values only if needed.
    if(outpt != "End") {  # Daily values not needed if only fitting final weight or cons
      # Cons_prey_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in J
      Cons_prey_J <- data.frame(t(Cons.gg*Prey_ED_i*W)) # Consumption by prey type in J  # added by JEB
      colnames(Cons_prey_J) <- paste(colnames(Cons_prey_J),"J", sep = " ")
      # Cons_prey_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Consumption by prey in g
      Cons_prey_G <- data.frame(t(Cons.gg*(globalout_Prey[i,]*W))) # Consumption by prey type in g  # added by JEB
      colnames(Cons_prey_G) <- paste(colnames(Cons_prey_G),"g", sep = " ")
      # Cons_prey_pop_J <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in J
      Cons_prey_pop_J <- data.frame(t(Cons.gg*Prey_ED_i*W*Ind)) # Population consumption by prey in J  # added by JEB
      colnames(Cons_prey_pop_J) <- paste(colnames(Cons_prey_pop_J),"pop.J", sep = " ")
      # Cons_prey_pop_G <- data.frame(t(consumption(Temperature=Temperature[i], W=W, p=p, CEQ=CEQ)* # Population consumption by prey in g
      Cons_prey_pop_G <- data.frame(t(Cons.gg*(globalout_Prey[i,]*W*Ind))) # Population consumption by prey in g  # added by JEB
      colnames(Cons_prey_pop_G) <- paste(colnames(Cons_prey_pop_G),"pop.g", sep = " ")
    }

    Eg  <- egestion(C=Cons,Temperature=Temperature[i],p=p, EGEQ=EGEQ) # Egestion in J/g
    Ex  <- excretion(C=Cons, Eg=Eg,Temperature=Temperature[i], p=p, EXEQ=EXEQ) # Excretion in J/g
    SpecDA  <- SpDynAct(C=Cons,Eg=Eg) # Specific dynamic action in J/g
    Res  <- respiration(Temperature=Temperature[i], W=W, REQ)*Oxycal #  respiration in (J/g) = (g O2/g)*(J/g O2); Oxycal = 13560 J/g O2
    R.O2 <- (Res + SpecDA)/Oxycal # respiration in (g O2/g); used in some contaminant models

    G <-  Cons - (Res + Eg + Ex + SpecDA) # Energy put towards growth in J/g

    egain  <-  (G * W)      # net energy gain in J
    #Now account for spawning loss of energy; JEB
    if(calc.spawn==TRUE){ # Spawning function
      spawn <- Reproduction[i]
    }else{
      spawn <- 0
    }
    # I think spawning should appear as another daily loss, so use spawn*W*Pred_E_i; JEB
    #   Not spawn at end of day.
    #TotSpawnE <- TotSpawnE + spawn*W*Pred_E_i  # Better to spawn during day i; Total E lost in reproduction; JEB
    #TotSpawnE <- TotSpawnE + spawn*finalwt*Pred_E_i  # Caution! If using finalwt, then use Pred_E_iplusone to balance energy; Total E lost in reproduction; JEB
    SpawnE <- spawn*W*Pred_E_i  # use W at start of day and energy density at start of day; JEB
    TotSpawnE <- TotSpawnE + SpawnE  # Cumulative Total Energy lost in reproduction so far; JEB

    # Calculate finalwt = weight at end of day i, and corresponding Predator energy density at end of day i; JEB
    if(PREDEDEQ == 3) {  # calculating ED as power function of weight
      finalwt <- ((egain -SpawnE +(Pred_E_i*W))/alpha1)^(1/(beta1+1))
      Pred_E_iplusone <- pred_En_D(W=finalwt,day=(i),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    }else if(PREDEDEQ == 2){  # estimate ED from weight using one of two line segments
      Wco = as.numeric(cutoff)  # weight at cutoff
      if(W < Wco){    # weight (W) at start of day is below cutoff.
        if(beta1 != 0){  # use quadratic formula to calc finalwt
          flagvalue1 <- ((alpha1*alpha1 +4*beta1*(W*(alpha1+beta1*W) +egain -SpawnE)))
          if(is.na(flagvalue1)){prt.msg(nR,i,W,Temperature[i],p,outpt,1,1);warning("fv1: Number inside sqrt is NaN: not a number. Fish lost too much weight.")
          }else if(flagvalue1 < 0){prt.msg(nR,i,W,Temperature[i],p,outpt,2,2);warning("fv1: Number inside sqrt is negative. Fish lost too much weight.")}
          finalwt <- (-alpha1 +sqrt(alpha1*alpha1 +4*beta1*(W*(alpha1+beta1*W) +egain -SpawnE)))/(2*beta1)
        }else if(beta1 == 0){  # can't use quadratic formula; ED = alpha1
          finalwt = (egain -SpawnE +W*alpha1)/alpha1
        }
        if(finalwt > Wco){  # if new wt crosses cutoff, recalculate finalwt to correctly account for this
          egainCo = Wco*(alpha1+beta1*Wco) - W*(alpha1+beta1*W)  # energy needed to reach cutoff from W < Wco
          if(beta2 != 0){  # accounting for energy needed to reach cutoff and beyond, calc new wt
            flagvalue2 <- (alpha2*alpha2 +4*beta2*(egain-SpawnE -egainCo +Wco*(alpha1+beta1*Wco)))
            if(is.na(flagvalue2)){prt.msg(nR,i,W,Temperature[i],p,outpt,1,3);warning("fv2: Number inside sqrt is NaN: not a number. Fish lost too much weight.")
            }else if(flagvalue2 < 0){prt.msg(nR,i,W,Temperature[i],p,outpt,2,4);warning("fv2: Number inside sqrt is negative. Fish lost too much weight.")}
            finalwt = (-alpha2 +sqrt(alpha2*alpha2 +4*beta2*(egain-SpawnE -egainCo +Wco*(alpha1+beta1*Wco))))/(2*beta2)
          }else if(beta2 == 0){  # then grow to cutoff, with ED = alpha2 beyond cutoff
            finalwt = (egain-SpawnE -egainCo +Wco*(alpha1+beta1*Wco))/alpha2
          }
        }
      }else if(W >= Wco){   # weight (W) at start of day is above cutoff.
        if(beta2 != 0){  # use quadratic formula to calc finalwt
          flagvalue3 <- ((alpha2*alpha2 +4*beta2*(W*(alpha2+beta2*W) +egain -SpawnE)))
          if(is.na(flagvalue3)){prt.msg(nR,i,W,Temperature[i],p,outpt,1,5);warning("fv3: Number inside sqrt is NaN: not a number. Fish lost too much weight.")
          }else if(flagvalue3 < 0){prt.msg(nR,i,W,Temperature[i],p,outpt,2,6);warning("fv3: Number inside sqrt is negative. Fish lost too much weight.")}
          finalwt <- (-alpha2 +sqrt(alpha2*alpha2 +4*beta2*(W*(alpha2+beta2*W) +egain -SpawnE)))/(2*beta2)
        }else if(beta2 == 0){  # can't use quadratic formula; ED = alpha1
          finalwt = (egain -SpawnE +W*alpha2)/alpha2
        }
        if(finalwt < Wco){  # if new wt decreases below cutoff, recalculate finalwt to account for this
          elossCo = W*(alpha2+beta2*W) - Wco*(alpha1+beta1*Wco)  # energy loss needed to reach cutoff from W
          if(beta1 != 0){  # accounting for energy to reach cutoff and beyond, calc new weight
            flagvalue4 <- (alpha1*alpha1 +4*beta1*(egain-SpawnE +elossCo +Wco*(alpha1+beta1*Wco)))
            if(is.na(flagvalue4)){prt.msg(nR,i,W,Temperature[i],p,outpt,1,7);warning("fv4: Number inside sqrt is NaN: not a number. Fish lost too much weight.")
            }else if(flagvalue4 < 0){prt.msg(nR,i,W,Temperature[i],p,outpt,2,8);warning("fv4: Number inside sqrt is negative. Fish lost too much weight.")}
            partwt1 = sqrt(alpha1*alpha1 +4*beta1*(egain-SpawnE +elossCo +Wco*(alpha1+beta1*Wco)))
            finalwt = (-alpha1 +partwt1)/(2*beta1)
            testfinalwt = finalwt
          }else if(beta1 == 0){  # then decline to cutoff, with ED = alpha1 below cutoff
            finalwt = (egain-SpawnE +elossCo +Wco*alpha1)/alpha1
          }
        }
      }
      Pred_E_iplusone <- pred_En_D(W=finalwt,day=(i),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g)
    }else if(PREDEDEQ == 1){   # if PREDEDEQ == 1: then use 'day=(i+1)' to interpolate from the input file
      Pred_E_iplusone <- pred_En_D(W=W,day=(i+1),PREDEDEQ=PREDEDEQ) # Predator energy density (J/g) is from csv file
      finalwt <- (egain -SpawnE +(Pred_E_i*W))/Pred_E_iplusone  # For PREDEDEQ ==1, Pred_E_iplusone
    }

    # finalwt is Predator weight (g) at end of current day
    if(finalwt < 0){prt.msg(nR,i,finalwt,Temperature[i],p,outpt,3,10)}  # print msg if weight becomes negative.
    if(finalwt < 0){W <- finalwt; break}  # break out of daily "for" loop; (Skip p-values that let W go negative.)

    weightgain  <-  finalwt-W  	#change in g/day


    #Cons_cont: vector of (g prey consumed), by prey type;
    #  Note: Cons.gg is now computed (above) for all values of "fit.to"; JEB
    Cons_cont <- (Cons.gg*W)*globalout_Prey[i,]  # vector of (g prey), by prey type; use Cons.gg to avoid call to Consumption(); JEB

    if(calc.nut==TRUE){
      Phos <- phosphorous_allocation(C=Cons_cont,p_conc_prey=globalout_Phos_Conc_Prey[i,],AEp=globalout_Phos_Ae[i,],weightgain=weightgain,p_conc_pred=globalout_Phos_Conc_Pred[i,])
      Nit <- nitrogen_allocation(C=Cons_cont,n_conc_prey=globalout_Nit_Conc_Prey[i,],AEn=globalout_Nit_Ae[i,],weightgain=weightgain,n_conc_pred=globalout_Nit_Conc_Pred[i,])
    } else{
      Phos <- NA
      Nit <- NA
    }

    if(calc.contaminant==TRUE){
      #Cont <- pred_cont_conc_old(C=Cons_cont,W=finalwt,Temperature=Temperature,X_Prey=globalout_Prey_Conc[i,],X_Pred=X_Pred,TEx=globalout_Trans_eff[i,],X_ae=globalout_Prey_ass[i,],CONTEQ=CONTEQ)
      Cont <- pred_cont_conc(R.O2=R.O2,C=Cons_cont,W=W,Temperature=Temperature[i],
                             X_Prey=globalout_Prey_Conc[i,],X_Pred=X_Pred,
                             TEx=globalout_Trans_eff[i,],X_ae=globalout_Prey_ass[i,],
                             Ew=Ew, Kbw=Kbw, CONTEQ=CONTEQ)
      X_Pred <- Cont[3]/finalwt  # Calc new burden for this day, then calc conc at end of day
      Cont[4] = X_Pred
    } else {
      Cont <- NA
    }

    ConsW <- Cons*W  # units (J) = (J/g)*(g); save a repeated multiplication

    if(outpt != "End") {  # Daily values not needed if only fitting final weight or cons
      ConsWInd <- ConsW*Ind  # save a repeated multiplication
      globalout[i,"Day"] <- Day_Temp[i]              ## row 1 in globalout data.frame
      globalout[i,"Temperature.C"]<-Temperature[i]   ## row 2
      globalout[i,"Starting.Weight"]<-W              ## row 3
      globalout[i,"Weight.g"]<-finalwt  # spawning loss already accounted for; ## row 4
      globalout[i,"Population.Number"]<-Ind2         ## row 5
      globalout[i,"Population.Biomass.g"]<-finalwt*Ind    ## row 6
      globalout[i,"Specific.Growth.Rate.J.g.d"]<-G        ## row 7
      globalout[i,"Specific.Consumption.Rate.J.g.d"]<-Cons  # (J/g) ## row 8
      globalout[i,"Specific.Egestion.Rate.J.g.d"]<-Eg     ## row 9
      globalout[i,"Specific.Excretion.Rate.J.g.d"]<-Ex    ## row 10
      globalout[i,"Specific.Respiration.Rate.J.g.d"]<-Res ## row 11
      globalout[i,"Specific.SDA.Rate.J.g.d"]<-SpecDA      ## row 12
      globalout[i,"Specific.Consumption.Rate.g.g.d"]<-Cons/mean_prey_ED # (g/g) ## row 13
      globalout[i,"Specific.Growth.Rate.g.g.d"]<-weightgain/W # G/pred_E_iplusone ## row 14; fixed in v1.1.3; JEB(was G/mean_prey_ED)
      globalout[i,"Initial.Predator.Energy.Density.J.g"]<-Pred_E_i         ## row 15
      globalout[i,"Final.Predator.Energy.Density.J.g"]<-Pred_E_iplusone    ## row 16
      globalout[i,"Mean.Prey.Energy.Density.J.g"]<-mean_prey_ED  # (J/g)   ## row 17
      globalout[i,"Gross.Production.g"]<-(Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i ## row 18
      globalout[i,"Gross.Production.J"]<-(Cons + Res + Eg + Ex + SpecDA)*W          ## row 19
      globalout[i,"Cum.Gross.Production.g"]<-cumsum((Cons + Res + Eg + Ex + SpecDA)*W/Pred_E_i) ## row 20
      globalout[i,"Cum.Gross.Production.J"]<-cumsum((Cons + Res + Eg + Ex + SpecDA)*W) ## row 21
      globalout[i,"Gametic.Production.g"]<-spawn*W  # (g); JEB; was spawn*finalwt   ## row 22
      globalout[i,"Cum.Gametic.Production.J"]<-TotSpawnE # (J); Cumulative energy (J) for spawning; ## row 23
      globalout[i,"Net.Production.g"]<-weightgain  # includes spawning losses; JEB  ## row 24
      globalout[i,"Net.Production.J"]<-egain  # not including spawning; JEB         ## row 25
      globalout[i,"Cum.Net.Production.g"]<-cumsum(weightgain)  ## row 26
      globalout[i,"Cum.Net.Production.J"]<-cumsum(egain)## row 27
      globalout[i,"Consumption.g"]<-ConsW/mean_prey_ED  ## row 28
      globalout[i,"Consumption.J"]<-ConsW               ## row 29
      globalout[i,"Cum.Cons.g"]<-cumsum(ConsW/mean_prey_ED) ## row 30
      globalout[i,"Cum.Cons.J"]<-cumsum(ConsW)          ## row 31
      globalout[i,"Cons.Pop.g"]<-ConsWInd/mean_prey_ED  ## row 32
      globalout[i,"Cons.Pop.J"]<-ConsWInd               ## row 33
      globalout[i,"Cum.Cons.Pop.g"]<-cumsum(ConsWInd/mean_prey_ED) ## row 34
      globalout[i,"Cum.Cons.Pop.J"]<-cumsum(ConsWInd)   ## row 35
      globalout[i,"Mortality.number"]<-Ind-Ind2         ## row 36
      globalout[i,"Mortality.g"]<-(Ind-Ind2)*W          ## row 37
      globalout[i,"Nitrogen.Egestion.g"]<-Nit[4]        ## row 38
      globalout[i,"Phosphorous.Egestion.g"]<-Phos[4]    ## row 39
      globalout[i,"N.to.P.Egestion"]<-Nit[4]/Phos[4]    ## row 40
      globalout[i,"Nitrogen.Excretion.g"]<-Nit[3]       ## row 41
      globalout[i,"Phosphorous.Excretion.g"]<-Phos[3]   ## row 42
      globalout[i,"N.to.P.Excretion"]<-Nit[3]/Phos[3]   ## row 43
      globalout[i,"Nitrogen.Consumption.g"]<-Nit[1]     ## row 44
      globalout[i,"Phosphorous.Consumption.g"]<-Phos[1] ## row 45
      globalout[i,"N.to.P.Consumption"]<-Nit[1]/Phos[1] ## row 46
      globalout[i,"Nitrogen.Growth.g"]<-Nit[2]   ## row 47
      globalout[i,"Phosphorous.Growth.g"]<-Phos[2]    ## row 48
      globalout[i,"N.to.P.Growth"]<-Nit[2]/Phos[2]    ## row 49
      globalout[i,"Contaminant.Clearance.Rate.ug.d"]<-Cont[1]   ## row 50
      globalout[i,"Contaminant.Uptake.ug"]<-Cont[2]   ## row 51
      globalout[i,"Contaminant.Burden.ug"]<-Cont[3]   ## row 52
      globalout[i,"Contaminant.Predator.Concentration.ug.g"]<-Cont[4]   ## row 53
      globalout[i,paste("Cons",colnames(globalout_Prey),"J", sep = " ")]<-Cons_prey_J
      globalout[i,paste("Cons",colnames(globalout_Prey),"g", sep = " ")] <-Cons_prey_G
      globalout[i,paste("Cons Pop",colnames(globalout_Prey),"J", sep = " ")]<-Cons_prey_pop_J
      globalout[i,paste("Cons Pop",colnames(globalout_Prey),"g", sep = " ")] <-Cons_prey_pop_G
    }
    #globalout<-cbind(globalout,Cons_prey)
    TotConsG  <- TotConsG + ConsW/mean_prey_ED  # Tot g cons; JEB
    # TotSpawnE is total energy lost at spawning; value now calculated earlier and passed in globalout; JEB
    #W <- finalwt-(spawn*finalwt) # spawning loss is accounted for earlier
    W <- finalwt  # Weight at the end of the day serves as the starting weight for the next day
    Ind <- Ind2   # N at end of the day (Ind2) is the starting N (Ind) for the next day
  } # end of "for" loop for days
  if(outpt != "End") {  # Daily values not needed if only fitting final weight or cons
    globalout[,c("Cum.Gross.Production.g","Cum.Gross.Production.J","Cum.Net.Production.g","Cum.Net.Production.J","Cum.Cons.g","Cum.Cons.J","Cum.Cons.Pop.g","Cum.Cons.Pop.J")] <-
      cumsum( globalout[,c("Cum.Gross.Production.g","Cum.Gross.Production.J","Cum.Net.Production.g","Cum.Net.Production.J","Cum.Cons.g","Cum.Cons.J","Cum.Cons.Pop.g","Cum.Cons.Pop.J")])
  }
  if(outpt == "vector")                         {return(globalout)} # return entire set of daily values
  if(outpt == "End" && fit.to=="Weight")        {return(W)} # only ending W value is needed; JEB
  if(outpt == "End" && fit.to=="Consumption")   {return(TotConsG)} # only ending Total Consumption value is needed; JEB
  if(outpt == "final" && fit.to=="Weight")      {return(globalout[Fin,4])}
  if(outpt == "final" && fit.to=="Consumption") {return(sum(globalout[,'Specific.Consumption.Rate.g.g.d']*globalout[,'Starting.Weight']))}
  if(outpt == "final" && fit.to=="p-value")     {return(sum(globalout[,'Specific.Consumption.Rate.g.g.d']*globalout[,'Starting.Weight']))}

}  # end of function grow()
