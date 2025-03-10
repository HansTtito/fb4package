########################################################################
### Mortality
########################################################################

FB4.Param2 = ("calc.pop_mort")
FB4.Value2 = (as.logical(calc.pop_mort))
if(calc.pop_mort==TRUE){   # only read Mortality_File if needed
  if(file.exists(Mortality_File)){
    Mortality <- read.csv(Mortality_File,head=TRUE, stringsAsFactors = FALSE)
  } else {
    print(paste("Cannot find Mortality_File: ",Mortality_File))
    exit()
  }
  FB4.Param2 = append(FB4.Param2, "Mortality_File")
  FB4.Value2 = append(FB4.Value2,  as.character(Mortality_File))  # record file used
  Day_mort <- Mortality[,1] # Days
  mort_types <- (ncol(Mortality))-1
  last_day_mort <- tail(Day_mort, n = 1)  # get the total number of days
  globalout_mort <- NULL

  for(j in 1:mort_types){
    Mort <- Mortality[,j+1]
    Mort <- approx(Day_mort,Mort, n = last_day_mort,method="constant")$y # interpolate mortality
    Mort <- Mort[First_day:Last_day]
    globalout_mort <- cbind(globalout_mort,Mort)
    Mortality <- read.csv(Mortality_File,head=TRUE, stringsAsFactors = FALSE)
  }

  colnames(globalout_mort) <- names(Mortality)[-1]
  globalout_mort <- data.frame(globalout_mort)
  globalout_mort$Nat_Mort_Int <- NA
  globalout_mort$Fish_Mort_Int <- NA

  days <- nrow(globalout_mort)
  intervals <- 1
  nat_int_start <- 1
  nat_int_dur <- 0
  fish_int_start <- 1
  fish_int_dur <- 0

  for(i in 1:(days-1)){
    nat_int_dur <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],nat_int_dur+1,nat_int_dur+1)
    globalout_mort$Nat_Mort_Int[nat_int_start:i] <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],rep(nat_int_dur,nat_int_dur),NA)
    globalout_mort$Nat_Mort_Int[nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-nat_int_start,NA)
    globalout_mort$Nat_Mort_Int[nat_int_start:nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-nat_int_start+1,globalout_mort$Nat_Mort_Int[nat_int_start:i])
    nat_int_dur <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],0,nat_int_dur)
    nat_int_start <- ifelse(globalout_mort$natural[i+1]!=globalout_mort$natural[i],i+1,nat_int_start)

    fish_int_dur <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],fish_int_dur+1,fish_int_dur+1)
    globalout_mort$Fish_Mort_Int[fish_int_start:i] <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],rep(fish_int_dur,fish_int_dur),NA)
    globalout_mort$Fish_Mort_Int[nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-fish_int_start,NA)
    globalout_mort$Fish_Mort_Int[fish_int_start:nrow(globalout_mort)] <- ifelse(i+1==nrow(globalout_mort),i+1-fish_int_start+1,globalout_mort$Fish_Mort_Int[fish_int_start:i])
    fish_int_dur <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],0,fish_int_dur)
    fish_int_start <- ifelse(globalout_mort$fishing[i+1]!=globalout_mort$fishing[i],i+1,fish_int_start)
  }

  Individuals <- Ind
  globalout_individuals <- NULL

  for(i in 1:(Last_day-First_day+1)){
    Z <- exp(log(1-globalout_mort$natural[i])/globalout_mort$Nat_Mort_Int[i]+log(1-globalout_mort$fishing[i])/globalout_mort$Fish_Mort_Int[i]-
               (log(1-globalout_mort$natural[i])/globalout_mort$Nat_Mort_Int[i]*log(1-globalout_mort$fishing[i])/globalout_mort$Fish_Mort_Int[i]))
    Individuals <- Individuals*Z
    globalout_individuals <- rbind(globalout_individuals,data.frame(day=i,individuals=Individuals))
  }
  globalout_individuals$day <- First_day:Last_day
} else {
  FB4.Param2 = append(FB4.Param2, "Mortality_File")
  FB4.Value2 = append(FB4.Value2,  "NA")  #  file not used
} # end of mortality section
