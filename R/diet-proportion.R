########################################################################
### Diet proportions and energetical contribution
########################################################################

if(file.exists(Diet_prop_File)){
  Diet_prop <- read.csv(Diet_prop_File,head=TRUE, stringsAsFactors = FALSE)
} else {
  print(paste("Cannot find Diet_prop_File: ",Diet_prop_File))
  exit()
}
Day_prey <- Diet_prop[,1] # Days
if(file.exists(Prey_E_File)){
  Prey_E <- read.csv(Prey_E_File,head=TRUE, stringsAsFactors = FALSE)
} else {
  print(paste("Cannot find Prey_E_File: ",Prey_E_File))
  exit()
}
Day_Prey_E <- Prey_E[,1] # Days
prey_items   <- (ncol(Diet_prop))-1  # number of prey items in Diet_prop file
prey_items_E <- (ncol(Prey_E))-1     # number of prey items in Prey_E file
if(prey_items != prey_items_E) stop("Must have same number of prey items in file Prey_E.csv as in file Diet_prop.csv")
last_day_prey <- tail(Day_prey, n = 1)  # get the total number of days
last_day_prey_E <- tail(Day_Prey_E, n = 1)  # get the total number of days

globalout_Prey <- NULL
globalout_Prey_E <- NULL

for(i in 1:prey_items){
  Prey <- Diet_prop[,i+1]
  Prey <- approx(Day_prey,Prey, n = last_day_prey,method="linear")$y # interpolate prey 1 energy density
  Prey <- Prey[First_day:Last_day]
  globalout_Prey <- cbind(globalout_Prey,Prey)  # Proportion of each prey type in the diet
  Diet_prop <- read.csv(Diet_prop_File,head=TRUE, stringsAsFactors = FALSE)
  Prey_E <- Prey_E[,i+1]
  Prey_E <- approx(Day_Prey_E,Prey_E, n = last_day_prey_E,method="linear")$y # interpolate prey 1 energy density
  Prey_E <- Prey_E[First_day:Last_day]
  globalout_Prey_E <- cbind(globalout_Prey_E,Prey_E)
  Prey_E <- read.csv(Prey_E_File,head=TRUE, stringsAsFactors = FALSE)
}

colnames(globalout_Prey) <- names(Diet_prop)[-1]
colnames(globalout_Prey_E) <- names(Prey_E)[-1]
# end of diet proportions and energetical contributions section
