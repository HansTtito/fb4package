########################################################################
### Reproduction
########################################################################

FB4.Param2 = append(FB4.Param2, "calc.spawn")
FB4.Value2 = append(FB4.Value2, as.logical(calc.spawn))


if(calc.spawn){
  if(file.exists(Reproduction_File)){
    Reproduction <- read.csv(Reproduction_File,head=TRUE, stringsAsFactors = FALSE)
  } else {
    print(paste("Cannot find Reproduction_File: ",Reproduction_File))
    exit()
  }
  FB4.Param2 = append(FB4.Param2, "Reproduction_File")
  FB4.Value2 = append(FB4.Value2, as.character(Reproduction_File))  # record file used
  Day <- Reproduction[,1] # Days
  Reproduction <- Reproduction[,2]  # Just use the Temp values, which are in column 2
  last_day <- tail(Day, n = 1)  # get the total number of days
  Dayz <- approx(Day,Reproduction, n = last_day, method="linear")$x
  Dayz <- Dayz[First_day:Last_day]
  Reproduction <- approx(Day,Reproduction, n = last_day, method="constant")$y # interpolate temperature data
  Reproduction <- Reproduction[First_day:Last_day]
} else {
  FB4.Param2 = append(FB4.Param2, "Reproduction_File")
  FB4.Value2 = append(FB4.Value2, "NA")  # file not used
}
