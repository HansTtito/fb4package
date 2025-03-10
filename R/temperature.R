########################################################################
### Temperature
########################################################################

if(file.exists(Temperature_File)){
  Temperature <- read.csv(Temperature_File,stringsAsFactors = FALSE) #  Read daily Temp values from .csv file
} else {
  print(paste("Cannot find Temperature_File: ",Temperature_File))
  exit()
}
Day <- Temperature[,1] # Days
Temperature <- Temperature[,2]  # Just use the Temp values, which are in column 2
last_day <- tail(Day, n = 1)  # get the total number of days
Day_Temp <- approx(Day,Temperature, n = last_day, method="linear")$x
Day_Temp <- Day_Temp[First_day:Last_day]
Temperature <- approx(Day,Temperature, n = last_day, method="linear")$y # interpolate temperature data
Temperature <- Temperature[First_day:Last_day]
