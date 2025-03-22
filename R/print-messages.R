########################################################################
### Function to Print Message if problem detected
########################################################################

prt.msg <- function(run, day, wt, waterT, p, outpt, msg.num, msg.loc) {
  # prt.msg(nR,i,W,Temperature[i],0)  # example of using this function
  if(msg.num == 1) {
    print("Error in calculating weight at end of the day.")
    print("Number inside sqrt is NaN: not a number. Fish lost too much weight.")
  }else if(msg.num == 2) {
    print("Error in calculating weight at end of the day.")
    print("Number inside sqrt is negative. Fish lost too much weight.")
  }else if(msg.num == 3) {
    if(outpt == "End") {print("While fitting the p-value, ")}
    print("Fish weight became negative at end of this day.")
  }else if(msg.num == 4) {
    if(outpt == "End") {print("While fitting the p-value, ")}
    print("Could not determine an adequate p-value to fit the desired value.")
  }
  # Create a data frame for the Console version of the Error message
  FB4.Err.Param <- c("Run","Day","W(g)","T(C)","p-value","Msg.number","Msg.location")
  FB4.Err.Value <- c(run, day, wt, waterT, p, msg.num, msg.loc)
  FB4.Err.Log <- as.data.frame(cbind(FB4.Err.Param, FB4.Err.Value), stringsAsFactors = FALSE)
  row.names(FB4.Err.Log) <- seq(1:nrow(FB4.Err.Log))  # Use numbers for row names
  print(FB4.Err.Log)  # print to the RStudio Console
  return
}  # end of prt.msg function
