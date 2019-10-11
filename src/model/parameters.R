set_parameters <- function(){
  # This function will store all necessary parameters in lists and saves the data under ouput/parameters.rds
  
  # Set parameters list -------------------------------------------------------------------------------------------------------------------------
  parameters <- list(
    color_palette = c("#173F5F","#3CAEA3","#ED553B","#F6D55C","#20639B","#E89498", "#086E75","#948BE5","#B9E83A")
  )
  
  # Save in rds file -----------------------------------------------------------------------------------------------------------------------
  saveDf(parameters,"parameters")
}
