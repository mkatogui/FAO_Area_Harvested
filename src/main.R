# List
scriptsToRun <- list.files(pattern = "FAO")

# Use lapply to run each script and then pause for 5 seconds
lapply(scriptsToRun, function(script) {
  # Measure the time taken to run the script
  time_taken <- system.time(source(script))
  
  # Pause for 5 seconds
  Sys.sleep(5)
  
  # Return the time taken
  return(time_taken)
})

# Make sure the shiny package is installed and loaded
if (!require(shiny)) {
  install.packages("shiny")
}

# Run the Shiny app
shiny::runApp()
