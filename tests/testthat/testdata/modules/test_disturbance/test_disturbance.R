
defineModule(sim, list(
  name = "test_disturbance",
  description = "Simple testing module to simulate cohort disturbance in `cohortData`",
  timeunit = "year",
  reqdPkgs = list("data.table"),
  inputObjects = bindrows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  )
))

doEvent.test_disturbance = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      
      # Wildfire in pixel 3
      sim$treedFirePixelTableSinceLastDisp <- data.table::data.table(
        burnTime   = 2000,
        pixelIndex = 3
      )
      
      sim <- scheduleEvent(sim, start(sim), "test_disturbance", "disturbance", eventPriority = 6.25)
    },
    
    disturbance = {
      
      # Remove burned cohorts
      sim$cohortData <- sim$cohortData[!pixelIndex %in% sim$treedFirePixelTableSinceLastDisp[burnTime == time(sim)]$pixelIndex]
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_disturbance", "disturbance", eventPriority = 6.25)
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

