
defineModule(sim, list(
  name = "test_mortality",
  description = "Test module mimicking **Biomass_core** to simulate cohort mortality in `cohortData`",
  timeunit = "year",
  reqdPkgs = list("data.table"),
  inputObjects = bindrows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  )
))

doEvent.test_mortality = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      sim <- scheduleEvent(sim, start(sim), "test_mortality", "mortality", eventPriority = 6.5)
    },
    
    mortality = {
      
      if (time(sim) == start(sim)){
      
        # Remove Abie_las from pixel 1
        sim$cohortData <- sim$cohortData[!(pixelGroup == 1 & speciesCode == "Abie_las")]
        
        # Remove all cohorts from pixel 2
        sim$cohortData <- sim$cohortData[!(pixelGroup == 2)]
      }
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_mortality", "mortality", eventPriority = 6.5)
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

