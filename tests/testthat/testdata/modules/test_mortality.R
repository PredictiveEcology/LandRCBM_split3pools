
defineModule(sim, list(
  name     = "test_mortality",
  desc     = "Simple testing module to simulate cohort mortality in `cohortData`",
  timeunit = "year",
  reqdPkgs = list("data.table"),
  inputObjects = bindrows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  )
))

doEvent.test_mortality = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      sim <- scheduleEvent(sim, start(sim), "test_mortality", "mortality", eventPriority = 6)
    },
    
    mortality = {
      
      # Remove Abie_las from pixel 1
      pixelGroupID <- terra::extract(sim$pixelGroupMap, 1)[1,1]
      sim$cohortData <- sim$cohortData[!(pixelGroup == pixelGroupID & speciesCode == "Abie_las")]
      
      # Remove all cohorts from pixel 2
      pixelGroupID <- terra::extract(sim$pixelGroupMap, 2)[1,1]
      sim$cohortData <- sim$cohortData[pixelGroup != pixelGroupID]
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

