
defineModule(sim, list(
  name = "test_disturbance",
  description = "Test module mimicking **Biomass_regeneration** to simulate cohort disturbance in `cohortData`",
  timeunit = "year",
  reqdPkgs = list("data.table"),
  inputObjects = bindrows(
    expectsInput(objectName = "cohortDT",   objectClass = "data.table", desc = NA, sourceURL = NA),
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
      sim <- scheduleEvent(sim, start(sim), "test_disturbance", "disturbance", eventPriority = 8.5)
    },
    
    disturbance = {
      
      # Remove disturbed cohorts from cohortData
      distCohorts <- sim$cohortDT[
        gcID == 0 & (pools.SoftwoodMerch + pools.SoftwoodFoliage + pools.SoftwoodOther + pools.HardwoodMerch + pools.HardwoodFoliage + pools.HardwoodOther) > 0]
      data.table::setnames(distCohorts, "pixelIndex", "pixelGroup")
      distCohorts[, age := age + 1]
      sim$cohortData <- sim$cohortData[!distCohorts, on = c("pixelGroup", "speciesCode", "age")]
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_disturbance", "disturbance", eventPriority = 8.5)
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

