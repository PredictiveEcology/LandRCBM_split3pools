
defineModule(sim, list(
  name = "test_growth",
  description = "Test module mimicking **Biomass_core** to simulate yearly growth in `cohortData`.",
  timeunit = "year",
  reqdPkgs = list("data.table"),
  inputObjects = bindrows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  )
))

doEvent.test_growth = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      
      sim <- scheduleEvent(sim, start(sim), "test_growth", "growth", eventPriority = 6.25)
    },
    
    growth = {
      
      # Every year:
      # - Increment cohort ages by 1 year
      # - Increase biomass for all cohorts by 100 g/m^2 (1 tonnes/ha)
      sim$cohortData[!is.na(pixelGroup), age := age + 1]
      sim$cohortData[!is.na(pixelGroup), B   := B + 100]
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_growth", "growth", eventPriority = 6.25)
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

