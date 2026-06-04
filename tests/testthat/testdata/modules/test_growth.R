
defineModule(sim, list(
  name     = "test_growth",
  desc     = "Simple testing module to simulate yearly growth in `cohortData`",
  timeunit = "year",
  reqdPkgs = list("data.table"),
  inputObjects = bindrows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  )
))

doEvent.test_growth = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      sim <- scheduleEvent(sim, start(sim), "test_growth", "growth", eventPriority = 6)
    },
    
    growth = {
      
      # Every year:
      # - Increment cohort ages by 1 year
      # - Increase biomass for all cohorts by 1 tonnes/ha
      sim$cohortData[, age := age + 1]
      sim$cohortData[, B   := B + 100]
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_growth", "growth", eventPriority = 1)
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

