
defineModule(sim, list(
  name = "test_recruitment",
  description = "Test module mimicking **Biomass_core** to simulate cohort recruitment in `cohortData`",
  timeunit = "year",
  reqdPkgs = list("data.table"),
  inputObjects = bindrows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "cohortData", objectClass = "data.table", desc = NA)
  )
))

doEvent.test_recruitment = function(sim, eventTime, eventType) {
  switch(
    eventType,
    
    init = {
      sim <- scheduleEvent(sim, start(sim), "test_recruitment", "recruitment", eventPriority = 6.5)
    },
    
    recruitment = {
      
      if (time(sim) == start(sim)){
      
        # Add a cohort to pixel 1
        sim$cohortData <- rbind(
          sim$cohortData,
          data.table::data.table(
            pixelGroup     = 1,
            speciesCode    = "Abie_las",
            ecoregionGroup = "1_210",
            age            = 1,
            B              = 200
          ),
          fill = TRUE)
      }
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_recruitment", "recruitment", eventPriority = 6.5)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

