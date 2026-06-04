
defineModule(sim, list(
  name = "test_recruitment",
  description = "Simple testing module to simulate cohort recruitment in `cohortData`",
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
      sim <- scheduleEvent(sim, start(sim), "test_recruitment", "recruitment", eventPriority = 6)
    },
    
    recruitment = {
      
      # Add a cohort to pixel 1
      pixelGroupID <- terra::extract(sim$pixelGroupMap, 1)[1,1]
      sim$cohortData <- rbind(
        sim$cohortData,
        data.table::data.table(
          speciesCode    = "Abie_las",
          ecoregionGroup = "1_210",
          age            = 1,
          B              = 200,
          pixelGroup     = pixelGroupID,
          totalBiomass   = NA
        )
      )
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

