
defineModule(sim, list(
  name = "test_growth",
  description = "Simple testing module to simulate yearly growth in `cohortData`",
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
      sim <- scheduleEvent(sim, start(sim), "test_growth", "cohortDataByPixel", eventPriority = 6)
      sim <- scheduleEvent(sim, start(sim), "test_growth", "growth", eventPriority = 6.5)
      sim <- scheduleEvent(sim, start(sim), "test_growth", "cohortDataByGroup", eventPriority = 6.75)
    },
    
    cohortDataByPixel = {
      
      # Associate cohorts with pixels
      pixelGroups <- data.table(
        pixelGroup = as.integer(terra::values(sim$pixelGroupMap)[,1])
      )[, pixelIndex := .I]
      
      sim$cohortData[pixelGroups, pixelIndex := pixelIndex, on = "pixelGroup"]
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_growth", "cohortDataByPixel", eventPriority = 6)
      
    },
    
    growth = {
      
      # Every year:
      # - Increment cohort ages by 1 year
      # - Increase biomass for all cohorts by 1 tonnes/ha
      sim$cohortData[!is.na(pixelGroup), age := age + 1]
      sim$cohortData[!is.na(pixelGroup), B   := B + 100]
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_growth", "growth", eventPriority = 6.5)
      
    },
    
    cohortDataByGroup = {
      
      # Recreate pixelGroupMap
      sim$cohortData[, cohortID := .GRP, by = c("ecoregionGroup", "speciesCode", "age", "B")]
      
      pixelGroups <- sim$cohortData[, .(
        cohortIDs = sapply(list(cohortID), function(x) paste(sort(unique(unlist(x))), collapse = ","))),
        by = "pixelIndex"][, pixelGroup := .GRP, by = "cohortIDs"]
      
      sim$cohortData[, pixelGroup := NULL]
      sim$cohortData <- merge(sim$cohortData, pixelGroups[, .(pixelIndex, pixelGroup)], by = "pixelIndex")
      sim$cohortData[, pixelIndex := NULL]
      sim$cohortData[, cohortID   := NULL]
      sim$cohortData <- unique(sim$cohortData)
      
      sim$pixelGroupMap <- terra::rast(sim$pixelGroupMap)
      terra::set.values(sim$pixelGroupMap, pixelGroups$pixelIndex, pixelGroups$pixelGroup)
      
      sim <- scheduleEvent(sim, time(sim) + 1, "test_growth", "cohortDataByGroup", eventPriority = 6.75)
      
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) return(invisible(sim))

