splitCohortData <- function(cohortData, pixelGroupMap, standDT, table6, table7){
  # Prepare cohort data for biomass splitting-----------------------------------
  # Match pixel group with jurisdiction and CBM spatial units
  spatialDT <- data.table(
    pixelGroup = as.integer(pixelGroupMap[])
  ) 
  spatialDT <- spatialDT[, pixelIndex := .I]
  spatialDT <- merge(
    standDT,
    spatialDT) |> na.omit()
  # New pixel group for unique combination of pixelGroup and CBM spatial units
  spatialDT[, newPixelGroup := .GRP, by = .(pixelGroup, spatial_unit_id)]
  # Add spatial information to cohortData
  spatialUnits <- unique(spatialDT, by = "newPixelGroup")[, !("pixelIndex")]
  allInfoCohortData <- addSpatialUnits(
    cohortData = cohortData,
    spatialUnits = spatialUnits
  ) # note that the new pixelGroup column is the unique combination of pixelGroup and CBM spatial units
  
  # Get species information
  # Add the species code in canfi
  allInfoCohortData <- addSpeciesCode(
    cohortData = allInfoCohortData,
    code = "CanfiCode"
  )
  setnames(allInfoCohortData, old = "newCode", new = "canfi_species")
  # Convert biomass units from g/m^2 to tonnes/ha: 1 g/m^2 = 0.01 tonnes/ha
  allInfoCohortData[, B := B/100]
  
  # Split above ground biomass of current year.---------------------------------
  cohortPools <- CBMutils::cumPoolsCreateAGB(allInfoAGBin = allInfoCohortData,
                                             table6 = table6,
                                             table7 = table7,
                                             "pixelGroup")
  
  # Get pixel-level biomass data.-----------------------------------------------
  spatialDT[, pixelGroup := NULL]
  biomassCurrent  <- merge(spatialDT, # The pixel-pixelGroup reference
                           cohortPools, # Cohort Biomass data
                           by.x = "newPixelGroup", 
                           by.y = "pixelGroup", 
                           allow.cartesian = TRUE) # There are multiple cohorts per pixelGroup and multiple pixels per pixelGroup
  # Only keep needed columns
  biomassCurrent <- biomassCurrent[, .(pixelIndex, speciesCode, age, merch, foliage, other)]
  setorderv(biomassCurrent, c("pixelIndex", "speciesCode", "age"))
  return(biomassCurrent)
}