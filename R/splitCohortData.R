splitCohortData <- function(cohortData, pixelGroupMap, standDT, table6, table7, tableMerchantability,
                            sppEquiv = NULL){
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
  spatialDT[, newPixelGroup := .GRP, by = .(pixelGroup, juris_id, ecozone)]
  
  # Add spatial information to cohortData
  # note that the new pixelGroup column is the unique combination of pixelGroup and CBM spatial units
  spatialUnits <- unique(spatialDT, by = "newPixelGroup")[, !("pixelIndex")]
  allInfoCohortData <- merge(cohortData, spatialUnits, by = "pixelGroup", allow.cartesian = TRUE)
  allInfoCohortData[, pixelGroup := NULL]
  setnames(allInfoCohortData, old = "newPixelGroup", new = "pixelGroup")
  
  # Add CanFI species code
  allInfoCohortData$canfi_species <- CBMutils::sppMatch(
    allInfoCohortData$speciesCode, sppEquivalencies = sppEquiv,
    match = "LandR", return = "CanfiCode")$CanfiCode
  
  # Convert biomass units from g/m^2 to tonnes/ha: 1 g/m^2 = 0.01 tonnes/ha
  allInfoCohortData[, B := B/100]
  
  # Split above ground biomass of current year.---------------------------------
  cohortPools <- CBMutils::cumPoolsCreateAGB(allInfoAGBin = allInfoCohortData,
                                             table6 = table6,
                                             table7 = table7,
                                             tableMerchantability = tableMerchantability,
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

