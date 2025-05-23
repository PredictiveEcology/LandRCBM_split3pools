generateCohortDT <- function(cohortData, pixelGroupMap, yieldTablesId){
  # get the pixelGroup for each pixelIndex
  cohortDT <- data.table(
    pixelGroup = as.integer(pixelGroupMap[])
  ) 
  cohortDT <- cohortDT[, pixelIndex := .I] |> na.omit()
  # add cohort information for each pixelIndex
  cohortDT <- merge(cohortDT,
                    cohortData[age > 0,.(speciesCode, age, pixelGroup)],
                    allow.cartesian = TRUE)

  # add the canfi code of species
  cohortDT <- addSpeciesCode(cohortDT, code = "CBM_speciesID")
  # add sw_hw information of species
  cohortDT <- addForestType(cohortDT)
  setorder(cohortDT, pixelIndex, speciesCode, age)
  # add the index for individual cohorts
  cohortDT[, cohortID := .I]
  if (!is.null(yieldTablesId)){
    # add the yield table index
    cohortDT <- merge(cohortDT, yieldTablesId, by = "pixelIndex")
    # add the growth curve index: 1 per species x yield table
    cohortDT[, gcids := .GRP, by = .(yieldTableIndex, speciesCode)]
    cohortDT <- cohortDT[,.(cohortID, pixelIndex, speciesCode, species_id = newCode, age, gcids, yieldTableIndex, sw_hw)]
  } else {
    # add the growth curve index: 1 per species x age x pixel index
    cohortDT[, gcids := .GRP, by = .(pixelIndex, speciesCode, age)]
    cohortDT <- cohortDT[,.(cohortID, pixelIndex, speciesCode, species_id = newCode, age, gcids, sw_hw)]
  }
  
  setkey(cohortDT, cohortID)
  return(cohortDT)
}
