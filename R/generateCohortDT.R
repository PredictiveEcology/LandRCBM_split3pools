generateCohortDT <- function(cohortData, pixelGroupMap, yieldTablesId){
  cohortDT <- data.table(
    pixelGroup = as.integer(pixelGroupMap[])
  ) 
  cohortDT <- cohortDT[, pixelIndex := .I] |> na.omit()
  cohortDT <- merge(cohortDT,
                    cohortData[,.(speciesCode, age, pixelGroup)],
                    allow.cartesian = TRUE)
  cohortDT <- addSpeciesCode(cohortDT, code = "CBM_speciesID")
  cohortDT <- addForestType(cohortDT)
  setorder(cohortDT, pixelIndex, speciesCode, age)
  cohortDT[, cohortID := .I]
  if (!is.null(yieldTablesId)){
    cohortDT <- merge(cohortDT, yieldTablesId, by = "pixelIndex")
    cohortDT[, gcids := .GRP, by = .(yieldTableIndex, speciesCode)]
    cohortDT <- cohortDT[,.(cohortID, pixelIndex, speciesCode, species_id = newCode, age, gcids, yieldTableIndex, sw_hw)]
  } else {
    cohortDT[, gcids := .GRP, by = .(pixelIndex, speciesCode, age)]
    cohortDT <- cohortDT[,.(cohortID, pixelIndex, speciesCode, species_id = newCode, age, gcids, sw_hw)]
  }
  setkey(cohortDT, cohortID)
}