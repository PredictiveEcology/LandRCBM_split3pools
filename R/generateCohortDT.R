generateCohortDT <- function(cohortData, pixelGroupMap, yieldTablesId){
  cohortDT <- data.table(
    pixelGroup = as.integer(pixelGroupMap[])
  ) 
  cohortDT <- cohortDT[, pixelIndex := .I] |> na.omit()
  cohortDT <- merge(cohortDT,
                    cohortData[,.(speciesCode, age, pixelGroup)],
                    allow.cartesian = TRUE)
  cohortDT <- addCanfiCode(cohortDT)
  setorder(cohortDT, pixelIndex, speciesCode, age)
  cohortDT[, cohortIndex := .I]
  if (!is.null(yieldTablesId)){
    cohortDT <- merge(cohortDT, yieldTablesId, by = "pixelIndex")
    cohortDT[, gcIndex := .GRP, by = .(yieldTableIndex, speciesCode)]
    cohortDT <- cohortDT[,.(cohortIndex, pixelIndex, speciesCode, canfiCode = canfi_species, age, gcIndex, yieldTableIndex)]
  } else {
    cohortDT[, gcIndex := .GRP, by = .(pixelIndex, speciesCode, age)]
    cohortDT <- cohortDT[,.(cohortIndex, pixelIndex, speciesCode, canfiCode = canfi_species, age, gcIndex)]
  }
  setkey(cohortDT, cohortIndex)
}