generateCohortDT <- function(cohortData, pixelGroupMap, yieldTablesId){
  browser()
  cohortDT <- data.table(
    pixelGroup = as.integer(pixelGroupMap[])
  ) 
  cohortDT <- cohortDT[, pixelIndex := .I] |> na.omit()
  cohortDT <- merge(cohortDT,
                    cohortData[,.(speciesCode, age, pixelGroup)])
  cohortDT <- addCanfiCode(cohortDT)
  setorder(cohortDT, pixelIndex, speciesCode, age)
  cohortDT[, cohortIndex := .I]
  cohortDT <- merge(cohortDT, yieldTablesId, by = "pixelIndex")
  setkey(cohortDT, cohortIndex)
  cohortDT[, gcIndex := .GRP, by = .(gcid, speciesCode)]
  cohortDT <- cohortDT[,.(cohortIndex, pixelIndex, speciesCode, canfiCode = canfi_species, age, gcIndex, gcid)]
}