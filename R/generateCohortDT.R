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
    # Merge yield table index into cohortDT
    cohortDT <- merge(cohortDT, yieldTablesId, by = "pixelIndex", all.x = TRUE)
    
    # Get the species combinations per yieldTableIndex from the original data
    species_combos <- unique(cohortDT[, .(yieldTableIndex, speciesCode, sw_hw, species_id = newCode)])
    
    # Find pixels in yieldTablesId that are not in original cohortDT
    missing_pixels <- setdiff(yieldTablesId[yieldTableIndex != 0, pixelIndex], unique(cohortDT$pixelIndex))
    
    if (length(missing_pixels) > 0) {
      # For each missing pixel, get its yieldTableIndex
      missing_info <- yieldTablesId[pixelIndex %in% missing_pixels]
      
      # Join with species combinations for that yieldTableIndex
      new_rows <- merge(missing_info, species_combos, by = "yieldTableIndex", allow.cartesian = TRUE)
      
      # Add age = 0
      new_rows[, age := 0L]
      
      # Keep same column order as final table
      setcolorder(new_rows, c("pixelIndex", "speciesCode", "species_id", "age", "yieldTableIndex", "sw_hw"))
      
      # Bind to cohortDT
      cohortDT <- rbindlist(list(cohortDT, new_rows), use.names = TRUE, fill = TRUE)
    }
    
    # Add the growth curve index: 1 per species x yield table
    cohortDT[, gcids := .GRP, by = .(yieldTableIndex, speciesCode)]
    
    # Keep final columns in desired order
    cohortDT <- cohortDT[, .(cohortID, pixelIndex, speciesCode, species_id, age, gcids, yieldTableIndex, sw_hw)]
  } else {
    
  setkey(cohortDT, cohortID)
  return(cohortDT)
}
