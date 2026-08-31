
# Helper function: check structure of output objects
# @param spinup logical. Checks match expected state after the spinup.
check_module_outputs <- function(simTest, spinup = FALSE){
  
  # gcMeta
  if (spinup){
    expect_in(c("gcID", "admin_abbrev", "eco_id", "speciesCode", "canfi_species", "sw"),
              names(simTest$gcMeta))
    expect_in("yieldTableIndex", names(simTest$gcMeta))
    expect_true(all(simTest$gcMeta$yieldTableIndex %in% simTest$yieldTablesId$yieldTableIndex))
  }else{
    expect_in(c("gcID", "speciesCode", "sw"),
              names(simTest$gcMeta))
  }
  
  ## Disturbed and DOM cohorts will have no species and increments == 0
  expect_true(all(
    is.na(simTest$gcMeta[gcID %in% c(-1, 0), .(speciesCode)])
  ))
  
  # gcIncrements
  expect_is(simTest$gcIncrements, "data.table")
  expect_in(
    c("gcID", "age", "merch_inc", "foliage_inc", "other_inc"),
    names(simTest$gcIncrements))
  expect_setequal(simTest$gcIncrements$gcID, simTest$gcMeta$gcID)
  
  if (spinup){
    expect_equal(nrow(simTest$gcIncrements), nrow(simTest$yieldTablesCumulative))
  }else{
    expect_setequal(simTest$gcIncrements$gcID, simTest$gcMeta$gcID)
    expect_equal(nrow(simTest$gcIncrements), nrow(simTest$gcMeta))
  }
  
  ## Disturbed and DOM cohorts will have increments == 0
  expect_true(all(
    simTest$gcIncrements[gcID %in% c(-1, 0), .(merch_inc, foliage_inc, other_inc)] == 0
  ))
  
  # cohortDT
  expect_is(simTest$cohortDT, "data.table")
  expect_in(c("pixelIndex", "age", "gcID"),
            names(simTest$cohortDT))
  
  expect_true(all(simTest$cohortDT$gcID %in% simTest$gcMeta$gcID))
  
  if ("CBM_core" %in% modules(simTest)){
    
    cohortDT <- data.table::copy(simTest$cohortDT)
    cohortDT[simTest$gcMeta, speciesCode := speciesCode, on = "gcID"]
    cohortDT[, AGC := 
               pools.SoftwoodMerch + pools.SoftwoodFoliage + pools.SoftwoodOther + 
               pools.HardwoodMerch + pools.HardwoodFoliage + pools.HardwoodOther]
    
    # checks for "active" cohorts
    cohort_active <- cohortDT$gcID > 0
    
    ## check that total AG carbon matches cohortData total AG biomass
    AGB <- data.table::data.table(
      pixelIndex = terra::cells(simTest$pixelGroupMap)
    )
    AGB[, pixelGroup := terra::extract(simTest$pixelGroupMap, pixelIndex)]
    AGB <- merge(AGB, simTest$cohortData, by = "pixelGroup")
    
    expect_equal(sum(cohort_active), nrow(AGB))
    expect_equal(
      sum(cohortDT[cohort_active, AGC]),
      sum(AGB$B) / 200
    )
    
    ## check that cohort AG carbon matches cohortData cohort AG biomass
    cohortDT[AGB, B := B, on = c("pixelIndex", "speciesCode", "age")]
    expect_equal(
      cohortDT[cohort_active, AGC],
      cohortDT[cohort_active, B] / 200
    )
    
    # checks for DOM cohorts
    
    ## DOM cohort groups have 0 above ground biomass
    expect_equal(
      round(sum(cohortDT[!cohort_active, AGC]), 10^-12),
      0
    )
    
    ## There can't be more than 1 DOM cohort groups per pixel
    expect_true(!any(duplicated(simTest$cohortDT[!cohort_active, pixelIndex])))
  }
  
  # summaryAGB
  if (!spinup){
    
    expect_is(simTest$summaryAGB, "data.table")
    expect_in(
      c("speciesCode", "merch", "foliage", "other", "year"),
      names(simTest$summaryAGB))
    expect_equal(simTest$summaryAGB$year, do.call(c, lapply(start(simTest):end(simTest), rep, 2)))
  }
}

