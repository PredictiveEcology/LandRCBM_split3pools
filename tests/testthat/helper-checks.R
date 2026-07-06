
# Helper function: check structure of output objects
# @param spinup logical. Checks match expected state after the spinup.
check_module_outputs <- function(simTest, spinup = FALSE){
  
  # abovegroundbiomass
  expect_is(simTest$aboveGroundBiomass, "data.table")
  expect_in(c("pixelIndex", "speciesCode", "age", "merch", "foliage", "other"), 
            names(simTest$aboveGroundBiomass))
  
  ## check that total biomass per species match cohortData
  expectedSpeciesB <- simTest$cohortData[, .(total_biomass = sum(B)/200), by = speciesCode]
  resultSpeciesB <- copy(simTest$aboveGroundBiomass)[, B := merch + foliage + other]
  resultSpeciesB <- resultSpeciesB[, .(total_biomass = sum(B)), by = speciesCode]
  expect_equal(expectedSpeciesB[order(speciesCode)], resultSpeciesB[order(speciesCode)])
  
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
  
  # checks for "active" cohorts
  cohortDT <- data.table::copy(simTest$cohortDT)
  cohortDT[simTest$gcMeta, speciesCode := speciesCode, on = "gcID"]
  
  cohort_active <- cohortDT$gcID > 0
  
  expect_equal(sum(cohort_active), nrow(simTest$aboveGroundBiomass))
  
  expect_equal(
    cohortDT[cohort_active, .(
      pixelIndex,
      speciesCode, age,
      merch   = pools.SoftwoodMerch   + pools.HardwoodMerch,
      foliage = pools.SoftwoodFoliage + pools.HardwoodFoliage,
      other   = pools.SoftwoodOther   + pools.HardwoodOther
    )][order(pixelIndex, speciesCode, age)],
    simTest$aboveGroundBiomass[, .(pixelIndex, speciesCode, age, merch, foliage, other)][
      order(pixelIndex, speciesCode, age)],
    check.attributes = FALSE
  )
  
  expect_equal(
    simTest$cohortDT[cohort_active, .(
      merch   = sum(pools.SoftwoodMerch   + pools.HardwoodMerch), 
      foliage = sum(pools.SoftwoodFoliage + pools.HardwoodFoliage),
      other   = sum(pools.SoftwoodOther   + pools.HardwoodOther)
    ), by = pixelIndex],
    simTest$aboveGroundBiomass[, .(merch = sum(merch), foliage = sum(foliage), other = sum(other)), by = pixelIndex],
    tolerance = 0.01, # hmmm
    check.attributes = FALSE
  )
  
  # checks for DOM cohorts
  
  ## DOM cohort groups have 0 above ground biomass
  expect_true(
    all(round(
      simTest$cohortDT[!cohort_active, .(
        pools.SoftwoodMerch,   pools.HardwoodMerch,
        pools.SoftwoodFoliage, pools.HardwoodFoliage, 
        pools.SoftwoodOther,   pools.HardwoodOther)], 
      10^-12) == 0)
  )
  
  ## There can't be more than 1 DOM cohort groups per pixel
  expect_true(!any(duplicated(simTest$cohortDT[!cohort_active, pixelIndex])))
  
  # summaryAGB
  if (!spinup){
    
    expect_is(simTest$summaryAGB, "data.table")
    expect_in(
      c("speciesCode", "merch", "foliage", "other", "year"),
      names(simTest$summaryAGB))
    expect_equal(simTest$summaryAGB$year, do.call(c, lapply(start(simTest):end(simTest), rep, 2)))
  }
}

