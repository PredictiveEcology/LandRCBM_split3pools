if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("PostSpinupAdjustBiomass", {
  
  simData <- list(
    
    # 2 cohorts in the same CBM spinup pixel group
    ## replacement AGB in aboveGroundBiomass is different for each
    cohortGroup1 = list(
      aboveGroundBiomass = rbind(
        data.table::data.table(
          pixelIndex  = 1,
          speciesCode = "Abie_las",
          age         = 1,
          merch       = 2,
          foliage     = 2,
          other       = 2
        ),
        data.table::data.table(
          pixelIndex  = 2,
          speciesCode = "Abie_las",
          age         = 1,
          merch       = 3,
          foliage     = 3,
          other       = 3
        )
      ),
      key = data.table::data.table(
        cohortID   = 1:2,
        pixelIndex = 1:2,
        row_idx    = 1
      ),
      state = data.table::data.table(
        row_idx     = 1,
        speciesCode = "Abie_las",
        sw_hw       = 0,
        age         = 1
      ),
      pools = data.table::data.table(
        row_idx     = 1,
        Merch       = 1,
        Foliage     = 1,
        Other       = 1,
        CoarseRoots = 1,
        FineRoots   = 1
      ),
      flux = data.table::data.table(row_idx = 1),
      parameters = data.table::data.table(row_idx = 1)
    ),
    
    # DOM cohort
    ## Will not be found in aboveGroundBiomass
    ## pools will remain 0
    cohortGroup1 = list(
      
      ## Will not be found in aboveGroundBiomass
      aboveGroundBiomass = data.table::data.table(),
      key = data.table::data.table(
        cohortID   = 3,
        pixelIndex = 3,
        row_idx    = 2
      ),
      state = data.table::data.table(
        row_idx     = 2,
        speciesCode = "Abie_las",
        sw_hw       = 0,
        age         = 0
      ),
      pools = data.table::data.table(
        row_idx     = 2,
        Merch       = 0,
        Foliage     = 0,
        Other       = 0,
        CoarseRoots = 0,
        FineRoots   = 0
      ),
      flux = data.table::data.table(row_idx = 1),
      parameters = data.table::data.table(row_idx = 1)
    )
  )
  
  sim <- list()
  sim$aboveGroundBiomass <- data.table::rbindlist(lapply(simData, `[[`, "aboveGroundBiomass"))
  sim$cbm_vars <- list()
  for (tbl in c("key", "state", "pools", "flux", "parameters")){
    sim$cbm_vars[[tbl]] <- data.table::rbindlist(lapply(simData, `[[`, tbl))
  }
  
  # Run PostSpinupAdjustBiomass
  sim <- modEnv$PostSpinupAdjustBiomass(sim)
  
  # Expect 3 pixel groups
  expect_setequal(sim$cbm_vars$key$row_idx, 1:3)
  expect_all_equal(
    sapply(sim$cbm_vars[c("state", "pools", "flux", "parameters")], nrow),
    3)
  
  # Check pools have been set correctly
  expect_equal(
    sim$cbm_vars$pools[row_idx == sim$cbm_vars$key[pixelIndex == 1, row_idx], 
                       .(Merch, Foliage, Other, CoarseRoots, FineRoots)] |> as.numeric(),
    c(2, 2, 2, 0.834449, 0.497551),
    tolerance = 0.000001)
  
  expect_equal(
    sim$cbm_vars$pools[row_idx == sim$cbm_vars$key[pixelIndex == 2, row_idx], 
                       .(Merch, Foliage, Other, CoarseRoots, FineRoots)] |> as.numeric(),
    c(3, 3, 3, 1.2981061, 0.6998939),
    tolerance = 0.000001)
  
  expect_equal(
    sim$cbm_vars$pools[row_idx == sim$cbm_vars$key[pixelIndex == 3, row_idx], 
                       .(Merch, Foliage, Other, CoarseRoots, FineRoots)] |> as.numeric(),
    c(0, 0, 0, 0, 0),
    tolerance = 0.000001)
})

