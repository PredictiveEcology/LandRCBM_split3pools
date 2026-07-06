if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration with CBM_core: spinup", {
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_1-spinup"
  times <- list(start = 2000, end = 2000)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@CBM4"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),
    params = list(
      CBM_core = list(
        .plot = FALSE,
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    cohortData            = file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    pixelGroupMap         = file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit, events = list(
    LandRCBM_split3pools = c("init", "splitInit", "postSpinupAdjustBiomass"),
    CBM_core             = c("init", "setStands", "spinup")
  ))
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest, spinup = TRUE)
  
})

test_that("Integration with CBM_core: step", {
  
  ## NOTE: this test runs for 2 years to test that the module prepares inputs 
  ## properly for CBM_core after the spinup and in a following simulation year.
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_2-step"
  times <- list(start = 2000, end = 2001)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "test_growth",
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@CBM4"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),
    params = list(
      CBM_core = list(
        .plot = FALSE,
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    cohortData            = file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    pixelGroupMap         = file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest)
  
  # gcMeta
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcMeta$sw, TRUE)
  
  # gcIncrements
  ## Check that the total increase in carbon for each cohort is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[!gcID %in% c(-1, 0), .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Expect that all input cohorts are still present
  inCohorts <- merge(
    data.table::data.table(
      pixelGroup = terra::values(terra::rast(file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif")))[,1]
    )[, pixelIndex := .I],
    data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv")),
    by = "pixelGroup")
  
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts))
  
  ## Check cohort ages
  simTest$cohortDT[simTest$gcMeta, speciesCode := speciesCode, on = "gcID"]
  expect_equal(
    simTest$cohortDT[order(pixelIndex, speciesCode)]$age - (end(simTest) - start(simTest) + 1),
    inCohorts[order(pixelIndex, speciesCode)]$age
  )
})

test_that("Integration with CBM_core: step with new cohorts", {
  
  ## NOTE: this test runs for 1 year to check that the cohort's initial biomass
  ## leads to carbon increments with the expected value.
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_4-step-new"
  times <- list(start = 2000, end = 2000)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "test_growth",
      "test_recruitment",
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@CBM4"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),
    params = list(
      CBM_core = list(
        .plot = FALSE,
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    cohortData            = file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    pixelGroupMap         = file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest)
  
  # gcMeta
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcMeta$sw, TRUE)
  
  # gcIncrements
  ## Check that the total increase in carbon for the new cohort is 1 tonnes/ha
  ## This is expected with an an addition of 2 tonnes/ha biomass in the first year
  gcID_new <- simTest$cohortDT[pixelIndex == 1 & age == 1, gcID]
  expect_equal(
    simTest$gcIncrements[gcID == gcID_new, sum(merch_inc, foliage_inc, other_inc)],
    1)
  
  ## Check that the total increase in carbon for the other cohorts is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[!gcID %in% c(-1, 0, gcID_new), .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Expect that 1 cohort has been added
  inCohorts <- data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 1]), 3)
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts) + 1)
  
})

test_that("Integration with CBM_core: step with DOM cohorts: mortality", {
  
  ## NOTE: this test runs for 2 years to allow DOM cohorts to register as DOM.
  ## During the year that the cohort dies, the cohort will have negative increments
  ## to move the existing biomass into DOM pools.
  ## TODO: maybe not required anymore.
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_3-step-DOM"
  times <- list(start = 2000, end = 2001)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "test_growth",
      "test_mortality",
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@CBM4"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),
    params = list(
      CBM_core = list(
        .plot = FALSE,
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    cohortData            = file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    pixelGroupMap         = file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest)
  
  # gcMeta
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcMeta$sw, TRUE)
  
  # gcIncrements
  ## Check that the total increase in carbon for each cohort is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[!gcID %in% c(-1, 0), .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  ## Expect that 3 cohorts are gone
  inCohorts <- data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT[gcID > 0]), nrow(inCohorts) - 3)
  
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 1]), 2) # 1 active, 1 DOM cohort
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 2]), 1) # 2 DOM cohorts should be merged
  
  # check that DOM cohorts are located in the correct pixels
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 1]), 2) # 1 active, 1 DOM cohort
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 2]), 1) # 2 DOM cohorts should be merged
  
})

test_that("Integration with CBM_core: step with DOM cohorts: disturbance", {
  
  ## NOTE: This test runs for 1 year to check that the disturbed cohorts
  ## Will have their disturbance type correctly registered in sim$cbm_vars
  ## and have been assigned increments == 0
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_3-step-DOM"
  times <- list(start = 2000, end = 2000)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "test_growth",
      "test_disturbance",
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@CBM4"
    ),
    times = times,
    paths = list(
      projectPath = spadesTestPaths$projectPath,
      modulePath  = spadesTestPaths$temp$modules,
      packagePath = spadesTestPaths$packagePath,
      inputPath   = spadesTestPaths$inputPath,
      cachePath   = spadesTestPaths$cachePath,
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata
    ),
    params = list(
      CBM_core = list(
        .plot = FALSE,
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    cohortData            = file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    pixelGroupMap         = file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # check output object structure
  check_module_outputs(simTest)
  
  # Check that disturbance registered in disturbanceMeta and disturbanceEvents
  expect_equal(simTest$disturbanceMeta[eventID == 2001, disturbance_type_name], "Wildfire")
  expect_equal(simTest$disturbanceEvents, data.table::data.table(eventID = 2001, pixelIndex = 3, year = 2000), check.attributes = FALSE)
  
  # Check that all cohorts in disturbed pixels have been merged
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 3]), 1)
  
  # Check that increments for disturbed cohorts are 0
  expect_equal(simTest$cohortDT[pixelIndex == 3, gcID], 0)
  expect_true(all(simTest$gcIncrements[gcID == 0, .(merch_inc, foliage_inc, other_inc)] == 0))
  
  # Check that the wildfire cleared all aboveground biomass
  expect_true(all(
    simTest$cohortDT[pixelIndex == 3, .(
      pools.SoftwoodMerch, pools.SoftwoodFoliage, pools.SoftwoodOther,
      pools.HardwoodMerch, pools.HardwoodFoliage, pools.HardwoodOther)] == 0
  ))
})


