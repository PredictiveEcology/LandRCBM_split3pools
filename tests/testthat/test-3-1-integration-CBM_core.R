if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration with CBM_core: spinup", {
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_1-spinup"
  times <- list(start = 2000, end = 2000)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@development"
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
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    
    # Set the `cohortData$pixelGroup` key to be equal to the `pixelIndex` for simplification.
    pixelGroupMap         = terra::rast(rasterToMatch, vals = 1:terra::ncell(rasterToMatch)),
    cohortData            = {
      
      cohortData    <- file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE)
      pixelGroupMap <- file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast()
      
      pixelGroupKey <- terra::values(pixelGroupMap)[,1]
      cohortData[, pixelGroup := (1:length(pixelGroupKey))[match(cohortData$pixelGroup, pixelGroupKey)]]
      return(cohortData)
    },
    
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
    CBM_core             = c("init", "spinup")
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
      "PredictiveEcology/CBM_core@development"
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
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    
    # Set the `cohortData$pixelGroup` key to be equal to the `pixelIndex` for simplification.
    pixelGroupMap         = terra::rast(rasterToMatch, vals = 1:terra::ncell(rasterToMatch)),
    cohortData            = {
      
      cohortData    <- file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE)
      pixelGroupMap <- file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast()
      
      pixelGroupKey <- terra::values(pixelGroupMap)[,1]
      cohortData[, pixelGroup := (1:length(pixelGroupKey))[match(cohortData$pixelGroup, pixelGroupKey)]]
      return(cohortData)
    },
    
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
  
  # gcIncrements
  ## Check that the total increase in carbon for each cohort is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[, .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcMeta$sw, TRUE)
  
  ## Expect that all input cohorts are still present
  inCohorts <- merge(
    data.table::data.table(
      pixelGroup = terra::values(terra::rast(file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif")))[,1]
    )[, pixelIndex := .I],
    data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv")),
    by = "pixelGroup")
  
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts))
  
  ## Check cohort ages
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
      "PredictiveEcology/CBM_core@development"
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
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    
    # Set the `cohortData$pixelGroup` key to be equal to the `pixelIndex` for simplification.
    pixelGroupMap         = terra::rast(rasterToMatch, vals = 1:terra::ncell(rasterToMatch)),
    cohortData            = {
      
      cohortData    <- file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE)
      pixelGroupMap <- file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast()
      
      pixelGroupKey <- terra::values(pixelGroupMap)[,1]
      cohortData[, pixelGroup := (1:length(pixelGroupKey))[match(cohortData$pixelGroup, pixelGroupKey)]]
      return(cohortData)
    },
    
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
    round(simTest$gcIncrements[gcID != gcID_new, .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcMeta$sw, TRUE)
  
  ## Expect that 1 cohort has been added
  inCohorts <- data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 1]), 3)
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts) + 1)
  
  # check that new cohort is located in the correct pixel
  expect_equal(
    simTest$cbm_vars$key[row_idx == simTest$cbm_vars$state[gcID == gcID_new, row_idx], pixelIndex],
    1
  )
})

test_that("Integration with CBM_core: step with DOM cohorts: mortality", {
  
  ## NOTE: this test runs for 2 years to allow DOM cohorts to register as DOM.
  ## During the year that the cohort dies, the cohort will have negative increments
  ## to move the existing biomass into DOM pools.
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_3-step-DOM"
  times <- list(start = 2000, end = 2001)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "test_growth",
      "test_mortality",
      "LandRCBM_split3pools",
      "PredictiveEcology/CBM_core@development"
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
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    
    # Set the `cohortData$pixelGroup` key to be equal to the `pixelIndex` for simplification.
    pixelGroupMap         = terra::rast(rasterToMatch, vals = 1:terra::ncell(rasterToMatch)),
    cohortData            = {
      
      cohortData    <- file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE)
      pixelGroupMap <- file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast()
      
      pixelGroupKey <- terra::values(pixelGroupMap)[,1]
      cohortData[, pixelGroup := (1:length(pixelGroupKey))[match(cohortData$pixelGroup, pixelGroupKey)]]
      return(cohortData)
    },
    
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
  
  # gcIncrements
  ## Check that the total increase in carbon for each cohort is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[, .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcMeta$sw, TRUE)
  
  ## Expect that 3 cohorts are gone
  inCohorts <- data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 1]), 1)
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 2]), 0)
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts) - 3)
  
  # check that DOM cohorts are located in the correct pixels
  row_idx_DOM <- c(
    simTest$cbm_vars$key[pixelIndex == 1 & row_idx %in% simTest$cbm_vars$state[speciesCode == "Abie_las", row_idx], row_idx],
    simTest$cbm_vars$key[pixelIndex == 2, row_idx]
  )
  expect_equal(simTest$cbm_vars$state[row_idx_DOM, gcID], c(0, 0))
  
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
      "PredictiveEcology/CBM_core@development"
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
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),

    # Set the `cohortData$pixelGroup` key to be equal to the `pixelIndex` for simplification.
    pixelGroupMap         = terra::rast(rasterToMatch, vals = 1:terra::ncell(rasterToMatch)),
    cohortData            = {
      
      cohortData    <- file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE)
      pixelGroupMap <- file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast()
      
      pixelGroupKey <- terra::values(pixelGroupMap)[,1]
      cohortData[, pixelGroup := (1:length(pixelGroupKey))[match(cohortData$pixelGroup, pixelGroupKey)]]
      return(cohortData)
    },
    
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
    
    # Wildfire in pixel 3
    disturbanceMeta = data.table::data.table(
      eventID = 1,
      disturbance_type_id = 1
    ),
    disturbanceEvents = data.table::data.table(
      pixelIndex = 3,
      year       = 2000,
      eventID    = 1
    )
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
  
  # Check that disturbance_type_id registered in simTest$cbm_vars$key
  expect_equal(
    simTest$cbm_vars$key[pixelIndex == 3]$disturbance_type_id,
    rep(1, 2))
  
  # Check that disturbance_type_id registered in simTest$cbm_vars$parameters
  row_idx_dist <- simTest$cbm_vars$key[pixelIndex == 3, row_idx]
  expect_true(all(
    simTest$cbm_vars$parameters[row_idx %in% row_idx_dist, disturbance_type] == 1
  ))
  
  # Check that increments for disturbed cohorts are 0
  expect_true(all(
    simTest$cbm_vars$parameters[row_idx %in% row_idx_dist, .(merch_inc, foliage_inc, other_inc)] == 0
  ))
  
  # Check that the wildfire cleared all aboveground biomass
  expect_true(all(
    simTest$cbm_vars$pools[row_idx %in% row_idx_dist, .(Merch, Foliage, Other)] == 0
  ))
})


