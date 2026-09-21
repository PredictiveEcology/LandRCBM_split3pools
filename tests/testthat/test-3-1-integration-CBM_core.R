if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# Set the `pixelGroup` key to be equal to the `pixelIndex` for simplification.
{
  pixelGroupMap <- terra::rast(file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif"))
  cohortData    <- data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv"))
  
  pixelGroupKey <- terra::values(pixelGroupMap)[,1]
  cohortData[, pixelGroup := (1:length(pixelGroupKey))[match(cohortData$pixelGroup, pixelGroupKey)]]
  data.table::setorder(cohortData, pixelGroup, speciesCode, age)
  
  pixelGroupMap <- terra::rast(pixelGroupMap, vals = 1:terra::ncell(pixelGroupMap))
  terra::set.values(pixelGroupMap, NA, cells = setdiff(1:terra::ncell(pixelGroupMap), cohortData$pixelGroup))
  
  dir.create(file.path(spadesTestPaths$temp$inputs, "intg-CBM_core"), showWarnings = FALSE)
  terra::writeRaster(pixelGroupMap, file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "pixelGroupMap.tif"), overwrite = TRUE)
  data.table::fwrite(cohortData, file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "cohortData.csv"))
  rm(pixelGroupMap)
  rm(cohortData)
}

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
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
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
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
    
    # Increase biomass for all cohorts by 100 g/m^2 (1 tonnes/ha)
    cohortGrowth = 100
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
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcIncrements$sw, TRUE)
  ## Check that the total increase in carbon for each cohort is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[gcID != 0, .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Expect that all input cohorts are still present
  inCohorts <- data.table::fread(file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts))
  
  ## Check cohort ages
  simTest$cohortDT[simTest$gcIncrements, speciesCode := speciesCode, on = "gcID"]
  expect_equal(
    simTest$cohortDT[order(pixelIndex, speciesCode)]$age - (end(simTest) - start(simTest) + 1),
    inCohorts[order(pixelGroup, speciesCode)]$age
  )
})

test_that("Integration with CBM_core: step with new cohorts", {
  
  ## NOTE: this test runs for 1 year to check that the cohort's initial biomass
  ## leads to carbon increments with the expected value.
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_3-step-new"
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
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
    
    # Increase biomass for all cohorts by 100 g/m^2 (1 tonnes/ha)
    cohortGrowth = 100,
    
    # Add a cohort to pixel 1 in year 2000
    cohortRecruit = data.table::data.table(
      year           = 2000,
      pixelGroup     = 1,
      speciesCode    = "Abie_las",
      ecoregionGroup = "1_210",
      age            = 1,
      B              = 200
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
  
  # gcIncrements
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcIncrements$sw, TRUE)
  ## Check that the new cohort is the correct species
  gcID_new <- simTest$cohortDT[pixelIndex == 1 & age == 1, gcID]
  expect_equal(as.character(simTest$gcIncrements[gcID == gcID_new, speciesCode]), "Abie_las")
  ## Check that the total increase in carbon for the new cohort is 1 tonnes/ha
  ## This is expected with an an addition of 2 tonnes/ha biomass in the first year
  expect_equal(
    simTest$gcIncrements[gcID == gcID_new, sum(merch_inc, foliage_inc, other_inc)],
    1)
  
  ## Check that the total increase in carbon for the other cohorts is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[!gcID %in% c(0, gcID_new), .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  # cohortDT
  ## Expect that 1 cohort has been added to pixel 3
  inCohorts <- data.table::fread(file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 1]), 3)
  expect_equal(nrow(simTest$cohortDT), nrow(inCohorts) + 1)
  
})

test_that("Integration with CBM_core: step with DOM cohorts: mortality", {
  
  ## NOTE: this test runs for 3 years to allow all cohorts experiencing mortality 
  ## to register as DOM. During the year that the cohort dies, the cohort will 
  ## have negative increments to move the existing biomass into DOM pools.
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_4-step-DOM"
  times <- list(start = 2000, end = 2002)
  
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
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),    
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
    
    # Increase biomass for all cohorts by 100 g/m^2 (1 tonnes/ha)
    cohortGrowth = 100,
    
    cohortMortality = rbind(
      
      # Year 2000: remove Abie_las from pixel 1
      data.table::data.table(year = 2000, pixelGroup = 1, speciesCode = "Abie_las"),
      
      # Year 2000: remove all cohorts from pixel 2
      data.table::data.table(year = 2000, pixelGroup = 2),
      
      # Year 2000: remove Abie_las from pixel 3
      # Year 2001: remove Pinu_con from pixel 3
      data.table::data.table(year = 2000, pixelGroup = 3, speciesCode = "Abie_las"),
      data.table::data.table(year = 2001, pixelGroup = 3, speciesCode = "Pinu_con"),
      
      fill = TRUE)
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
  ## Check that all cohorts are set as softwood
  expect_in(simTest$gcIncrements$sw, TRUE)
  ## Check that the total increase in carbon for each cohort is 0.5 tonnes/ha
  ## This is expected with an increase of biomass of 1 tonnes/ha for each cohort per year
  expect_true(all(
    round(simTest$gcIncrements[gcID != 0, .(inc = merch_inc + foliage_inc + other_inc)]$inc, 6) == 0.5
  ))
  
  ## Expect that 5 cohorts are gone
  inCohorts <- data.table::fread(file.path(spadesTestPaths$temp$inputs, "intg-CBM_core", "cohortData.csv"))
  expect_equal(nrow(simTest$cohortDT[gcID != 0]), nrow(inCohorts) - 5)
  
  # check that DOM cohorts are located in the correct pixels
  ## Pixels 2 and 3 should have 1 merged DOM cohort
  expect_equal(nrow(simTest$cohortDT[gcID == 0]), 3)
  
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 1]), 2) # 1 active, 1 DOM cohort
  expect_true(0 %in% simTest$cohortDT[pixelIndex == 1, gcID])
  expect_false(simTest$gcIncrements[gcID == simTest$cohortDT[pixelIndex == 1 & gcID != 0, gcID], speciesCode] == "Abie_las")
  
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 2]), 1) # 2 DOM cohorts should be merged
  expect_equal(simTest$cohortDT[pixelIndex == 2, gcID], 0)
  
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 3]), 1) # 2 DOM cohorts should be merged
  expect_equal(simTest$cohortDT[pixelIndex == 3, gcID], 0)
  
})

test_that("Integration with CBM_core: step with DOM cohorts: disturbance", {
  
  ## NOTE: This test runs for 1 year to check that the disturbed cohorts
  ## Will have their disturbance type correctly registered in disturbanceEvents
  ## and have been assigned increments == 0
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_5-step-dist"
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
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        fixedCohorts = FALSE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),    
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    
    masterRaster          = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
    
    # Increase biomass for all cohorts by 100 g/m^2 (1 tonnes/ha)
    cohortGrowth = 100,
    
    # Wildfire in pixel 3
    treedFirePixelTableSinceLastDisp = data.table::data.table(
      burnTime   = 2000,
      pixelIndex = 3
    ),
    
    # Partial harvest in pixel 2
    partialHarvestEvents = data.table::data.table(
      year        = 2000,
      pixelIndex  = 2,
      speciesCode = "Abie_las",
      age         = 150
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
  
  # Check that disturbances registered in disturbanceEvents
  expect_equal(
    simTest$disturbanceEvents[pixelIndex == 3, .(year, pixelIndex, gcID, disturbance_type_name)], 
    data.table::data.table(year = 2000, pixelIndex = 3, NA_integer_, disturbance_type_name = "Wildfire"), 
    check.attributes = FALSE)
  expect_equal(
    simTest$disturbanceEvents[pixelIndex == 2, .(year, pixelIndex, gcID, disturbance_type_name)], 
    data.table::data.table(year = 2000, pixelIndex = 2, 0L, disturbance_type_name = "Clearcut harvesting without salvage"), 
    check.attributes = FALSE)
  
  # Check that increments for disturbed cohorts are 0
  simTest$cohortDT[simTest$gcIncrements, speciesCode := speciesCode, on = "gcID"]
  expect_equal(simTest$cohortDT[pixelIndex == 3, gcID], rep(0, 2))
  expect_setequal(simTest$cohortDT[pixelIndex == 2, speciesCode], c(NA_character_, "Pinu_con"))
  expect_equal(nrow(simTest$cohortDT[pixelIndex == 2 & gcID == 0]), 1)
  expect_equal(nrow(simTest$cohortDT[gcID == 0]), 3)
  
  # Check disturbed cohort ages
  expect_equal(simTest$cohortDT[gcID == 0, age], rep(1, 3))
  
  # Check disturbed cohort last disturbance type
  expect_equal(simTest$cohortDT[pixelIndex == 3, state.last_disturbance_type], rep(1, 2))
  expect_equal(simTest$cohortDT[pixelIndex == 2 & is.na(speciesCode), state.last_disturbance_type], 204)
  
  # Check that the disturbances cleared all aboveground biomass
  expect_true(all(
    simTest$cohortDT[pixelIndex == 3, .(
      pools.SoftwoodMerch, pools.SoftwoodFoliage, pools.SoftwoodOther,
      pools.HardwoodMerch, pools.HardwoodFoliage, pools.HardwoodOther)] == 0
  ))
  expect_true(all(
    simTest$cohortDT[pixelIndex == 2 & is.na(speciesCode), .(
      pools.SoftwoodMerch, pools.SoftwoodFoliage, pools.SoftwoodOther,
      pools.HardwoodMerch, pools.HardwoodFoliage, pools.HardwoodOther)] == 0
  ))
  expect_false(all(
    simTest$cohortDT[pixelIndex == 2 & speciesCode == "Pinu_con", .(
      pools.SoftwoodMerch, pools.SoftwoodFoliage, pools.SoftwoodOther,
      pools.HardwoodMerch, pools.HardwoodFoliage, pools.HardwoodOther)] == 0
  ))
})


