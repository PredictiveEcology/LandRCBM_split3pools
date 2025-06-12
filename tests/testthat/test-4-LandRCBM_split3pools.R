if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("module runs as a standAlone when not dynamic", {

  ## Gets throught simInit
  simInitTest <- SpaDEStestMuffleOutput(
    simInit(
      times = list(start = 2000, end = 2000),
      modules = "LandRCBM_split3pools",
      paths = list(modulePath = spadesTestPaths$temp$modules,
                   inputPath = spadesTestPaths$temp$inputs,
                   outputPath = spadesTestPaths$temp$outputs,
                   cachePath = spadesTestPaths$temp$cache),
      objects = list(
        cohortData            = file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv") |> data.table::fread(),
        pixelGroupMap         = file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
        rasterToMatch         = file.path(spadesTestPaths$testdata, "rasterToMatch.tif") |> terra::rast(),
        standDT               = file.path(spadesTestPaths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
        studyArea             = file.path(spadesTestPaths$testdata, "studyArea.shp") |> sf::st_read(),
        yieldTablesCumulative = file.path(spadesTestPaths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
        yieldTablesId         = file.path(spadesTestPaths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
      )
    )
  )
  
  # Run tests
  expect_s4_class(simInitTest, "simList")
  
  ## Get through init event:
  #  splits the cohort data and the yield tables
  simTest <- SpaDEStestMuffleOutput(
    spades(simInitTest,
           events = list("LandRCBM_split3pools" = "init")
    )
  )
  
  ## Run tests
  expect_s4_class(simTest, "simList")
  
  # check abovegroundbiomass
  expect_is(simTest$aboveGroundBiomass, "data.table")
  expect_named(simTest$aboveGroundBiomass, c("pixelIndex", "speciesCode", "age", "merch", "foliage", "other"))
  # check that total biomass per species match cohortData
  expectedSpeciesB <- simTest$cohortData[, .(total_biomass = sum(B)/200), by = speciesCode]
  resultSpeciesB <- copy(simTest$aboveGroundBiomass)[, B := merch + foliage + other]
  resultSpeciesB <- resultSpeciesB[, .(total_biomass = sum(B)), by = speciesCode]
  expect_equal(expectedSpeciesB, resultSpeciesB)
  
  # check cohortDT
  expect_is(simTest$cohortDT, "data.table")
  expect_named(simTest$cohortDT, c("cohortID", "pixelIndex", "age", "gcids"))
  expect_true(all(simTest$cohortDT$age %in% simTest$cohortData$age))
  
  # check gcMeta
  expect_is(simTest$gcMeta, "data.table")
  expect_named(simTest$gcMeta, c("gcids", "species_id", "speciesCode", "sw_hw"))
  expect_true(all(simTest$cohortDT$gcids %in% simTest$gcMeta$gcids))
  
  # check growth_increments
  expect_is(simTest$growth_increments, "data.table")
  expect_named(simTest$growth_increments, c("gcids", "yieldTableIndex", "age", "merch_inc", "foliage_inc", "other_inc"))
  expect_true(all(simTest$growth_increments$gcids %in% simTest$gcMeta$gcids))
  expect_true(all(simTest$growth_increments$yieldTableIndex %in% simTest$yieldTablesId$yieldTableIndex))
  
  # check yieldTablesCumulative
  expect_is(simTest$yieldTablesCumulative, "data.table")
  expect_named(simTest$yieldTablesCumulative, c("speciesCode", "age", "yieldTableIndex", "merch", "foliage", "other"))
  preLandRCBM_ytc <- file.path(spadesTestPaths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread()
  expect_equal(nrow(simTest$yieldTablesCumulative), nrow(preLandRCBM_ytc))
  expect_true(all(simTest$yieldTablesCumulative$yieldTableIndex %in% simTest$yieldTablesId$yieldTableIndex))
  
  # check yieldTablesId
  expect_is(simTest$yieldTablesId, "data.table")
  expect_named(simTest$yieldTablesId, c("pixelIndex", "yieldTableIndex"))
  
  # check events
})

test_that("module runs with Biomass_core and CBM_core when dynamic", {

  # Set times
  times <- list(start = 2000, end = 2002)
  
  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "integration_LandRCBM")
  dir.create(projectPath)
  withr::local_dir(projectPath)
  
  ## Gets throught simInit
  simInitInput <- SpaDEStestMuffleOutput(
    
    SpaDES.project::setupProject(
      
      modules = c(
        "DominiqueCaron/CBM_core@run-with-LandR",
        "PredictiveEcology/Biomass_core@development",
        "LandRCBM_split3pools"
      ),
      times   = times,
      params = list(
        CBM_core = list(
          skipCohortGroupHandling = TRUE,
          skipPrepareCBMvars = TRUE
        )
      ),
      paths   = list(
        projectPath = projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(projectPath, "outputs")
      ),
      biomassMap = file.path(spadesTestPaths$testdata, "LandR", "biomassMap.tif") |> terra::rast(),
      cohortData            = file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
      ecoregion = file.path(spadesTestPaths$testdata, "LandR", "ecoregion.csv") |> data.table::fread(colClasses = list(factor = c("ecoregionGroup"))),
      ecoregionMap = file.path(spadesTestPaths$testdata, "LandR", "ecoregionMap.tif") |> terra::rast(),
      minRelativeB = file.path(spadesTestPaths$testdata, "LandR", "minRelativeB.csv") |> data.table::fread(stringsAsFactors = TRUE),
      pixelGroupMap         = file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
      rasterToMatch         = file.path(spadesTestPaths$testdata, "rasterToMatch.tif") |> terra::rast(),
      species = file.path(spadesTestPaths$testdata, "LandR", "species.csv") |> data.table::fread(colClasses = list(factor = c("Area", "postfireregen", "hardsoft", "speciesCode"))),
      speciesEcoregion = file.path(spadesTestPaths$testdata, "LandR", "speciesEcoregion.csv") |> data.table::fread(stringsAsFactors = TRUE),
      speciesLayers = file.path(spadesTestPaths$testdata, "LandR", "speciesLayers.tif") |> terra::rast(),
      standDT               = {
        standDT = file.path(spadesTestPaths$testdata, "CBM", "standDT.csv") |> data.table::fread()
        standDT$disturbance_type_id = 0L
        standDT
        },
      studyArea             = file.path(spadesTestPaths$testdata, "studyArea.shp") |> sf::st_read(),
      yieldTablesCumulative = file.path(spadesTestPaths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
      yieldTablesId         = file.path(spadesTestPaths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread(),
      pooldef               = file.path(spadesTestPaths$testdata, "CBM", "pooldef.txt") |> readLines(),
      spinupSQL             = file.path(spadesTestPaths$testdata, "CBM", "spinupSQL.csv") |> data.table::fread()
      
    )
  )
  
  # Run simInit
  simTestInit <- SpaDEStestMuffleOutput(
    SpaDES.core::simInit2(simInitInput)
  )
  
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDEStestMuffleOutput(
    SpaDES.core::spades(simTestInit)
  )
  
  # Run tests
  expect_s4_class(simTest, "simList")
  
  # check all outputs are there
  expect_true(all(
    c("aboveGroundBiomass", "cbm_vars", "cohortDT", "cohortGroupKeep", "cohortGroups", "growth_increments", 
      "gcMeta", "masterRaster", "spinupResult", "summaryAGB", "yieldTablesCumulative", "yieldTablesId") %in%
      names(simTest)
  ))
  
  # check aboveGroundBiomass
  expect_is(simTest$aboveGroundBiomass, "data.table")
  expect_named(simTest$aboveGroundBiomass,
               c("pixelIndex", "speciesCode", "age", "merch", "foliage", "other"),
               ignore.order = TRUE)
  # check that total biomass per species match cohortData
  expectedSpeciesB <- simTest$cohortData[, .(total_biomass = sum(B)/200), by = speciesCode]
  resultSpeciesB <- copy(simTest$aboveGroundBiomass)[, B := merch + foliage + other]
  resultSpeciesB <- resultSpeciesB[, .(total_biomass = sum(B)), by = speciesCode]
  expect_equal(expectedSpeciesB, resultSpeciesB)
  
  # check cbm_vars
  expect_is(simTest$cbm_vars, "list")
  expect_named(simTest$cbm_vars, c("pools", "flux", "parameters", "state"))
  expect_equal(nrow(simTest$cbm_vars$pools), nrow(simTest$aboveGroundBiomass))
  expect_equal(nrow(simTest$cbm_vars$pools), nrow(simTest$cohortGroups))
  expect_equal(simTest$cbm_vars$pools$Merch, simTest$aboveGroundBiomass$merch)
  expect_equal(simTest$cbm_vars$pools$Foliage, simTest$aboveGroundBiomass$foliage)
  expect_equal(simTest$cbm_vars$pools$Other, simTest$aboveGroundBiomass$other)
  expect_equal(nrow(simTest$cbm_vars$flux), nrow(simTest$cohortGroups))
  expect_equal(nrow(simTest$cbm_vars$parameters), nrow(simTest$cohortGroups))
  expect_equal(simTest$cbm_vars$parameters$merch_inc, simTest$growth_increments$merch_inc)
  expect_equal(simTest$cbm_vars$parameters$foliage_inc, simTest$growth_increments$foliage_inc)
  expect_equal(simTest$cbm_vars$parameters$other_inc, simTest$growth_increments$other_inc)
  expect_equal(nrow(simTest$cbm_vars$state), nrow(simTest$cohortGroups))
  expect_equal(simTest$cbm_vars$state$age, simTest$cohortGroups$age)
  
  # check cohortGroupKeep
  expect_is(simTest$cohortGroupKeep, "data.table")
  expect_named(simTest$cohortGroupKeep, c("pixelIndex", "cohortGroupPrev", "cohortID", "spinup", "2000", "2001", "cohortGroupID", "2002"), ignore.order = TRUE)
  
  # check cohortGroups
  expect_is(simTest$cohortGroups, "data.table")
  expect_named(simTest$cohortGroups, c("cohortGroupID", "spatial_unit_id", "age", "gcids"), ignore.order = TRUE)
  
  # check cohortDT
  expect_is(simTest$cohortDT, "data.table")
  expect_named(simTest$cohortDT,
               c("cohortID", "pixelIndex", "age", "gcids"),
               ignore.order = TRUE)
  expect_setequal(simTest$cohortDT$gcids, simTest$cohortDT$cohortID)
  
  # check growth_increments
  expect_is(simTest$growth_increments, "data.table")
  expect_named(simTest$growth_increments,
               c("gcids", "age", "merch_inc", "foliage_inc", "other_inc"),
               ignore.order = TRUE)
  expect_equal(nrow(simTest$growth_increments), nrow(simTest$cohortDT))
  expect_setequal(simTest$cohortDT$gcids, simTest$growth_increments$gcids)
  
  # check gcMeta
  expect_is(simTest$gcMeta, "data.table")
  expect_named(simTest$gcMeta, 
               c("gcids", "species_id", "speciesCode", "sw_hw"))
  expect_equal(nrow(simTest$gcMeta), nrow(simTest$growth_increments))
  expect_setequal(simTest$gcMeta$gcids, simTest$growth_increments$gcids)
  
  # check summaryAGB
  expect_is(simTest$summaryAGB, "data.table")
  expect_named(
    simTest$summaryAGB, 
    c("speciesCode", "merch", "foliage", "other", "year"),
    ignore.order = TRUE
  )
  expect_equal(simTest$summaryAGB$year, c(2000, 2000, 2001, 2001,  2002, 2002))
  
  ## Check event order
  completedEvents <- completed(simTest)
  eventsToCheck <- c(
    "spinup",
    "postSpinupAdjustBiomass",
    "mortalityAndGrowth",
    "annualIncrements",
    "annual_preprocessing",
    "prepareCBMvars",
    "annual_carbonDynamics",
    "summarizeAGBPools",
    "accumulateResults",
    "plotSummaries"
  )
  
  # check event 1st year
  expectedEventOrder <- c(
    "spinup",
    "postSpinupAdjustBiomass",
    "mortalityAndGrowth",
    "annualIncrements",
    "annual_preprocessing",
    "prepareCBMvars",
    "annual_carbonDynamics",
    "summarizeAGBPools"
  )
  realizedEventOrder <- completedEvents[eventTime == start(simTest) & eventType %in% eventsToCheck]
  expect_equal(expectedEventOrder, realizedEventOrder$eventType)
  
  # check events last year
  expectedEventOrder <- c(
    "mortalityAndGrowth",
    "annualIncrements",
    "annual_preprocessing",
    "prepareCBMvars",
    "annual_carbonDynamics",
    "summarizeAGBPools",
    "accumulateResults",
    "plotSummaries"
  )
  realizedEventOrder <- completedEvents[eventTime == end(simTest) & eventType %in% eventsToCheck]
  expect_equal(expectedEventOrder, realizedEventOrder$eventType)
  
})