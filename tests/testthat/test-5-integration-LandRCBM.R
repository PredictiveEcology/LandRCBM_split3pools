if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("module runs with Biomass_core and CBM_core when dynamic", {
  
  # Set up project
  projectName <- "integration_LandRCBM"
  times <- list(start = 2000, end = 2021)
  
  simInitInput <- SpaDES.project::setupProject(
    
    modules = c(
      "PredictiveEcology/CBM_core@development",
      "PredictiveEcology/Biomass_core@development",
      "LandRCBM_split3pools"
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
      .globals = list(
        dataYear = 2001, #will get kNN 2011 data, and NTEMS 2011 landcover
        sppEquivCol = 'LandR'
      ),
      CBM_core = list(
        .plot = FALSE,
        skipCohortGroupHandling = TRUE,
        skipPrepareCBMvars = TRUE
      ),
      Biomass_core = list(
        .plots = NA
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra", "sf"),
    
    studyArea             = file.path(paths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
    rasterToMatch         = file.path(paths$testdata, "rasterToMatch.tif") |> terra::rast(),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    biomassMap            = file.path(paths$testdata, "LandR", "biomassMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    pixelGroupMap         = file.path(paths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
    speciesLayers         = file.path(paths$testdata, "LandR", "speciesLayers.tif") |> terra::rast(),
    ecoregionMap          = file.path(paths$testdata, "LandR", "ecoregionMap.tif") |> terra::rast(),
    minRelativeB          = file.path(paths$testdata, "LandR", "minRelativeB.csv") |> data.table::fread(stringsAsFactors = TRUE),
    ecoregion             = file.path(paths$testdata, "LandR", "ecoregion.csv") |> data.table::fread(colClasses = list(factor = c("ecoregionGroup"))),
    species               = file.path(paths$testdata, "LandR", "species.csv") |> data.table::fread(colClasses = list(factor = c("Area", "postfireregen", "hardsoft", "speciesCode"))),
    speciesEcoregion      = file.path(paths$testdata, "LandR", "speciesEcoregion.csv") |> data.table::fread(stringsAsFactors = TRUE),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
  # check all outputs are there
  expect_true(all(
    c("aboveGroundBiomass", "cbm_vars", "cohortDT", "gcIncrements", "gcMeta", 
      "summaryAGB", "yieldTablesCumulative", "yieldTablesId") %in%
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
  
  # check cbm_vars key
  expect_is(simTest$cbm_vars$key, "data.table")
  expect_named(simTest$cbm_vars$key, c("pixelIndex", "row_idx_prev", "cohortID", "row_idx", "disturbance_type_id"), ignore.order = TRUE)
  
  # check cbm_vars
  expect_is(simTest$cbm_vars, "list")
  expect_named(simTest$cbm_vars, c("key", "pools", "flux", "parameters", "state"))
  NcohortGroups <- length(unique(simTest$cbm_vars$key$row_idx))
  expect_equal(nrow(simTest$cbm_vars$pools), NcohortGroups)
  expect_equal(simTest$cbm_vars$pools$Merch, simTest$aboveGroundBiomass$merch)
  expect_equal(simTest$cbm_vars$pools$Foliage, simTest$aboveGroundBiomass$foliage)
  expect_equal(simTest$cbm_vars$pools$Other, simTest$aboveGroundBiomass$other)
  expect_equal(nrow(simTest$cbm_vars$flux), NcohortGroups)
  expect_equal(nrow(simTest$cbm_vars$parameters), NcohortGroups)
  expect_equal(simTest$cbm_vars$parameters$merch_inc, simTest$gcIncrements$merch_inc)
  expect_equal(simTest$cbm_vars$parameters$foliage_inc, simTest$gcIncrements$foliage_inc)
  expect_equal(simTest$cbm_vars$parameters$other_inc, simTest$gcIncrements$other_inc)
  expect_equal(nrow(simTest$cbm_vars$state), NcohortGroups)
  
  # check species types
  expect_in(simTest$cbm_vars$state[speciesCode == "Abie_las", sw_hw], 0L) # SW
  expect_in(simTest$cbm_vars$state[speciesCode == "Pinu_con", sw_hw], 0L) # SW
  
  # spatial unit id is correct
  expect_true(all(simTest$cbm_vars$state$spatial_unit_id == 42))
  
  # checks for "active" cohorts
  ActiveCohortGroups <- simTest$cbm_vars$state[gcID != 0, row_idx]
  expect_equal(
    simTest$aboveGroundBiomass[,.(Merch = merch, Foliage = foliage, Other = other)],
    simTest$cbm_vars$pools[ActiveCohortGroups,.(Merch, Foliage, Other)]
  )
  expect_equal(
    simTest$aboveGroundBiomass$age,
    simTest$cbm_vars$state$age[ActiveCohortGroups]
  )
  
  # checks for DOM cohorts (there are none in this test)
  DOMCohortGroups  = simTest$cbm_vars$state[gcID == 0, row_idx]
  # DOM cohort groups have 0 above ground biomass
  expect_true(
    all(simTest$cbm_vars$pools[DOMCohortGroups, .(Merch, Foliage, Other)] == 0)
  )
  # There can't be more than 1 DOM cohort groups per pixel
  expect_equal(
    length(DOMCohortGroups),
    nrow(simTest$cbm_vars$key[row_idx %in% DOMCohortGroups])
  )
  
  # check cohortDT
  expect_is(simTest$cohortDT, "data.table")
  expect_named(simTest$cohortDT,
               c("cohortID", "pixelIndex", "age", "speciesCode", "sw", "gcID"),
               ignore.order = TRUE)
  expect_setequal(simTest$cohortDT$gcID, simTest$cohortDT$cohortID)
  
  # check gcIncrements
  expect_is(simTest$gcIncrements, "data.table")
  expect_named(simTest$gcIncrements,
               c("gcID", "age", "merch_inc", "foliage_inc", "other_inc"),
               ignore.order = TRUE)
  expect_equal(nrow(simTest$gcIncrements), nrow(simTest$cohortDT))
  expect_setequal(simTest$cohortDT$gcID, simTest$gcIncrements$gcID)
  
  # check gcMeta
  expect_is(simTest$gcMeta, "data.table")
  expect_named(simTest$gcMeta, 
               c("gcID", "speciesCode", "sw"))
  expect_equal(nrow(simTest$gcMeta), nrow(simTest$gcIncrements))
  expect_setequal(simTest$gcMeta$gcID, simTest$gcIncrements$gcID)
  
  # check summaryAGB
  expect_is(simTest$summaryAGB, "data.table")
  expect_named(
    simTest$summaryAGB, 
    c("speciesCode", "merch", "foliage", "other", "year"),
    ignore.order = TRUE
  )
  expect_equal(simTest$summaryAGB$year, do.call(c, lapply(times$start:times$end, rep, 2)))
  
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
    "plotSummaries"
  )
  realizedEventOrder <- completedEvents[eventTime == end(simTest) & eventType %in% eventsToCheck]
  expect_equal(expectedEventOrder, realizedEventOrder$eventType)
  
})
