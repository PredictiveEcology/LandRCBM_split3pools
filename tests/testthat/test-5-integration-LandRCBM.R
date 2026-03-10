if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("module runs with Biomass_core and CBM_core when dynamic", {
  
  # Set up project
  projectName <- "integration_LandRCBM"
  times <- list(start = 2000, end = 2002)
  
  simInitInput <- SpaDEStestMuffleOutput(
    
    SpaDES.project::setupProject(
      
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
        outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
      ),
      params = list(
        CBM_core = list(
          skipCohortGroupHandling = TRUE,
          skipPrepareCBMvars = TRUE
        ),
        Biomass_core = list(
          .plots = NA
        )
      ),
      biomassMap    = file.path(spadesTestPaths$testdata, "LandR", "biomassMap.tif") |> terra::rast(),
      cohortData    = file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
      ecoregion     = file.path(spadesTestPaths$testdata, "LandR", "ecoregion.csv") |> data.table::fread(colClasses = list(factor = c("ecoregionGroup"))),
      ecoregionMap  = file.path(spadesTestPaths$testdata, "LandR", "ecoregionMap.tif") |> terra::rast(),
      minRelativeB  = file.path(spadesTestPaths$testdata, "LandR", "minRelativeB.csv") |> data.table::fread(stringsAsFactors = TRUE),
      pixelGroupMap = file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
      rasterToMatch = file.path(spadesTestPaths$testdata, "rasterToMatch.tif") |> terra::rast(),
      masterRaster  = rasterToMatch,
      species       = file.path(spadesTestPaths$testdata, "LandR", "species.csv") |> data.table::fread(colClasses = list(factor = c("Area", "postfireregen", "hardsoft", "speciesCode"))),
      speciesEcoregion = file.path(spadesTestPaths$testdata, "LandR", "speciesEcoregion.csv") |> data.table::fread(stringsAsFactors = TRUE),
      speciesLayers = file.path(spadesTestPaths$testdata, "LandR", "speciesLayers.tif") |> terra::rast(),
      standDT       = {
        standDT = file.path(spadesTestPaths$testdata, "CBM", "standDT.csv") |> data.table::fread()
        standDT$disturbance_type_id = 0L
        standDT
      },
      studyArea             = file.path(spadesTestPaths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
      yieldTablesCumulative = file.path(spadesTestPaths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
      yieldTablesId         = file.path(spadesTestPaths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
      
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
    c("aboveGroundBiomass", "cbm_vars", "cohortDT", "gcIncrements", "gcMeta", 
      "masterRaster", "summaryAGB", "yieldTablesCumulative", "yieldTablesId") %in%
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
  
  # check cbm_vars key
  expect_is(simTest$cbm_vars$key, "data.table")
  expect_named(simTest$cbm_vars$key, c("pixelIndex", "row_idx_prev", "cohortID", "row_idx", "disturbance_type_id"), ignore.order = TRUE)
  
  # check cohortDT
  expect_is(simTest$cohortDT, "data.table")
  expect_named(simTest$cohortDT,
               c("cohortID", "pixelIndex", "age", "gcID"),
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
               c("gcID", "species_id", "speciesCode", "sw_hw"))
  expect_equal(nrow(simTest$gcMeta), nrow(simTest$gcIncrements))
  expect_setequal(simTest$gcMeta$gcID, simTest$gcIncrements$gcID)
  
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
    "plotSummaries"
  )
  realizedEventOrder <- completedEvents[eventTime == end(simTest) & eventType %in% eventsToCheck]
  expect_equal(expectedEventOrder, realizedEventOrder$eventType)
  
})

test_that("module runs with Biomass_core and CBM_core when dynamic: RIA-small", {

  # Set up project
  projectName <- "integration_LandRCBM_RIA-small"

  simInitInput <- SpaDEStestMuffleOutput(
    
    SpaDES.project::setupProject(
      
      modules = c(
        "PredictiveEcology/Biomass_core@development",
        "LandRCBM_split3pools",
        "PredictiveEcology/CBM_core@development"
      ),
      times   = list(start = 2000, end = 2021),
      paths   = list(
        projectPath = spadesTestPaths$projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
      ),
      require = c("terra", "reproducible"),
      
      # Parameters
      params = list(
        .globals = list(
          dataYear = 2001, #will get kNN 2011 data, and NTEMS 2011 landcover
          sppEquivCol = 'LandR'
        ),
        CBM_core = list(
          .plot = FALSE,
          skipCohortGroupHandling = TRUE,
          skipPrepareCBMvars = TRUE
        )),

      # Prepare input objects
      studyArea             = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "studyArea.shp")     |> sf::st_read(quiet = TRUE),
      rasterToMatch         = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "rasterToMatch.tif") |> terra::rast(),
      standDT               = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "standDT.qs2")       |> qs2::qs_read(),
      standDT               = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "standDT.qs2")       |> qs2::qs_read(),
      biomassMap            = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "biomassMap.tif")    |> terra::rast(),
      cohortData            = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "cohortData.qs2")    |> qs2::qs_read(),
      pixelGroupMap         = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "pixelGroupMap.tif") |> terra::rast(),
      speciesLayers         = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "speciesLayers.tif") |> terra::rast(),
      ecoregionMap          = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "ecoregionMap.tif")  |> terra::rast(),
      minRelativeB          = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "minRelativeB.qs2")  |> qs2::qs_read(),
      ecoregion             = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "ecoregion.qs2")     |> qs2::qs_read(),
      species               = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "species.qs2")       |> qs2::qs_read(),
      speciesEcoregion      = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "speciesEcoregion.qs2") |> qs2::qs_read(),
      yieldTablesCumulative = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "yieldTablesCumulative.qs2") |> qs2::qs_read(),
      yieldTablesId         = file.path(spadesTestPaths$testdata, "LandRCBM-RIA-small/input", "yieldTablesId.qs2") |> qs2::qs_read(),
      sppEquiv = {
        speciesInStudy <- LandR::speciesInStudyArea(studyArea, dPath = spadesTestPaths$inputPath)
        species <- LandR::equivalentName(speciesInStudy$speciesList, df = LandR::sppEquivalencies_CA, "LandR")
        sppEquiv <- LandR::sppEquivalencies_CA[LandR %in% species]
        sppEquiv <- sppEquiv[KNN != "" & LANDIS_traits != ""]
      }
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

  expect_s4_class(simTest, "simList")
  
  # species ID are correct
  expect_equal(head(simTest$cbm_vars$state$species, 5), c(31, 15, 31, 15, 31))
  
  # spatial unit id is correct
  expect_true(all(simTest$cbm_vars$state$spatial_unit_id == 42))

  # checks for "active" cohorts
  with(
    list(
      ActiveCohortGroups  = simTest$cbm_vars$state[gcID != 0, row_idx]
    ), {
      # "Active" cohorts should match the above ground biomass equal to LandR
      expect_equal(
        simTest$aboveGroundBiomass[,.(Merch = merch, Foliage = foliage, Other = other)],
        simTest$cbm_vars$pools[ActiveCohortGroups,.(Merch, Foliage, Other)]
      )
      expect_equal(
        simTest$aboveGroundBiomass$age,
        simTest$cbm_vars$state$age[ActiveCohortGroups]
      )
    }
  )
  # checks for DOM cohorts
  with(
    list(
      DOMCohortGroups  = simTest$cbm_vars$state[gcID == 0, row_idx]
    ), {
      # DOM cohort groups have 0 above ground biomass
      expect_true(
        all(simTest$cbm_vars$pools[DOMCohortGroups, .(Merch, Foliage, Other)] == 0)
      )
      # There can't be more than 1 DOM cohort groups per pixel
      expect_equal(
        length(DOMCohortGroups),
        nrow(simTest$cbm_vars$key[row_idx %in% DOMCohortGroups])
      )
    }
  )
})



