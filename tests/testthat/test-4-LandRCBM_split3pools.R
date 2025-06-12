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
})

test_that("module runs with Biomass_core and CBM_core when dynamic", {
  browser()
  
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
      paths   = list(
        projectPath = projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        packagePath = spadesTestPaths$packagePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(projectPath, "outputs")
      ),
      cohortData = data.table::fread(),
      ecoregion = data.table::fread(),
      ecoregionMAp = terra::rast(),
      minRelativeB = data.table::fread(),
      pixelGroupMap = terra::rast(),
      rasterToMatch = terra::rast(),
      species = data.table::fread(),
      speciesEcoregion = data.table::fread(),
      speciesLayers = terra::rast(),
      standDT = data.table::fread(),
      studyArea = terra::vect(),
      yieldTablesCumulative = data.table::fread(),
      yieldTablesId = data.table::fread(),
      pooldef = file.path(spadesTestPaths$testdata, "SK/input", "pooldef.txt") |> readLines(),
      spinupSQL = file.path(spadesTestPaths$testdata, "SK/input", "spinupSQL.csv") |> data.table::fread()
      
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
  
  # Run tests
  expect_s4_class(simTest, "simList")
  
  # check all outputs are there
  expect_true(all(
    c("aboveGroundBiomass", "cohortDT", "disturbanceEvents", "growth_increments", 
      "gcMeta", "standDT", "summaryAGB", "yieldTablesCumulative", "yieldTablesId") %in%
      names(simTest)
  ))
  
  # check aboveGroundBiomass
  expect_is(simTest$aboveGroundBiomass, "data.table")
  expect_named(simTest$aboveGroundBiomass,
               c("pixelIndex", "speciesCode", "age", "merch", "foliage", "other"),
               ignore.order = TRUE)
  
  # check cohortDT
  expect_is(simTest$cohortDT, "data.table")
  expect_named(simTest$cohortDT,
               c("cohortID", "pixelIndex", "age", "gcids"),
               ignore.order = TRUE)
  expect_setequal(simTest$cohortDT$gcids, simTest$cohortDT$cohortID)
  
  # check disturbanceEvents
  expect_is(simTest$disturbanceEvents, "data.table")
  expect_named(simTest$disturbanceEvents,
               c("pixelIndex", "year", "eventID"),
               ignore.order = TRUE)
  expect_equal(nrow(simTest$disturbanceEvents), 0)
  
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
  
  # check standDT
  expect_is(simTest$standDT, "data.table")
  expect_named(simTest$standDT, 
               c("pixelIndex", "area", "spatial_unit_id", "ecozone", "juris_id"))
  expect_setequal(simTest$standDT$pixelIndex, yieldTablesId$pixelIndex)
  
  # check summaryAGB
  expect_is(simTest$summaryAGB, "data.table")
  expect_named(
    simTest$summaryAGB, 
    c("speciesCode", "merch", "foliage", "other", "year"),
    ignore.order = TRUE
  )
  expect_contains(simTest$summaryAGB$year, c(2011:2016))
  
  # check yieldTablesCumulative
  expect_is(simTest$yieldTablesCumulative, "data.table")
  expect_named(
    simTest$yieldTablesCumulative, 
    c("yieldTableIndex", "speciesCode", "age", "merch", "foliage", "other"),
    ignore.order = TRUE
    )
  expect_equal(nrow(simTest$yieldTablesCumulative), nrow(yieldTablesCumulative))
  setorderv(yieldTablesCumulative, cols = c("yieldTableIndex", "speciesCode"))
  simTest$yieldTablesCumulative$B <- (simTest$yieldTablesCumulative$merch + simTest$yieldTablesCumulative$foliage + simTest$yieldTablesCumulative$other)*200
  expect_true(all((
    yieldTablesCumulative$biomass[yieldTablesCumulative$age != 0] - 
      simTest$yieldTablesCumulative$B[simTest$yieldTablesCumulative$age != 0]) <= 0.001 ))
  
  # check yieldTablesId
  expect_is(simTest$yieldTablesId, "data.table")
  expect_named(simTest$yieldTablesId, c("pixelIndex", "yieldTableIndex"), ignore.order = TRUE)
  expect_setequal(simTest$yieldTablesId$pixelIndex, yieldTablesId$pixelIndex)
  expect_equal(length(unique(simTest$yieldTablesId$yieldTableIndex)), length(unique(yieldTablesId$yieldTableIndex)))
  expect_setequal(simTest$yieldTablesId$yieldTableIndex, simTest$yieldTablesCumulative$yieldTableIndex)
  
  # test splitting the yield tables event only
  simTestInitEvent <-  SpaDEStestMuffleOutput(
    SpaDES.core::simInitAndSpades(modules = module,
                                  object = list(
                                    rasterToMatch = rtm,
                                    studyArea = studyArea,
                                    jurisdictions = jurisdictions,
                                    ecozones = ecozones,
                                    yieldTablesCumulative = yieldTablesCumulative,
                                    yieldTablesId = yieldTablesId
                                    ), 
                                  paths = list(
                                    cachePath = spadesTestPaths$temp$cache,
                                    modulePath  = spadesTestPaths$temp$modules,
                                    inputPath   = spadesTestPaths$temp$inputs,
                                    outputPath = spadesTestPaths$temp$outputs
                                  ),
                                  events = "init")
  )
  
  # check growth_increments
  expect_is(simTestInitEvent$growth_increments, "data.table")
  expect_named(simTestInitEvent$growth_increments,
               c("gcids", "yieldTableIndex", "age", "merch_inc", "foliage_inc", "other_inc"),
               ignore.order = TRUE)
  expect_in(simTestInitEvent$growth_increments$yieldTableIndex, simTestInitEvent$yieldTablesCumulative$yieldTableIndex)
  expect_in(simTestInitEvent$growth_increments$gcids, simTestInitEvent$gcMeta$gcids)
  
  # check cohortDT
  expect_is(simTestInitEvent$cohortDT, "data.table")
  expect_named(simTestInitEvent$cohortDT,
               c("cohortID", "pixelIndex", "age", "gcids"),
               ignore.order = TRUE)

})