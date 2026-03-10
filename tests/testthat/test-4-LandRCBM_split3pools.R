if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("module runs as a standAlone when not dynamic", {

  ## Gets throught simInit
  projectName <- "LandRCBM_split3pools"
  
  simInitTest <- SpaDEStestMuffleOutput(
    simInit(
      modules = "LandRCBM_split3pools",
      times   = list(start = 2000, end = 2021),
      paths   = list(
        modulePath  = spadesTestPaths$modulePath,
        inputPath   = spadesTestPaths$inputPath,
        cachePath   = spadesTestPaths$cachePath,
        outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
      ),
      objects = list(
        cohortData            = file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv") |> data.table::fread(),
        pixelGroupMap         = file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif") |> terra::rast(),
        rasterToMatch         = file.path(spadesTestPaths$testdata, "rasterToMatch.tif") |> terra::rast(),
        standDT               = file.path(spadesTestPaths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
        studyArea             = file.path(spadesTestPaths$testdata, "studyArea.shp") |> sf::st_read(quiet = TRUE),
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
           events = list("LandRCBM_split3pools" = c("init", "splitInit"))
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
  
  # check gcIncrements
  expect_is(simTest$gcIncrements, "data.table")
  expect_named(simTest$gcIncrements, c("gcids", "yieldTableIndex", "age", "merch_inc", "foliage_inc", "other_inc"))
  expect_true(all(simTest$gcIncrements$gcids %in% simTest$gcMeta$gcids))
  expect_true(all(simTest$gcIncrements$yieldTableIndex %in% simTest$yieldTablesId$yieldTableIndex))
  
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

