test_that("module runs with small example", {
  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "3-LandRCBM_split3pools")
  dir.create(projectPath)
  withr::local_dir(projectPath)
  module <- "LandRCBM_split3pools"
  rtm <- rast(ext(c(-1614000, -1612000, 7654000, 7656000)), 
                resolution = c(250, 250),
                crs = "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
  rtm[] <- 1L
  studyArea <- vect(ext(c(-1614000, -1612000, 7654000, 7656000)), crs = crs(rtm))
  
  simInitInput <-  SpaDEStestMuffleOutput(
    SpaDES.project::setupProject(
      times = list(start = 2011, end = 2016),
      modules = module,
      paths   = list(
        projectPath = projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        inputPath   = spadesTestPaths$temp$inputs,
        outputPath = spadesTestPaths$temp$outputs
      ),
      rasterToMatch = rtm,
      studyArea = studyArea
    )
  )
  
  simTestInit <-  SpaDEStestMuffleOutput(
    SpaDES.core::simInit2(simInitInput)
  )
  browser()
  # is output a simList?
  expect_s4_class(simTestInit, "simList")
  
  simTest <-  SpaDEStestMuffleOutput(
    SpaDES.core::spades(simTestInit)
  )
  expect_s4_class(simTest, "simList")
  expect_equal(time(simTest)[[1]], 2016)

    # check allInfoCohortData
  expect_is(simTest$allInfoCohortData, "data.table")
  
  expect_named(
    simTest$allInfoCohortData, 
    c("pixelGroup", "poolsPixelGroup", "speciesCode", "ecoregionGroup", "age", "B", "totalBiomass", "canfi_species", "juris_id", "ecozone"),
    ignore.order = TRUE
    )
  expect_in(simTest$allInfoCohortData$pixelGroup, simTest$pixelGroupMap[])
  expect_in(simTest$allInfoCohortData$speciesCode, simTest$allInfoYieldTables$speciesCode)
  expect_in(simTest$allInfoCohortData$canfi_species, simTest$table6$canfi_spec)
  expect_true(all(simTest$allInfoCohortData$juris_id == "BC"))
  expect_true(all(simTest$allInfoCohortData$ecozone == 14))
  
  # check allInfoYieldTables
  expect_is(simTest$allInfoYieldTables, "data.table")
  expect_named(
    simTest$allInfoYieldTables, 
    c("yieldPixelGroup", "cohort_id", "speciesCode", "age", "B", "canfi_species", "juris_id", "ecozone", "poolsPixelGroup"),
    ignore.order = TRUE
  )
  expect_in(simTest$allInfoYieldTables$yieldPixelGroup, simTest$yieldPixelGroupMap[])
  expect_in(simTest$allInfoYieldTables$cohort_id, simTest$yieldTables$cohort_id)
  expect_in(simTest$allInfoYieldTables$canfi_species, simTest$table6$canfi_spec)
  expect_true(all(simTest$allInfoYieldTables$juris_id == "BC"))
  expect_true(all(simTest$allInfoYieldTables$ecozone == 14))
  
  # check annualIncrements
  expect_is(simTest$annualIncrements, "data.table")
  expect_named(
    simTest$annualIncrements, 
    c("incrementPixelGroup", "species", "age", "totMerch", "fol", "other"),
    ignore.order = TRUE
  )
  expect_in(simTest$annualIncrements$incrementPixelGroup, simTest$incrementPixelGroupMap[])
  expect_in(simTest$annualIncrements$species, simTest$cohortData$speciesCode)
  expect_true(all(colSums(simTest$annualIncrements[,c("totMerch", "fol", "other")]) == 0))
  
  # check cohortPools
  expect_is(simTest$cohortPools, "data.table")
  expect_named(
    simTest$cohortPools, 
    c("species", "age", "poolsPixelGroup", "totMerch", "fol", "other"),
    ignore.order = TRUE
  )
  setorder(simTest$cohortPools, poolsPixelGroup, species)
  setorder(simTest$allInfoCohortData, poolsPixelGroup, speciesCode)
  expect_equivalent(simTest$cohortPools[,c("species", "age", "poolsPixelGroup")], 
                    simTest$allInfoCohortData[,c("speciesCode", "age", "poolsPixelGroup")])
  expect_equivalent(rowSums(simTest$cohortPools[,c("totMerch", "fol", "other")]), simTest$allInfoCohortData$B/2)
  
  #check cumPools
  expect_is(simTest$cumPools, "data.table")
  expect_named(
    simTest$cumPools, 
    c("gcids", "species", "age", "yieldPixelGroup", "totMerch", "fol", "other", "incMerch", "incFol", "incOther"),
    ignore.order = TRUE
  )
  expect_in(simTest$cumPools$gcids, simTest$allInfoYieldTables$cohort_id)
  expect_in(simTest$cumPools$species, simTest$allInfoYieldTables$speciesCode)
  expect_in(simTest$cumPools$yieldPixelGroup, simTest$yieldPixelGroupMap[])
  
  # check incrementPixelGroupMap
  expect_is(simTest$incrementPixelGroupMap, "SpatRaster")
  
  # check summaryAGBPoolsLandscape
  expect_is(simTest$summaryAGBPoolsLandscape, "data.table")
  expect_named(
    simTest$summaryAGBPoolsLandscape, 
    c("year", "totMerch", "fol", "other"),
    ignore.order = TRUE
  )
  expect_equal(simTest$summaryAGBPoolsLandscape$year, c(2011:2016))
  expect_in(simTest$summaryAGBPoolsLandscape$totMerch, simTest$summaryAGBPoolsLandscape$totMerch[1])
  expect_in(simTest$summaryAGBPoolsLandscape$fol, simTest$summaryAGBPoolsLandscape$fol[1])
  expect_in(simTest$summaryAGBPoolsLandscape$other, simTest$summaryAGBPoolsLandscape$other[1])
  
  # check summaryAGBPoolsSpecies
  expect_is(simTest$summaryAGBPoolsSpecies, "data.table")
  expect_named(
    simTest$summaryAGBPoolsSpecies, 
    c("year", "species", "totMerch", "fol", "other"),
    ignore.order = TRUE
  )
  expect_setequal(simTest$summaryAGBPoolsSpecies$year, c(2011:2016))
  expect_setequal(simTest$summaryAGBPoolsSpecies$species, simTest$allInfoCohortData$speciesCode)
  expect_true(all(simTest$summaryAGBPoolsSpecies$totMerch > 0))
  expect_true(all(simTest$summaryAGBPoolsSpecies$fol > 0))
  expect_true(all(simTest$summaryAGBPoolsSpecies$other > 0))
  
  # check yieldIncrements
  expect_is(simTest$yieldIncrements, "data.table")
  expect_named(
    simTest$yieldIncrements, 
    c("gcids", "yieldPixelGroup", "age", "species", "incMerch", "incFol", "incOther"),
    ignore.order = TRUE
  )
  expect_equivalent(simTest$yieldIncrements[, c("gcids", "yieldPixelGroup", "age")], simTest$yieldTables[, c("cohort_id", "yieldPixelGroup", "age")])
  expect_equivalent(simTest$cumPools[2,c("totMerch", "fol", "other")], simTest$yieldIncrements[2,c("incMerch", "incFol", "incOther")])
  expect_equivalent(simTest$cumPools[74,c("totMerch", "fol", "other")], 
                    simTest$cumPools[73,c("totMerch", "fol", "other")] + simTest$yieldIncrements[74,c("incMerch", "incFol", "incOther")])
})