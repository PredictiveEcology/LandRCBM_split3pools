test_that("module runs with small example", {
  
  # Set project path
  projectPath <- file.path(spadesTestPaths$temp$projects, "3-LandRCBM_split3pools")
  dir.create(projectPath)
  withr::local_dir(projectPath)
  module <- "LandRCBM_split3pools"
  
  # prepare inputs
  rtm <- rast(ext(c(-1614000, -1612000, 7654000, 7656000)), 
                resolution = c(250, 250),
                crs = "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
  rtm[] <- 1L
  studyArea <- vect(ext(c(-1614000, -1612000, 7654000, 7656000)), crs = crs(rtm))
  yieldTablesId <- prepInputs(url = "https://drive.google.com/file/d/1OExYMhxDvTWuShlRoCMeJEDW2PgSHofW/view?usp=drive_link",
                              fun = "data.table::fread",
                              destinationPath = spadesTestPaths$temp$inputs,
                              filename2 = "yieldTablesId.csv",
                              overwrite = TRUE)
  yieldTablesId$pixelIndex[c(1:ncell(rtm))] <- c(1:ncell(rtm))
  yieldTablesId <- yieldTablesId[c(1:ncell(rtm)), ]
  
  yieldTablesCumulative <- prepInputs(url = "https://drive.google.com/file/d/1ePPc_a8u6K_Sefd_wVS3E9BiSqK9DOnO/view?usp=drive_link",
                              fun = "data.table::fread",
                              destinationPath = spadesTestPaths$temp$inputs,
                              filename2 = "yieldTablesCumulative.csv",
                              overwrite = TRUE)
  yieldTablesCumulative <- yieldTablesCumulative[yieldTablesCumulative$yieldTableIndex %in% yieldTablesId$yieldTableIndex, ]
  ecozones <- data.table(
    pixelIndex = c(1:ncell(rtm)),
    ecozone = 14
  )
  jurisdictions <- data.table(
    pixelIndex = c(1:ncell(rtm)),
    juris_id = "BC"
  )

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
      studyArea = studyArea,
      jurisdictions = jurisdictions,
      ecozones = ecozones,
      yieldTablesCumulative = yieldTablesCumulative,
      yieldTablesId = yieldTablesId
    )
  )
  
  simTestInit <-  SpaDEStestMuffleOutput(
    SpaDES.core::simInit2(simInitInput)
  )

  # is output a simList?
  expect_s4_class(simTestInit, "simList")
  
  simTest <-  SpaDEStestMuffleOutput(
    SpaDES.core::spades(simTestInit)
  )
  expect_s4_class(simTest, "simList")
  expect_equal(time(simTest)[[1]], 2016)

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