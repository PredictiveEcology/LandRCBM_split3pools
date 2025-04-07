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
  yieldTablesCumulative <- yieldTablesCumulative[yieldTablesCumulative$gcid %in% yieldTablesId$gcid, ]
  
  
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

    # check yieldTablesCumulative
  expect_is(simTest$yieldTablesCumulative, "data.table")
  
  expect_named(
    simTest$yieldTablesCumulative, 
    c("gcid", "speciesCode", "age", "merch", "foliage", "other"),
    ignore.order = TRUE
    )
  expect_equal(nrow(simTest$yieldTablesCumulative), nrow(yieldTablesCumulative))
  setorderv(yieldTablesCumulative, cols = c("gcid", "speciesCode"))
  simTest$yieldTablesCumulative$B <- (simTest$yieldTablesCumulative$merch + simTest$yieldTablesCumulative$foliage + simTest$yieldTablesCumulative$other)*200
  expect_true(all((
    yieldTablesCumulative$biomass[yieldTablesCumulative$age != 0] - 
      simTest$yieldTablesCumulative$B[simTest$yieldTablesCumulative$age != 0]) <= 0.001 ))
  
  # check yieldTablesId
  expect_is(simTest$yieldTablesId, "data.table")
  expect_named(simTest$yieldTablesId, c("pixelIndex", "gcid"), ignore.order = TRUE)
  expect_setequal(simTest$yieldTablesId$pixelIndex, yieldTablesId$pixelIndex)
  expect_equal(length(unique(simTest$yieldTablesId$gcid)), length(unique(yieldTablesId$gcid)))
  expect_setequal(simTest$yieldTablesId$gcid, simTest$yieldTablesCumulative$gcid)

  # check yieldTablesIncrements
  expect_is(simTest$yieldTablesIncrements, "data.table")
  expect_named(simTest$yieldTablesIncrements, c("gcid", "speciesCode", "age", "merchInc", "foliageInc", "otherInc"), ignore.order = TRUE)
  expect_equal(simTest$yieldTablesIncrements$merchInc[100] + simTest$yieldTablesCumulative$merch[99], simTest$yieldTablesCumulative$merch[100])
  expect_equal(simTest$yieldTablesIncrements$foliageInc[1000] + simTest$yieldTablesCumulative$foliage[999], simTest$yieldTablesCumulative$foliage[1000])
  expect_true(all(is.na(simTest$yieldTablesIncrements$otherInc[simTest$yieldTablesIncrements$age == 0])))
  
  
  # check aboveGroundBiomass
  expect_is(simTest$aboveGroundBiomass, "data.table")
  expect_named(simTest$aboveGroundBiomass,
               c("pixelIndex", "speciesCode", "age", "merch", "foliage", "other"),
               ignore.order = TRUE)
  
  # check aboveGroundIncrements
  expect_is(simTest$aboveGroundIncrements, "data.table")
  expect_named(
    simTest$aboveGroundIncrements, 
    c("pixelIndex", "speciesCode", "age", "merchInc", "foliageInc", "otherInc"),
    ignore.order = TRUE
  )
  expect_equal(sum(simTest$aboveGroundIncrements$merchInc), 0 ) # there is not growth/mortality in the test

  # check summaryAGB
  expect_is(simTest$summaryAGB, "data.table")
  expect_named(
    simTest$summaryAGB, 
    c("speciesCode", "merch", "foliage", "other", "year"),
    ignore.order = TRUE
  )
  expect_contains(simTest$summaryAGB$year, c(2011:2016))
  
  # check disturbanceEvents
  expect_is(simTest$disturbanceEvents, "data.table")
  expect_named(
    simTest$disturbanceEvents, 
    c("pixelIndex", "year", "eventID"),
    ignore.order = TRUE
  )
  expect_equal(nrow(simTest$disturbanceEvents), 0)
  
})