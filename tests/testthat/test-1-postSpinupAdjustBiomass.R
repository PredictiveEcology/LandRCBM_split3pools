if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("updateSpinupCohortGroups handles a DOM-only (zero biomass) row", {
  
  # Minimal spinupOut with 3 rows:
  # - row 1 & 2: forested cohorts in pixel 1 (different species)
  # - row 3: a DOM-only cohort in pixel 2, with zero live above-ground pools
  pools <- data.table::data.table(
    Merch       = c(10, 0, 0),
    Foliage     = c(2,  0, 0),
    Other       = c(1,  0, 0),
    CoarseRoots = c(3,  0, 0),
    FineRoots   = c(1,  0, 0)
  )
  state <- data.table::data.table(
    spatial_unit_id = c(1, 1, 2),
    age             = c(50, 0, 0),
    speciesCode     = c("Abie_las", "Pinu_con", "Abie_las"),
    sw_hw           = c(0, 0, 0)
  )
  spinupOut <- list(
    pools      = pools,
    flux       = data.table::data.table(dummy = c(1, 2, 3)),
    parameters = data.table::data.table(dummy = c(1, 2, 3)),
    state      = state
  )
  key <- data.table::data.table(
    cohortID   = 1:3,
    pixelIndex = c(1, 1, 2),
    row_idx    = 1:3
  )
  
  result <- updateSpinupCohortGroups(spinupOut, key)
  
  expect_type(result, "list")
  expect_setequal(names(result), c("pools", "flux", "parameters", "state", "key"))
  
  # Each pixel should map to exactly one group for its DOM-only cohort,
  # i.e. the key for pixel 2 points to a group with zero live pools.
  domRowIdx <- result$key[pixelIndex == 2, row_idx]
  expect_length(unique(domRowIdx), 1)
  expect_equal(
    as.numeric(result$pools[domRowIdx, c("Merch", "Foliage", "Other")]),
    c(0, 0, 0)
  )
  
  # The forested cohorts in pixel 1 keep distinct, non-zero-biomass groups.
  forestedRowIdx <- result$key[pixelIndex == 1, row_idx]
  expect_length(unique(forestedRowIdx), 2)
})

test_that("postSpinupAdjustBiomass assigns zero live biomass to unforested (DOM-only) pixels", {
  
  ## SIMULATE ----
  # Build on the same fixtures as the CBM_core spinup integration test, but
  # make one pixel unforested at spinup: it has a yield-table entry
  # (yieldTableIndex != 0, so generateCohortDT() still creates a borrowed
  # spinup cohort for DOM estimation) but no LandR cohortData entry, i.e.
  # pixelGroupMap == 0 for that pixel.
  pixelGroupMap <- terra::rast(file.path(spadesTestPaths$testdata, "LandR", "pixelGroupMap.tif"))
  cohortData    <- data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "cohortData.csv"))
  
  pixelGroupKey <- terra::values(pixelGroupMap)[,1]
  cohortData[, pixelGroup := (1:length(pixelGroupKey))[match(cohortData$pixelGroup, pixelGroupKey)]]
  data.table::setorder(cohortData, pixelGroup, speciesCode, age)
  
  pixelGroupMap <- terra::rast(pixelGroupMap, vals = 1:terra::ncell(pixelGroupMap))
  terra::set.values(pixelGroupMap, NA, cells = setdiff(1:terra::ncell(pixelGroupMap), cohortData$pixelGroup))
  
  # Pick an existing forested pixel and make it unforested: remove its
  # cohortData rows and set its pixelGroupMap value to 0 (pixelGroup 0 means
  # "unforested" in LandR, as distinct from NA which means "outside the
  # study area" and is dropped entirely by generateCohortDT()/splitCohortData()).
  unforestedPixel <- cohortData$pixelGroup[1]
  cohortData <- cohortData[pixelGroup != unforestedPixel]
  terra::set.values(pixelGroupMap, 0L, cells = unforestedPixel)
  
  yieldTablesId <- data.table::fread(file.path(spadesTestPaths$testdata, "LandR", "yieldTablesId.csv"))
  # Confirm this pixel keeps a nonzero yieldTableIndex, i.e. CBM still
  # estimates DOM for it from a neighbouring yield curve.
  expect_false(yieldTablesId[pixelIndex == unforestedPixel, yieldTableIndex] == 0)
  
  dir.create(file.path(spadesTestPaths$temp$inputs, "unforested-pixel"), showWarnings = FALSE)
  terra::writeRaster(pixelGroupMap, file.path(spadesTestPaths$temp$inputs, "unforested-pixel", "pixelGroupMap.tif"), overwrite = TRUE)
  data.table::fwrite(cohortData, file.path(spadesTestPaths$temp$inputs, "unforested-pixel", "cohortData.csv"))
  
  projectName <- "unit_postSpinupAdjustBiomass"
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
      tempdata    = file.path(spadesTestPaths$temp$inputs, "unforested-pixel")
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = "png"
      ),
      CBM_core = list(
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra"),
    
    pixelGroupMap         = file.path(paths$tempdata, "pixelGroupMap.tif") |> terra::rast(),
    cohortData            = file.path(paths$tempdata, "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    standDT               = file.path(paths$testdata, "CBM", "standDT.csv") |> data.table::fread(),
    yieldTablesCumulative = file.path(paths$testdata, "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(paths$testdata, "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  ## Suppress warnings about test modules missing metadata
  simTestInit <- suppressWarnings(SpaDES.core::simInit2(simInitInput))
  expect_s4_class(simTestInit, "simList")
  
  # Run spades. Should not error on a landscape with an unforested pixel.
  simTest <- expect_no_error(SpaDES.core::spades(simTestInit, events = list(
    LandRCBM_split3pools = c("init", "splitInit", "postSpinupAdjustBiomass"),
    CBM_core             = c("init", "spinup")
  )))
  expect_s4_class(simTest, "simList")
  
  
  ## CHECK ----
  
  # Forested pixels: cbm_vars pools should still match aboveGroundBiomass
  # (guards against reintroducing positional misalignment).
  check_module_outputs(simTest, spinup = TRUE)
  
  # The unforested pixel should have no row in aboveGroundBiomass...
  expect_false(unforestedPixel %in% simTest$aboveGroundBiomass$pixelIndex)
  
  # ...but should still have a cohort (the borrowed DOM-only spinup cohort)
  # with zero live above-ground biomass in cbm_vars.
  unforestedRowIdx <- simTest$cbm_vars$key[pixelIndex == unforestedPixel, row_idx]
  expect_length(unique(unforestedRowIdx), 1)
  expect_equal(
    as.numeric(simTest$cbm_vars$pools[unforestedRowIdx, c("Merch", "Foliage", "Other")][1]),
    c(0, 0, 0)
  )
})
