
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration with CBM_core: allow unforested pixels", {
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_unforested-pixel"
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
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName)
    ),
    params = list(
      .globals = list(
        .useCache = FALSE,
        .plots    = NA
      ),
      CBM_core = list(
        skipPrepareCBMvars = TRUE
      )
    ),
    
    # Prepare input objects
    require = c("data.table", "terra"),
    
    # Pixel group 0 in LandR means "unforested"; NA means outside of study area
    # cohortData will not contain any rows for this pixel group
    # yieldTablesId will have rows for these pixels
    pixelGroupMap = terra::rast(matrix(c(10, 20, 0, NA), nrow = 2, ncol = 2, byrow = TRUE)),
    standDT = data.table(
      pixelIndex   = 1:4,
      area         = 1,
      admin_name   = "British Columbia",
      admin_abbrev = "BC",
      eco_id       = 4
    ),
    cohortData = data.table(
      pixelGroup  = c(10, 10, 20),
      speciesCode = c("Abie_bal", "Pinu_con", "Pinu_con"),
      age         = c(50, 60, 80),
      B           = c(15000, 8000, 20000)
    ),
    
    yieldTablesId = data.table::data.table(
      pixelIndex      = 1:4,
      yieldTableIndex = 1
    ),
    yieldTablesCumulative = do.call(rbind, lapply(unique(cohortData$speciesCode), function(speciesCode){
      data.table::data.table(
        yieldTableIndex = 1,
        speciesCode     = speciesCode,
        age             = 0:250,
        biomass         = 0:250 * 100
      )
    })),
    
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
  
  # check that there is a single DOM cohort in the unforested pixel
  expect_equal(nrow(simTest$cbm_vars$key[pixelIndex == 3]), 1)
  
  # The unforested pixels should have no rows in aboveGroundBiomass...
  expect_false(3 %in% simTest$aboveGroundBiomass$pixelIndex)
  
  # ...but should still have a cohort (the borrowed DOM-only spinup cohort)
  # with zero live above-ground biomass in cbm_vars.
  unforestedRowIdx <- simTest$cbm_vars$key[pixelIndex == 3, row_idx]
  expect_length(unique(unforestedRowIdx), 1)
  expect_equal(
    as.numeric(simTest$cbm_vars$pools[unforestedRowIdx, c("Merch", "Foliage", "Other")][1]),
    c(0, 0, 0)
  )
})

