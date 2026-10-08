
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("Integration with CBM_core: allow age 0 cohorts", {
  
  ## SIMULATE ----
  
  # Set up project
  projectName <- "integration_1-CBM_core_age-0-cohorts"
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
      outputPath  = file.path(spadesTestPaths$temp$outputs, projectName),
      testdata    = spadesTestPaths$testdata,
      tempdata    = file.path(spadesTestPaths$temp$inputs, "intg-CBM_core")
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
    
    # Pixel group 30 holds only an age-0, B = 0 cohort, as LandR produces after a fire.
    pixelGroupMap = terra::rast(matrix(c(10, 20, 30, 30), nrow = 2, ncol = 2, byrow = TRUE)),
    standDT = data.table(
      pixelIndex   = 1:4,
      area         = 1,
      admin_name   = "British Columbia",
      admin_abbrev = "BC",
      eco_id       = 4
    ),
    cohortData = data.table(
      pixelGroup  = c(10, 10, 20, 30),
      speciesCode = c("Abie_bal", "Pinu_con", "Pinu_con", "Abie_bal"),
      age         = c(50, 60, 80, 0),
      B           = c(15000, 8000, 20000, 0)
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
  
  # check ages of age==0 cohorts
  expect_equal(
    simTest$cohortDT[pixelIndex %in% 3:4, .(speciesCode, age)],
    data.table::data.table(speciesCode = rep("Abie_bal", 2), age = rep(2, 2))
  )
  
  # check AGB for age==0 cohorts
  ## Should be 1 cohort group with a total of 1 t/ha of carbon
  ## There would be 0 AGB after the spinup then + 0.5 t/ha per year for 2 years
  AGBage0 <- simTest$cbm_vars$pools[row_idx %in% simTest$cbm_vars$key[pixelIndex %in% 3:4, row_idx]][, .(Merch, Foliage, Other)]
  expect_equal(sum(AGBage0), 1)
  
})

