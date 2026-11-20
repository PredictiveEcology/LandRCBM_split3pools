
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("module runs with Biomass_core and CBM_core when dynamic", {
  
  skip_if(as.logical(Sys.getenv("CI", FALSE)) & !"main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")))
  
  # Set up project
  simInitInput <- setupProjectTest(
    projectName =  "integration_LandRCBM",
    times       = list(start = 2000, end = 2021),
    modules     = c(
      paste0("PredictiveEcology/CBM_core@",     ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      paste0("PredictiveEcology/Biomass_core@", ifelse("main" %in% Sys.getenv(c("GITHUB_BASE_REF", "GITHUB_REF_NAME")), "main", "development")),
      "LandRCBM_split3pools"
    ),
    
    params = list(
      .globals = list(
        .plots = NA,
        dataYear = 2001, #will get kNN 2011 data, and NTEMS 2011 landcover
        sppEquivCol = 'LandR'
      ),
      CBM_core = list(
        .plot = FALSE,
        skipPrepareCBMvars = TRUE
      )
    ),
    
    rasterToMatch         = file.path(test_path("testdata"), "rasterToMatch.tif") |> terra::rast(),
    studyArea             = file.path(test_path("testdata"), "studyArea.shp") |> terra::vect(),
    standDT               = file.path(test_path("testdata"), "CBM", "standDT.csv") |> data.table::fread(),
    biomassMap            = file.path(test_path("testdata"), "LandR", "biomassMap.tif") |> terra::rast(),
    cohortData            = file.path(test_path("testdata"), "LandR", "cohortData.csv") |> data.table::fread(stringsAsFactors = TRUE),
    pixelGroupMap         = file.path(test_path("testdata"), "LandR", "pixelGroupMap.tif") |> terra::rast(),
    speciesLayers         = file.path(test_path("testdata"), "LandR", "speciesLayers.tif") |> terra::rast(),
    ecoregionMap          = file.path(test_path("testdata"), "LandR", "ecoregionMap.tif") |> terra::rast(),
    minRelativeB          = file.path(test_path("testdata"), "LandR", "minRelativeB.csv") |> data.table::fread(stringsAsFactors = TRUE),
    ecoregion             = file.path(test_path("testdata"), "LandR", "ecoregion.csv") |> data.table::fread(colClasses = list(factor = c("ecoregionGroup"))),
    species               = file.path(test_path("testdata"), "LandR", "species.csv") |> data.table::fread(colClasses = list(factor = c("Area", "postfireregen", "hardsoft", "speciesCode"))),
    speciesEcoregion      = file.path(test_path("testdata"), "LandR", "speciesEcoregion.csv") |> data.table::fread(stringsAsFactors = TRUE),
    yieldTablesCumulative = file.path(test_path("testdata"), "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(test_path("testdata"), "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")
  
  # Run spades
  simTest <- SpaDES.core::spades(simTestInit)
  expect_s4_class(simTest, "simList")
  
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
    "accumulateResults"
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
    "summarizeAGBPools"
  )
  realizedEventOrder <- completedEvents[eventTime == end(simTest) & eventType %in% eventsToCheck]
  expect_equal(expectedEventOrder, realizedEventOrder$eventType)
  
  # check output object structure
  check_module_outputs(simTest)
  
  # check cbm_vars
  expect_in(simTest$cbm_vars$state[speciesCode == "Abie_las", sw_hw], 0L) # SW
  expect_in(simTest$cbm_vars$state[speciesCode == "Pinu_con", sw_hw], 0L) # SW
  
  expect_true(all(simTest$cbm_vars$state$spatial_unit_id == 42))
  
})

