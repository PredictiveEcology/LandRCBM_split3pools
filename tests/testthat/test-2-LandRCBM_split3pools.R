
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("module runs as a standAlone when not dynamic", {

  # Set up project
  simInitInput <- setupProjectTest(
    projectName = "LandRCBM_split3pools",
    times       = list(start = 2000, end = 2000),
    
    standDT               = file.path(test_path("testdata"), "CBM", "standDT.csv") |> data.table::fread(),
    cohortData            = file.path(test_path("testdata"), "LandR", "cohortData.csv") |> data.table::fread(),
    pixelGroupMap         = file.path(test_path("testdata"), "LandR", "pixelGroupMap.tif") |> terra::rast(),
    yieldTablesCumulative = file.path(test_path("testdata"), "LandR", "yieldTablesCumulative.csv") |> data.table::fread(),
    yieldTablesId         = file.path(test_path("testdata"), "LandR", "yieldTablesId.csv") |> data.table::fread()
  )
  
  # Run simInit
  simTestInit <- SpaDES.core::simInit2(simInitInput)
  expect_s4_class(simTestInit, "simList")
  
  ## Get through init event:
  #  splits the cohort data and the yield tables
  simTest <- SpaDES.core::spades(
    simTestInit, events = list("LandRCBM_split3pools" = c("init", "splitInit")))
  expect_s4_class(simTest, "simList")
  
  # check output object structure
  check_module_outputs(simTest, spinup = TRUE)
  
  # check yieldTablesCumulative
  preLandRCBM_ytc <- file.path(test_path("testdata"), "LandR", "yieldTablesCumulative.csv") |> data.table::fread()
  expect_equal(nrow(simTest$yieldTablesCumulative), nrow(preLandRCBM_ytc))

})

