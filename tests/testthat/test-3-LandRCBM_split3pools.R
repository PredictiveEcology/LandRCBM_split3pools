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
  
  simInitInput <-  SpaDEStestMuffleOutput(
    SpaDES.project::setupProject(
      times = list(start = 2011, end = 2061),
      modules = module,
      paths   = list(
        projectPath = projectPath,
        modulePath  = spadesTestPaths$temp$modules,
        inputPath   = spadesTestPaths$temp$inputs,
        outputPath = spadesTestPaths$temp$outputs
      ),
      rasterToMatch = rtm
    )
  )
  
  simTestInit <-  SpaDEStestMuffleOutput(
    SpaDES.core::simInit2(simInitInput)
  )
  
  # is output a simList?
  expect_s4_class(simTestInit, "simList")
  
  simTest <-  SpaDEStestMuffleOutput(
    SpaDES.core::spades(simTestInit, debug = FALSE)
  )
  
  
  
})