Require::Require("PredictiveEcology/SpaDES.core")
# Get the minimal amount of packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project")){
  Require::Install(c("SpaDES.project", "SpaDES.core", "reproducible"), repos = repos, dependencies = TRUE)
}

## Objects
CBM_AGB <- reproducible::prepInputs("tests/testthat/fixtures/yield_module_sample/CBM_AGB.csv", fun = "data.table::fread")
CBM_AGB <- CBM_AGB[CBM_AGB$pixelGroup %in% c(1,2,3),]
CBM_speciesCodes <- reproducible::prepInputs("tests/testthat/fixtures/yield_module_sample/CBM_speciesCodes.csv", fun = "data.table::fread")
CBM_speciesCodes <- CBM_speciesCodes[CBM_speciesCodes$pixelGroup %in% c(1,2,3),]
pixelGroupMap <- reproducible::prepInputs("tests/testthat/fixtures/yield_module_sample/pixelGroupMap.tiff", fun = "terra::rast")
rasterToMatch = pixelGroupMap
cohortData <- reproducible::prepInputs("tests/testthat/fixtures/cohortData.csv", fun = "data.table::fread")

objects <- list(CBM_AGB = CBM_AGB,
                CBM_speciesCodes = CBM_speciesCodes,
                pixelGroupMap = pixelGroupMap,
                rasterToMatch = rasterToMatch,
                cohortData = cohortData)

parameters <- list(
  LandRCBM_split3pools = list(.useCache = ".inputObjects")
  )

modules <- list("LandRCBM_split3pools")

# All the action is here
split3poolsInit <- simInit(
  params = parameters,
  modules = modules,
  objects = objects,
  paths = list(modulePath = "~/repos/",
               inputPath = "~/repos/LandRCBM_split3pools/inputs/",
               outputPath = "~/repos/LandRCBM_split3pools/outputs/",
               cache = "~/repos/LandRCBM_split3pools/cache/")
  )

split3poolsSim <- spades(split3poolsInit)
