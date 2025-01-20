# Get the minimal amount of packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project")){
  Require::Install(c("SpaDES.project", "SpaDES.core", "reproducible"), repos = repos, dependencies = TRUE)
}

## Objects
CBM_AGB <- reproducible::prepInputs("data/yield_module_sample/CBM_AGB.csv", fun = "data.table::fread")
CBM_AGB <- CBM_AGB[CBM_AGB$pixelGroup %in% c(1,2,3),]
CBM_speciesCodes <- reproducible::prepInputs("data/yield_module_sample/CBM_speciesCodes.csv", fun = "data.table::fread")
CBM_speciesCodes <- CBM_speciesCodes[CBM_speciesCodes$pixelGroup %in% c(1,2,3),]
pixelGroupMap <- reproducible::prepInputs("data/yield_module_sample/pixelGroupMap.tiff", fun = "terra::rast")
rasterToMatch = pixelGroupMap

objects <- list(CBM_AGB = CBM_AGB,
                CBM_speciesCodes = CBM_speciesCodes,
                pixelGroupMap = pixelGroupMap,
                rasterToMatch = rasterToMatch)

parameters <- list(
  LandRCBM_split3pools = list(.useCache = ".inputObjects")
  )

modules <- list("LandRCBM_split3pools")

# All the action is here
split3poolsInit <- SpaDES.core::simInit(
  params = parameters,
  modules = modules,
  objects = objects,
  paths = list(modulePath = "~/Repos/")
  )

split3poolsSim <- spades(split3poolsInit)
