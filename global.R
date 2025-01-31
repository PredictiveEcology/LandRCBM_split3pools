Require::Require("PredictiveEcology/SpaDES.core")
# Get the minimal amount of packages
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
if (!require("SpaDES.project")){
  Require::Install(c("SpaDES.project", "SpaDES.core", "reproducible"), repos = repos, dependencies = TRUE)
}

## Objects
parameters <- list(
  LandRCBM_split3pools = list(.useCache = ".inputObjects")
  )

modules <- list("LandRCBM_split3pools")

# All the action is here
split3poolsInit <- simInit(
  params = parameters,
  modules = modules,
  paths = list(modulePath = "~/repos/",
               inputPath = "~/repos/LandRCBM_split3pools/inputs/",
               outputPath = "~/repos/LandRCBM_split3pools/outputs/",
               cache = "~/repos/LandRCBM_split3pools/cache/")
  )

split3poolsSim <- spades(split3poolsInit)
