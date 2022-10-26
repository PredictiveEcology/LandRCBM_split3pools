options(repos = c(CRAN = "https://cloud.r-project.org"))
tempDir <- tempdir()

pkgPath <- file.path(tempDir, "packages", version$platform,
                     paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1]))
dir.create(pkgPath, recursive = TRUE)
.libPaths(pkgPath, include.site = FALSE)

if (!require(Require, lib.loc = pkgPath)) {
  install.packages("Require")
  library(Require, lib.loc = pkgPath)
}

setLinuxBinaryRepo()
Require("PredictiveEcology/SpaDES.project@6d7de6ee12fc967c7c60de44f1aa3b04e6eeb5db",
        require = FALSE, upgrade = FALSE, standAlone = TRUE)

paths <- list(inputPath = normPath(file.path(tempDir, "inputs")),
              cachePath = normPath(file.path(tempDir, "cache")),
              modulePath = normPath(file.path(tempDir, "modules")),
              outputPath = normPath(file.path(tempDir, "outputs")))

SpaDES.project::getModule(modulePath = paths$modulePath,
                          c("PredictiveEcology/Biomass_core@master"),
                          overwrite = TRUE)

## make sure all necessary packages are installed:
outs <- SpaDES.project::packagesInModules(modulePath = paths$modulePath)
Require(c(unname(unlist(outs)), "SpaDES"),
        require = FALSE, standAlone = TRUE)

## load necessary packages
Require(c("SpaDES", "LandR", "reproducible"), upgrade = FALSE, install = FALSE)

