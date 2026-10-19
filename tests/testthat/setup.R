
if (!testthat::is_testing()){
  suppressPackageStartupMessages(library(testthat))
  testthat::source_test_helpers(env = globalenv())
}

# Source work in progress SpaDES module testing functions
suppressPackageStartupMessages(library(SpaDES.core))
tempScript <- tempfile(fileext = ".R")
download.file(
  "https://raw.githubusercontent.com/suz-estella/SpaDES.core/refs/heads/suz-testthat/R/testthat.R",
  tempScript, quiet = TRUE)
source(tempScript)

# Set up testing directories and global options
SpaDEStestSetGlobalOptions()
spadesTestPaths <- SpaDEStestSetUpDirectories()

# Source the functions
lapply(list.files(file.path(spadesTestPaths$RProj, "R"), full.names = TRUE), source)
Require::Require(c("data.table", "terra"))

# Load the event functions from the module script
modEnv <- new.env(parent = globalenv())
for (e in parse(file.path(spadesTestPaths$RProj, "LandRCBM_split3pools.R"))){
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c(
        "PostSpinupAdjustBiomass", "AnnualIncrements", "UpdateCohortGroups", "PrepareCBMvars"
      )) eval(e, modEnv)
}

# Copy test modules
moduleDirs <- list.files(file.path(spadesTestPaths$testdata, "modules"), full = TRUE)
for (moduleDir in moduleDirs){
  file.copy(moduleDir, spadesTestPaths$temp$modules, recursive = TRUE, overwrite = TRUE)
}

# Install required packages
withr::with_options(c(timeout = 600), Require::Install(
  c(SpaDES.core::packages(modules = basename(getwd()), paths = "..")[[1]],
    "SpaDES.project", "googledrive", "data.table"),
  repos = unique(c("predictiveecology.r-universe.dev", getOption("repos")))
))

