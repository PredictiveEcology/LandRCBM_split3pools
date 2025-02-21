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

# Set up testing global options
SpaDEStestSetGlobalOptions()

# Set up testing directories
spadesTestPaths <- SpaDEStestSetUpDirectories(require = 'googledrive')

# Source the functions
lapply(list.files(file.path(spadesTestPaths$RProj, "R"), full.names = TRUE), source)

# Download some inputs

# Download gcMetaEg usually provided by CBM_vol2biomass
withr::with_options(
  c(googledrive_quiet = TRUE),
  googledrive::drive_download(
    "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo",
    path = file.path(spadesTestPaths$temp$inputs, "canfi_species.csv")
  ))
