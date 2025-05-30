## OPTIONS ----

# Suppress warnings from calls to setupProject, simInit, and spades
options("spades.test.suppressWarnings" = TRUE)

# Set custom directory paths
## Speed up tests by allowing inputs, cache, and R packages to persist between runs
options("spades.test.paths.inputs"   = NULL) # inputPath
options("spades.test.paths.cache"    = NULL) # cachePath
options("spades.test.paths.packages" = NULL) # packagePath

# Test recreating the Python virtual environment
## WARNING: this will slow down testing, avoid unless Python is having issues
Sys.setenv(RETICULATE_VIRTUALENV_ROOT = file.path(tempdir(), "virtualenvs"))

## RUN ALL TESTS ----
# Run all tests
testthat::test_dir(file.path("tests", "testthat"))

# Run all tests with different reporters
testthat::test_dir(file.path("tests", "testthat"), reporter = testthat::LocationReporter)
testthat::test_dir(file.path("tests", "testthat"), reporter = testthat::SummaryReporter)

## RUN INDIVIDUAL TESTS ----
testthat::test_file(file.path("tests", "testthat", "test-1-matchCurveToCohort.R"))
testthat::test_file(file.path("tests", "testthat", "test-2-generateCohortDT.R"))
testthat::test_file(file.path("tests", "testthat", "test-3-splitCohortData.R"))
testthat::test_file(file.path("tests", "testthat", "test-4-LandRCBM_split3pools.R"))
