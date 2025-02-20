## SET UP ----

# Get needed packages
packages = c("SpaDES.core", "SpaDES.project", "reproducible", "data.table", "ggplot2", "terra",
             "SpaDES.tools", "CBMutils", "LandR")
if (!requireNamespace("Require", quietly = TRUE)) {
  install.packages("Require")
}
Require::Require(packages, dependencies = TRUE, repos = unique(c("predictiveecology.r-universe.dev", getOption("repos"))))

# Suppress warnings from calls to setupProject, simInit, and spades
options("spades.test.suppressWarnings" = TRUE)

# Set custom input data location
options("reproducible.inputPaths" = NULL)

## RUN ALL TESTS ----
# Run all tests
testthat::test_dir(file.path("tests", "testthat"))

# Run all tests with different reporters
testthat::test_dir(file.path("tests", "testthat"), reporter = testthat::LocationReporter)
testthat::test_dir(file.path("tests", "testthat"), reporter = testthat::SummaryReporter)

## RUN INDIVIDUAL TESTS ----
testthat::test_file(file.path("tests", "testthat", "test-1-matchCurveToCohort.R"))
testthat::test_file(file.path("tests", "testthat", "test-2-cumPoolsCreateAGB.R"))
testthat::test_file(file.path("tests", "testthat", "test-3-LandRCBM_split3pools.R"))
