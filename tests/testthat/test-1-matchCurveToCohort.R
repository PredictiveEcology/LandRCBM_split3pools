if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("function addSpatialUnits works", {
  
  ## With yield tables
  
  # Setup: Create sample data
  yieldTableId <- data.table(
    yieldTableIndex = c(101, 102, 103),
    someValue = c(10, 20, 30)
  )
  
  spatialUnits <- data.table(
    yieldTableIndex = c(101, 102, 103),
    newytid = c(201, 202, 203),
    spatialAttribute = c("A", "B", "C")
  )
  # Expected results
  expected_output <- data.table(
    someValue = c(10, 20, 30),
    yieldTableIndex = c(201, 202, 203),
    spatialAttribute = c("A", "B", "C")
  )
  
  # Run function
  result <- addSpatialUnits(yieldTableId, spatialUnits)
  
  # Tests
  expect_is(result, "data.table")
  expect_equal(result, expected_output)
  expect_true("yieldTableIndex" %in% names(result))
  expect_false("newytid" %in% names(result))
  expect_equal(nrow(result), 3)
  
  ## With cohortData
  
  # Setup: create sample data
  cohortData <- data.table(
    pixelGroup = c(1, 2, 3),
    someValue = c(10, 20, 30)
  )
  
  spatialUnits <- data.table(
    pixelGroup = c(1, 2, 3),
    newPixelGroup = c(11, 22, 33),
    spatialAttribute = c("X", "Y", "Z")
  )
  
  # Expected Result
  expected_output <- data.table(
    someValue = c(10, 20, 30),
    pixelGroup = c(11, 22, 33),
    spatialAttribute = c("X", "Y", "Z")
  )
  
  # Run function
  result <- addSpatialUnits(cohortData, spatialUnits)
  
  # Tests
  expect_is(result, "data.table")
  expect_equal(result, expected_output)
  expect_true("pixelGroup" %in% names(result))
  expect_false("newPixelGroup" %in% names(result))
  
})

test_that("function addSpeciesCode works", {
  
  # Test data
  cohortData <- data.table(
    pixelGroup = c(1, 2, 3),
    speciesCode = c("Abie_las", "Betu_pap", "Pice_gla"),
    someValue = c(10, 20, 30)
  )
  
  ## With default CanfiCode
  # Expected result
  expected_output <- data.table(
    pixelGroup = c(1, 2, 3),
    speciesCode = c("Abie_las", "Betu_pap", "Pice_gla"),
    someValue = c(10, 20, 30),
    newCode = c(304, 1303, 105)
  )
  
  # Run function
  result <- addSpeciesCode(cohortData)
  
  # Tests
  expect_is(result, "data.table")
  expect_equal(result, expected_output)
  expect_true("newCode" %in% names(result))
  
  ## With other species code
  # Expected result
  expected_output <- data.table(
    pixelGroup = c(1, 2, 3),
    speciesCode = c("Abie_las", "Betu_pap", "Pice_gla"),
    someValue = c(10, 20, 30),
    newCode = c(31, 76, 6)
  )
  
  # Run function
  result <- addSpeciesCode(cohortData, code = "CBM_speciesID")
  
  # Tests
  expect_equal(result, expected_output)
  expect_true("newCode" %in% names(result))
  
  ## Errors
  
  # Throws an error when run with column not in LandR::sppSpeciesEquivalencies_CA
  expect_error(addSpeciesCode(cohortData, code = "Not a column"))
  
  # Throws an error when species not found
  cohortData <- data.table(
    pixelGroup = c(1, 2, 3),
    speciesCode = c("Abie_las", "Betu_pap", "notaspecies"),
    someValue = c(10, 20, 30)
  )
  
  expect_error(addSpeciesCode(cohortData))
  
})

test_that("function addForestType works", {
  
  # Test data
  cohortData <- data.table(
    pixelGroup = 1:4,
    speciesCode = c("Acer_sac", "Betu_pap", "Pinu_con", "Thuj_jun")
  )
  
  # Run function
  result <- addForestType(cohortData)
  
  # Expected result
  expected_output <- data.table(
    pixelGroup = c(1, 2, 3, 4),
    speciesCode = c("Acer_sac", "Betu_pap", "Pinu_con", "Thuj_jun"),
    sw_hw = c("hw", "hw", "sw", "sw")
  )
  
  # Tests
  expect_is(result, "data.table")
  expect_equal(result, expected_output)
  expect_true("sw_hw" %in% names(result))
  expect_equal(nrow(result), 4)
  
  # Error when incorrect species name
  cohortData <- data.table(
    pixelGroup = 1:2,
    speciesCode = c("Acer_sac", "NotASpecies")
  )
  
  expect_error(addForestType(cohortData))
  
})
  