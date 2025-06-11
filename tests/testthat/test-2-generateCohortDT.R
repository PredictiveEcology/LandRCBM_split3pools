if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("function generateDt works", {
  
  # Create test data
  pixelGroupMap <- rast(matrix(c(10, 20, 10, NA), nrow = 2, ncol = 2, byrow = TRUE))
  cohortData <- data.table(
    pixelGroup = c(10,10,20,20),
    speciesCode = c("Abie_las", "Pinu_con", "Abie_las", "Pinu_ban"),
    B = c(10, 500, 300, 250),
    age = c(10, 10, 100, 0)
  )
  yieldTablesId <- data.table(
    pixelIndex = c(1, 2, 3), 
    yieldTableIndex = c(1001, 1002, 1001) 
  )
  
  ## Runs with yieldTablesId
  
  # Run function
  result <- generateCohortDT(cohortData, pixelGroupMap, yieldTablesId)
  
  # Tests
  expect_is(result, "data.table")
  expect_equal(nrow(result), 5)
  expected_cols <- c("cohortID", "pixelIndex", "speciesCode", "species_id",
                     "age", "gcids", "yieldTableIndex", "sw_hw")
  expect_named(result, expected_cols, ignore.order = TRUE)
  expect_false(0 %in% result$age)
  
  # Pinu_con should all have the same gcids (both with Abi_las)
  gcid_pinu <- result[speciesCode == "Pinu_con", gcids]
  expect_equal(gcid_pinu[1], gcid_pinu[2])
  
  # Abie_las should have different gcids (two with Pinu_con, one alone)
  gcid_abie <- result[speciesCode == "Abie_las", gcids]
  expect_false(gcid_abie[1] == gcid_abie[2])
  
  # gcid for Abie_las and Pinu_con should be different and vice-versa
  expect_false(gcid_pinu[1] %in% gcid_abie)
  expect_false(any(gcid_abie %in% gcid_pinu))
  
  ## Runs without yieldTablesId

  # Run function
  result <- generateCohortDT(cohortData, pixelGroupMap, yieldTablesId = NULL)
  
  # Tests
  expect_is(result, "data.table")
  expect_equal(nrow(result), 5)
  expected_cols <- c("cohortID", "pixelIndex", "speciesCode", "species_id",
                          "age", "gcids", "sw_hw")
  expect_named(result, expected_cols, ignore.order = TRUE)
  expect_false("yieldTableIndex" %in% names(result))
  expect_equal(key(result), "cohortID")
  
  # All cohort should have different gcids
  expect_equal(result$gcids, c(1:5))
  
})