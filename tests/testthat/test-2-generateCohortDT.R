test_that("function generateDt works", {
  # create inputs
  pixelGroupMap <- rast(matrix(c(rep(1,3), NA, rep(1,1), NA, rep(2,2), NA), nrow = 3, ncol = 3))
  yieldTablesId <- data.table(
    pixelIndex = c(1,3:7),
    yieldTableIndex = c(rep(1,2), rep(2,4))
  )
  cohortData <- data.table(
    pixelGroup = c(1,1,2,2),
    speciesCode = c("Abie_las", "Pinu_con", "Abie_las", "Pinu_ban"),
    B = c(10, 500, 300, 250),
    age = c(10, 10, 100, 100)
  )
  
  # with yieldTablesId
  out1 <- generateCohortDT(cohortData, pixelGroupMap, yieldTablesId)
  
  expect_is(out1, "data.table")
  expect_named(out1, c("cohortID", "pixelIndex", "speciesCode", "species_id", "age", 
                      "gcids", "yieldTableIndex", "sw_hw"))
  expect_equal(out1$cohortID, c(1:12))
  expect_true(all(out1$sw_hw == "sw"))
  expect_equal(unique(out1$gcids), c(1:6))
  expect_equal(unique(out1$pixelIndex), c(1,3:7))
  
  # without yieldTablesId
  out2 <- generateCohortDT(cohortData, pixelGroupMap, yieldTablesId = NULL)
  
  expect_is(out2, "data.table")
  expect_named(out2, c("cohortID", "pixelIndex", "speciesCode", "species_id", "age", 
                       "gcids", "sw_hw"))
  expect_equal(out2$cohortID, c(1:12))
  expect_true(all(out2$sw_hw == "sw"))
  expect_equal(out2$gcids, out2$cohortID)
  expect_equal(unique(out2$pixelIndex), c(1,3:7))
  
})