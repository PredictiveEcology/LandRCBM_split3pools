test_that("function to split cohortData works", {
  
  # Prepare mock inputs
  pixelGroupMap <- rast(matrix(c(10,10,20,20), nrow = 2, ncol = 2))
  
  jurisdictions <- data.table(
    pixelIndex = 1:4,
    PRUID = c(48, 48, 59, 59), 
    juris_id = c("AB", "AB", "BC", "BC")
  )
  
  ecozones <- data.table(
    pixelIndex = 1:4,
    ecozone = c(5, 6, 5, 5) 
  )
  
  cohortData <- data.table(
    pixelGroup = c(10, 20, 20),
    speciesCode = c("Abie_bal", "Pinu_con", "Popu_tre"),
    age = c(50, 80, 80),
    B = c(15000, 20000, 5000) # Biomass in g/m^2
  )
  
  table6 <- fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv", showProgress = FALSE)
  table7 <- fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv", showProgress = FALSE)
  
  # Check that it runs with no error
  expect_no_error({
    result <- splitCohortData(
      cohortData = copy(cohortData), # Use copy to avoid side effects
      pixelGroupMap = copy(pixelGroupMap),
      jurisdictions = copy(jurisdictions),
      ecozones = copy(ecozones),
      table6 = copy(table6),
      table7 = copy(table7)
    )
  })
  
  # Check result type and columns
  expect_s3_class(result, "data.table")
  expect_named(result, c("pixelIndex", "speciesCode", "age", "merch", "foliage", "other"))
  
  # Check column types
  expect_type(result$pixelIndex, "integer")
  expect_type(result$speciesCode, "character")
  expect_type(result$age, "double") 
  expect_type(result$merch, "double")
  expect_type(result$foliage, "double")
  expect_type(result$other, "double")
  
  # Check that it produces the expected number of rows
  result <- splitCohortData(
    cohortData = copy(cohortData),
    pixelGroupMap = copy(pixelGroupMap),
    jurisdictions = copy(jurisdictions),
    ecozones = copy(ecozones),
    table6 = copy(table6),
    table7 = copy(table7)
  )
  
  # Calculate expected rows:
  # Pixel 1 (pg 10, AB, eco 5): 1 cohort (Abie_Bal) -> 1 row
  # Pixel 2 (pg 20, AB, eco 6): 1 cohort (Pinu_Con, Popu_Tre) -> 2 rows
  # Pixel 3 (pg 10, BC, eco 5): 2 cohorts (Abie_Bal) -> 1 row
  # Pixel 4 (pg 20, BC, eco 5): 2 cohorts (Pinu_Con, Popu_Tre) -> 2 rows
  # Total expected = 1 + 1 + 2 + 2 = 6 rows
  expect_equal(nrow(result), 6)
  
  # Check if pixels are present
  expect_equal(sort(unique(result$pixelIndex)), 1:4)
  
  # Check that it split biomass correctly
  result <- splitCohortData(
    cohortData = copy(cohortData),
    pixelGroupMap = copy(pixelGroupMap),
    jurisdictions = copy(jurisdictions),
    ecozones = copy(ecozones),
    table6 = copy(table6), # Use simplified table
    table7 = copy(table7)
  )
  
  # Add total biomass column to result
  result[, total_split := merch + foliage + other]
  
  # Create expected total biomass per cohort (in tonnes/ha)
  expected_totals <- copy(cohortData)
  expected_totals[, expected_B_tonnes_ha := B / 200]
  
  # Check for a specific pixel/cohort combination
  # Pixel 1: pg=10, Abie_bal, age=50, B=15000 -> expected = 75 t/ha
  res_pix1 <- result[pixelIndex == 1 & speciesCode == "Abie_bal"]
  expect_equal(nrow(res_pix1), 1)
  expect_equal(res_pix1$total_split, 75, tolerance = 1e-6)
  
  # Pixel 2: pg=20, Pinu_con, age=80, B=20000 -> expected = 100 t/ha
  res_pix2_pc <- result[pixelIndex == 2 & speciesCode == "Pinu_con"]
  expect_equal(nrow(res_pix2_pc), 1)
  expect_equal(res_pix2_pc$total_split, 100, tolerance = 1e-6)
  
  # Pixel 2: pg=20, Popu_tre, age=80, B=5000 -> expected = 25 t/ha
  res_pix2_pt <- result[pixelIndex == 2 & speciesCode == "Popu_tre"]
  expect_equal(nrow(res_pix2_pt), 1)
  expect_equal(res_pix2_pt$total_split, 25, tolerance = 1e-6)
  
})