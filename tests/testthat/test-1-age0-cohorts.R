if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("age-0 cohorts alone in a pixelGroup do not break the above ground pools", {
  
  # Pixel group 30 holds only an age-0, B = 0 cohort, as LandR produces after a fire.
  pixelGroupMap <- rast(matrix(c(10, 20, 30, 30), nrow = 2, ncol = 2, byrow = TRUE))
  standDT <- data.table(
    pixelIndex = 1:4,
    ecozone    = c(5, 5, 5, 5),
    juris_id   = c("BC", "BC", "BC", "BC")
  )
  cohortData <- data.table(
    pixelGroup  = c(10, 10, 20, 30),
    speciesCode = c("Abie_bal", "Pinu_con", "Pinu_con", "Abie_bal"),
    age         = c(50, 60, 80, 0),
    B           = c(15000, 8000, 20000, 0)
  )
  
  table6tb <- fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv", showProgress = FALSE)
  table7tb <- fread("https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv", showProgress = FALSE)
  tableMerch <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1wa2QMd7Eo-bPpfigchdpPPPxo7NVpPiC",
                                         fun = data.table::fread(targetFile, verbose = FALSE),
                                         destinationPath = spadesTestPaths$temp$inputs,
                                         targetFile = "merchantabilityParams.csv")
  tableMerch <- cbind(tableMerch, minAge = 15)
  
  # splitCohortData keeps the same cohorts as generateCohortDT: age > 0 only.
  agb <- splitCohortData(
    cohortData    = copy(cohortData),
    pixelGroupMap = copy(pixelGroupMap),
    standDT       = copy(standDT),
    table6tb      = copy(table6tb),
    table7tb      = copy(table7tb),
    tableMerch    = copy(tableMerch)
  )
  expect_true(all(agb$age > 0))
  expect_equal(nrow(agb), 3) # pixel 1: 2 cohorts, pixel 2: 1, pixels 3 and 4: 0
  expect_false(any(c(3, 4) %in% agb$pixelIndex))
  
  # The spinup pools are replaced cohort by cohort. Pixels 3 and 4 have an age-0
  # cohort that is not in the CBM cohorts, so the pool count matches agb.
  key <- data.table(cohortID = 1:4, pixelIndex = c(1L, 1L, 2L, 3L), row_idx = 1:4)
  state <- data.frame(
    age         = c(60L, 50L, 80L, 0L), # order differs from agb (species, age) order
    speciesCode = factor(c("Pinu_con", "Abie_bal", "Pinu_con", "Abie_bal"))
  )
  pools <- data.frame(Merch = 0, Foliage = 0, Other = 0, Input = 1)
  pools <- pools[rep(1, 4), ]
  
  result <- replaceAboveGroundPools(pools, state, key, agb)
  expect_equal(nrow(result), 4)
  
  # Each cohort gets its own biomass, regardless of row order
  for (i in 1:3) {
    expected <- agb[agb$pixelIndex == key$pixelIndex[i] &
                      agb$speciesCode == as.character(state$speciesCode[i]) &
                      agb$age == state$age[i], ]
    expect_equal(result$Merch[i], expected$merch)
    expect_equal(result$Foliage[i], expected$foliage)
    expect_equal(result$Other[i], expected$other)
  }
  # The age-0 cohort keeps its pools
  expect_equal(result$Merch[4], 0)
  
  # A CBM cohort with no biomass is an error rather than a silent misalignment
  expect_error(
    replaceAboveGroundPools(pools, state, key, agb[agb$age != 80, ]),
    "no above ground biomass"
  )
})
