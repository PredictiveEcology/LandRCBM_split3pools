if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

# The annual events AnnualIncrements, UpdateCohortGroups, and PrepareCBMvars must
# give the same results as before they were sped up. The fixture holds the inputs
# of two simulation years (2001: DOM and new cohorts; 2003: DOM cohorts and a
# disturbance) and the outputs of the previous implementation.

fixture <- readRDS(file.path(spadesTestPaths$testdata, "annualCohortGroups", "fixture.rds"))

# Load the event functions from the module script
modEnv <- new.env(parent = globalenv())
for (e in parse(file.path(spadesTestPaths$RProj, "LandRCBM_split3pools.R"))){
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c("AnnualIncrements", "UpdateCohortGroups", "PrepareCBMvars")) eval(e, modEnv)
}

newSim <- function(inputs){
  sim <- new.env()
  for (n in names(inputs)){
    x <- inputs[[n]]
    sim[[n]] <- if (inherits(x, "PackedSpatRaster")) terra::unwrap(x) else
      if (is.data.frame(x)) data.table::copy(x) else
        if (is.list(x)) lapply(x, data.table::copy) else x
  }
  sim
}

for (year in names(fixture)){
  
  test_that(paste("AnnualIncrements and UpdateCohortGroups give the same results:", year), {
    
    sim <- newSim(fixture[[year]]$pre)
    modEnv$AnnualIncrements(sim)
    modEnv$UpdateCohortGroups(sim)
    
    expected <- fixture[[year]]$expectA
    expect_equal(sim$aboveGroundBiomass, expected$agb)
    expect_equal(sim$cohortDT,           expected$cohortDT)
    expect_equal(sim$gcMeta,             expected$gcMeta)
    expect_equal(sim$gcIncrements,       expected$gcIncrements)
    for (tbl in names(expected$cbm_vars)){
      expect_equal(sim$cbm_vars[[tbl]], expected$cbm_vars[[tbl]], info = tbl)
    }
  })
  
  test_that(paste("PrepareCBMvars gives the same results:", year), {
    
    sim <- newSim(fixture[[year]]$mid)
    modEnv$PrepareCBMvars(sim)
    
    expected <- fixture[[year]]$expectP
    for (tbl in names(expected$cbm_vars)){
      expect_equal(sim$cbm_vars[[tbl]], expected$cbm_vars[[tbl]], info = tbl)
    }
  })
}

test_that("uniqueByRepeatedId equals unique() on a table keyed by the id", {
  
  dt <- data.table::data.table(
    cohortID = c(0L, 0L, 0L, 0L, 1L, 2L, 3L, 3L),
    row_idx  = c(5L, 5L, 6L, 5L, 1L, 2L, 3L, 4L),
    other    = c(1, 1, 2, 1, 1, 1, 1, 1))
  data.table::setkey(dt, cohortID)
  
  expect_equal(uniqueByRepeatedId(dt, "cohortID"), unique(dt))
  expect_equal(uniqueByRepeatedId(dt, "cohortID", by = c("row_idx", "cohortID")),
               unique(dt, by = c("row_idx", "cohortID")))
  
  # No repeated ids
  dt2 <- dt[cohortID != 0 & cohortID != 3]
  expect_equal(uniqueByRepeatedId(dt2, "cohortID"), unique(dt2))
  
  # Not keyed by the id: same result as unique()
  dt3 <- data.table::copy(dt)[, cohortID := rev(cohortID)]
  expect_equal(uniqueByRepeatedId(dt3, "cohortID"), unique(dt3))
})
