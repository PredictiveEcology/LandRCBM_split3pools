test_that("functions to match AGB with CBM spatial units and canfi species work", {
  
  # test spatialMatch with pixelGroupDT
  set.seed(1)
  nonforested <- sample(c(1:9), 3)
  pixelGroup <- data.table(pixelId = c(1:9),
                           gcid = c(rep(1,3), rep(2,3), rep(3,3)))
  pixelGroup <- pixelGroup[-nonforested,]
  
  juridictions <- data.table(pixelId = c(1:9),
                             juridiction = c(rep("a", 5), rep("b", 4)))
  ecozones <- data.table(pixelId = c(1:9),
                         ecozone = c(rep(1, 2), rep(2, 5), rep(1, 2)))
  out1 <- spatialMatch(pixelGroup, juridictions, ecozones)
  
  expect_is(out1, "data.table")
  expect_named(out1, c("pixelId", "gcid", "ecozone", "juridiction"))
  expect_equal(out1$ecozone, ecozones$ecozone)
  expect_equal(out1$juridiction, juridictions$juridiction)
  expect_true(all(is.na(out1$gcid[nonforested])))

    # test spatialMatch with spatRast
  pixelGroupMap <- terra::rast(
    matrix(data = c(rep(1,3), rep(2, 3), rep(3, 3)), nrow = 3, ncol = 3)
  )
  pixelGroupMap[nonforested] <- NA
  out2 <- spatialMatch(pixelGroupMap, juridictions, ecozones)
  expect_is(out2, "data.table")
  expect_named(out2, c("pixelId", "pixelGroup", "ecozone", "juridiction"), ignore.order = T)
  expect_equal(out2$ecozone, ecozones$ecozone)
  expect_equal(out2$juridiction, juridictions$juridiction)
  expect_true(all(is.na(out2$gcid[nonforested])))
  
  # test addSpatialUnits with yield tables
  spatialUnits <- na.omit(out1)
  spatialUnits[, newgcid := .GRP, by = .(gcid, ecozone, juridiction)]
  spatialUnits <- unique(spatialUnits[, pixelId := NULL])
  LandR_species = c("Abie_las", "Betu_pap", "Pice_gla")
  yieldTables <- data.table(
    expand.grid(speciesCode = LandR_species, 
                gcid = c(1:3))
  )
  out3 <- addSpatialUnits(yieldTables, spatialUnits)
  
  expect_true(all(LandR_species %in% out3$speciesCode))
  expect_equal(nrow(out3), 15)
  expect_named(out3, c("gcid", "speciesCode", "ecozone", "juridiction"), ignore.order = TRUE)

  # test with cohortData
  cohortData <- data.table(
    expand.grid(speciesCode = LandR_species, 
                pixelGroup = c(1:3))
  )
  setnames(spatialUnits, old = c("gcid", "newgcid"), new = c("pixelGroup", "newPixelGroup"))
  out4 <- addSpatialUnits(cohortData, spatialUnits)
  
  expect_true(all(LandR_species %in% out4$speciesCode))
  expect_equal(nrow(out4), 15)
  expect_named(out4, c("pixelGroup", "speciesCode", "ecozone", "juridiction"), ignore.order = TRUE)
  
  # test add CanfiCode
  out5 <- addCanfiCode(out3)
  expect_equal(nrow(out5), nrow(out3))
  expect_named(out5, c("speciesCode", "ecozone", "juridiction", "gcid", "canfi_species"), ignore.order = TRUE)
  expect_true(all(out5$canfi_species[out5$speciesCode == "Abie_las"] == 304))
  expect_true(all(out5$canfi_species[out5$speciesCode == "Betu_pap"] == 1303))
  expect_true(all(out5$canfi_species[out5$speciesCode == "Pice_gla"] == 105))
  
  x <- out3
  x$speciesCode <- as.character(x$speciesCode)
  x$speciesCode[1] <- "notaspecies"
  expect_error(addCanfiCode(x))
  
  })