test_that("functions to match AGB with CBM spatial units and canfi species work", {

  # test spatialMatch with pixelGroupDT
  set.seed(1)
  nonforested <- sample(c(1:9), 3)
  pixelGroup <- data.table(pixelIndex = c(1:9),
                           yieldTableIndex = c(rep(1,3), rep(2,3), rep(3,3)))
  pixelGroup <- pixelGroup[-nonforested,]
  
  jurisdictions <- data.table(pixelIndex = c(1:9),
                             jurisdiction = c(rep("a", 5), rep("b", 4)))
  ecozones <- data.table(pixelIndex = c(1:9),
                         ecozone = c(rep(1, 2), rep(2, 5), rep(1, 2)))
  out1 <- spatialMatch(pixelGroup, jurisdictions, ecozones)
  
  expect_is(out1, "data.table")
  expect_named(out1, c("pixelIndex", "yieldTableIndex", "ecozone", "jurisdiction"))
  expect_equal(out1$ecozone, ecozones$ecozone)
  expect_equal(out1$jurisdiction, jurisdictions$jurisdiction)
  expect_true(all(is.na(out1$gcid[nonforested])))

  # test spatialMatch with spatRast
  pixelGroupMap <- terra::rast(
    matrix(data = c(rep(1,3), rep(2, 3), rep(3, 3)), nrow = 3, ncol = 3)
  )
  pixelGroupMap[nonforested] <- NA
  out2 <- spatialMatch(pixelGroupMap, jurisdictions, ecozones)
  expect_is(out2, "data.table")
  expect_named(out2, c("pixelIndex", "pixelGroup", "ecozone", "jurisdiction"), ignore.order = T)
  expect_equal(out2$ecozone, ecozones$ecozone)
  expect_equal(out2$jurisdiction, jurisdictions$jurisdiction)
  expect_true(all(is.na(out2$gcid[nonforested])))
  
  # test addSpatialUnits with yield tables
  spatialUnits <- na.omit(out1)
  spatialUnits[, newytid := .GRP, by = .(yieldTableIndex, ecozone, jurisdiction)]
  spatialUnits <- unique(spatialUnits[, pixelIndex := NULL])
  LandR_species = c("Abie_las", "Betu_pap", "Pice_gla")
  yieldTables <- data.table(
    expand.grid(speciesCode = LandR_species, 
                yieldTableIndex = c(1:3))
  )
  out3 <- addSpatialUnits(yieldTables, spatialUnits)
  
  expect_true(all(LandR_species %in% out3$speciesCode))
  expect_equal(nrow(out3), 15)
  expect_named(out3, c("yieldTableIndex", "speciesCode", "ecozone", "jurisdiction"), ignore.order = TRUE)

  # test with cohortData
  cohortData <- data.table(
    expand.grid(speciesCode = LandR_species, 
                pixelGroup = c(1:3))
  )
  setnames(spatialUnits, old = c("yieldTableIndex", "newytid"), new = c("pixelGroup", "newPixelGroup"))
  out4 <- addSpatialUnits(cohortData, spatialUnits)
  
  expect_true(all(LandR_species %in% out4$speciesCode))
  expect_equal(nrow(out4), 15)
  expect_named(out4, c("pixelGroup", "speciesCode", "ecozone", "jurisdiction"), ignore.order = TRUE)

    # test addSpeciesCode
  out5 <- addSpeciesCode(out3, "CanfiCode")
  expect_equal(nrow(out5), nrow(out3))
  expect_named(out5, c("speciesCode", "ecozone", "jurisdiction", "yieldTableIndex", "newCode"), ignore.order = TRUE)
  expect_is(out5$newCode, "integer")
  expect_true(all(out5$newCode[out5$speciesCode == "Abie_las"] == 304))
  expect_true(all(out5$newCode[out5$speciesCode == "Betu_pap"] == 1303))
  expect_true(all(out5$newCode[out5$speciesCode == "Pice_gla"] == 105))
  
  out6 <- addSpeciesCode(out3, "CBM_speciesID")
  expect_equal(nrow(out6), nrow(out3))
  expect_named(out6, c("speciesCode", "ecozone", "jurisdiction", "yieldTableIndex", "newCode"), ignore.order = TRUE)
  expect_is(out6$newCode, "integer")
  expect_true(all(out6$newCode[out6$speciesCode == "Abie_las"] == 31))
  expect_true(all(out6$newCode[out6$speciesCode == "Betu_pap"] == 76))
  expect_true(all(out6$newCode[out6$speciesCode == "Pice_gla"] == 6))
  expect_error(addSpeciesCode(out3, "notacolumn"))
  
  x <- out3
  x$speciesCode <- as.character(x$speciesCode)
  x$speciesCode[1] <- "notaspecies"
  expect_error(addSpeciesCode(x))
  
  # test add forest Type
  out7 <- addForestType(out3)
  expect_equal(nrow(out7), nrow(out3))
  expect_true("sw_hw" %in% colnames(out7))
  expect_is(out7$sw_hw, "character")
  expect_true(all(out7$sw_hw[out7$speciesCode == "Pice_gla"] == "sw"))
  expect_true(all(out7$sw_hw[out7$speciesCode == "Betu_pap"] == "hw"))
  expect_true(all(out7$canfi_species[out7$speciesCode == "Abie_las"] == "sw"))
  
  })