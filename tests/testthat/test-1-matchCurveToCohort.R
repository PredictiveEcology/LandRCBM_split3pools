test_that("functions to match AGB with CBM spatial units and canfi species work", {
  
  # test matchCanfi
  LandR_species = c("Abie_las", "Betu_pap", "Pice_gla")
  
  pixelGroupMap <- terra::rast(
    matrix(data = c(rep(1,3), rep(2, 6), rep(3, 2), rep(4,5)), nrow = 4, ncol = 4)
  )
  
  spuRaster <- terra::rast(
    matrix(data = c(rep(1,4), rep(2, 10), rep(3, 2)), nrow = 4, ncol = 4)
  )
  
  admin <- data.table(
    SpatialUnitID = c(1:3),
    abreviation = c("a", "b", "b"),
    EcoBoundaryID = c(1, 1, 2),
    otherCol = 1
  )
  
  yieldSpeciesCodes <- data.table(
    expand.grid(speciesCode = LandR_species, 
                yieldPixelGroup = c(1:4))
  )
  out1 <- matchCurveToCohort(pixelGroupMap, 
                             spuRaster, 
                             admin, 
                             yieldSpeciesCodes = yieldSpeciesCodes)
  expect_named(out1, c("yieldPixelGroup", "speciesCode", "canfi_species", "abreviation", "EcoBoundaryID", "poolsPixelGroup"))
  
  # species match
  expect_true(all(!is.na(out1$canfi_species)))
  expect_true(all(out1$canfi_species[out1$speciesCode == "Abie_las"] == 304))
  expect_true(all(out1$canfi_species[out1$speciesCode == "Betu_pap"] == 1303))
  expect_true(all(out1$canfi_species[out1$speciesCode == "Pice_gla"] == 105))
  
  # spatial matching
  expect_true(all(out1$abreviation[out1$yieldPixelGroup == 1] == "a"))
  expect_true(all(out1$abreviation[out1$yieldPixelGroup %in% c(3,4)] == "b"))
  expect_false(all(out1$abreviation[out1$yieldPixelGroup == 2] == "a"))
  expect_true(all(c("a","b") %in% out1$abreviation[out1$yieldPixelGroup == 2]))
  
  expect_true(all(out1$EcoBoundaryID[out1$yieldPixelGroup %in% c(1:3)] == 1))
  expect_true(all(c(1,2) %in% out1$EcoBoundaryID[out1$yieldPixelGroup == 4]))
  expect_false(all(out1$EcoBoundaryID[out1$yieldPixelGroup == 4] == 1))
  
  # test with cohortData
  cohortData <- data.table(
    expand.grid(speciesCode = LandR_species, 
                pixelGroup = c(1:4))
  )
  out2 <- matchCurveToCohort(pixelGroupMap, 
                             spuRaster, 
                             admin, 
                             cohortData = cohortData)
  expect_named(out2, c("pixelGroup", "speciesCode", "canfi_species", "abreviation", "EcoBoundaryID", "poolsPixelGroup"))
  expect_equivalent(out1, out2)
  
  # test errors and warnings
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin, 
                                  cohortData = cohortData,
                                  yieldSpeciesCodes = yieldSpeciesCodes))
  # missing variable
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin, 
                                  yieldSpeciesCodes = yieldSpeciesCodes[,2]))
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin[,-2], 
                                  yieldSpeciesCodes = yieldSpeciesCodes))
  
  # missing species
  LandR_species = c("Abie_las", "Betu_pap", "NOTASPECIES")
  yieldSpeciesCodes <- data.table(
    expand.grid(speciesCode = LandR_species, 
                yieldPixelGroup = c(1:4))
  )
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin, 
                                  yieldSpeciesCodes = yieldSpeciesCodes))
  
  # species as more than a single entry in sppEquivalencies_CA
  LandR_species = c("Abie_las", "Betu_pap", "Alnu_inc")
  yieldSpeciesCodes <- data.table(
    expand.grid(speciesCode = LandR_species, 
                yieldPixelGroup = c(1:4))
  )
  out <- matchCurveToCohort(pixelGroupMap, 
                            spuRaster, 
                            admin, 
                            yieldSpeciesCodes = yieldSpeciesCodes)
  expect_equal(sort(unique(out$canfi_species)), c(304, 1303, 1805))
  
  
  # different raster sizes
  spuRaster <- terra::rast(
    matrix(data = 1L, nrow = 3, ncol = 3)
  )
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin, 
                                  yieldSpeciesCodes = yieldSpeciesCodes))
})