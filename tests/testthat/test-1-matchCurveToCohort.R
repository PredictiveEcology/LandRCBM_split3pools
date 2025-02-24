test_that("functions to match AGB with CBM spatial units and canfi species work", {
  
  # test matchCanfi
  LandR_species = c("Abie_las", "Betu_pap", "Pice_gla")
  
  
  canfi_sp <- reproducible::prepInputs(
    url = "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo",
    fun = "data.table::fread",
    filename2 = "canfi_species.csv",
    destinationPath = spadesTestPaths$temp$inputs,
    overwrite = TRUE
  )
  
  out <- matchCanfi(LandR_species, canfi_sp)
  
  expect_equal(out$speciesCode, LandR_species)
  expect_equal(out$canfi_species, c(304, 1303, 105))
  expect_true(all(colnames(out) == c("speciesCode", "canfi_species")))
  
  # try adding a species that is not in canfi
  LandR_species = c("Abie_las", "Betu_pap", "Pice_gla", "Betu_sp")
  
  out <- matchCanfi(LandR_species, canfi_sp)
  expect_equal(out$canfi_species, c(304, 1303, 105, NA))
  
  # test matchCurveCohort with yieldCurves
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
                             canfi_sp, 
                             yieldSpeciesCodes = yieldSpeciesCodes)
  expect_true(all(c("yieldPixelGroup", "speciesCode", "canfi_species", "abreviation", "EcoBoundaryID") %in% colnames(out1)))
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
                             canfi_sp, 
                             cohortData = cohortData)
  expect_true(all(c("pixelGroup", "speciesCode", "canfi_species", "abreviation", "EcoBoundaryID") %in% colnames(out2)))
  expect_equivalent(out1, out2)
  
  # test errors and warnings
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin, 
                                  canfi_sp, 
                                  cohortData = cohortData,
                                  yieldSpeciesCodes = yieldSpeciesCodes))
  # missing variable
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin, 
                                  yieldSpeciesCodes = yieldSpeciesCodes))
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin, 
                                  canfi_sp, 
                                  yieldSpeciesCodes = yieldSpeciesCodes[,2]))
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin, 
                                  canfi_sp[,-1], 
                                  yieldSpeciesCodes = yieldSpeciesCodes))
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin[,-2], 
                                  canfi_sp, 
                                  yieldSpeciesCodes = yieldSpeciesCodes))
  
  # different raster sizes
  spuRaster <- terra::rast(
    matrix(data = 1L, nrow = 3, ncol = 3)
  )
  expect_error(matchCurveToCohort(pixelGroupMap, 
                                  spuRaster, 
                                  admin, 
                                  canfi_sp, 
                                  yieldSpeciesCodes = yieldSpeciesCodes))
})