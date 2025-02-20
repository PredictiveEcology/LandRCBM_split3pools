

# test matchCanfi
LandR_species = c("Abie_las", "Betu_pap", "Pice_gla")

canfi_sp <- reproducible::prepInputs(
  url = "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo",
  fun = "data.table::fread",
  filename2 = "canfi_species.csv",
  destinationPath = "inputs"
)




# test with yieldCurves
pixelGroupMap <- terra::rast(
  matrix(data = c(rep(1,3), rep(2, 6), rep(3, 2), rep(4,5)), nrow = 4, ncol = 4)
)

spuRaster <- terra::rast(
  matrix(data = c(rep(1,4), rep(2, 10), rep(3, 2)), nrow = 4, ncol = 4)
)

admin <- data.table(
  SpatialUnitId = c(1:3),
  abbreviation = c("a", "b", "b"),
  EcoBoundaryID = c(1, 1, 2),
  otherCol = 1
)


# test with cohortData




source("R/matchCurveToCohort.R")
library(data.table)
CBM_AGB <- reproducible::prepInputs(
  "tests/testthat/fixtures/yield_module_sample/CBM_AGB.csv", 
  fun = "data.table::fread"
)
CBM_AGB <- CBM_AGB[CBM_AGB$pixelGroup %in% c(1,2,3),]
CBM_speciesCodes <- reproducible::prepInputs(
  "tests/testthat/fixtures/yield_module_sample/CBM_speciesCodes.csv", 
  fun = "data.table::fread"
)
CBM_speciesCodes <- CBM_speciesCodes[CBM_speciesCodes$pixelGroup %in% c(1,2,3),]
pixelGroupMap <- reproducible::prepInputs(
  "tests/testthat/fixtures/yield_module_sample/pixelGroupMap.tiff", 
  fun = "terra::rast"
)
rasterToMatch = pixelGroupMap

spuSF <- reproducible::prepInputs(
  url         = "https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed",
  filename1   = "spUnit_Locator.zip",
  targetFile  = "spUnit_Locator.shp",
  alsoExtract = "similar",
  fun         = sf::st_read(targetFile, quiet = TRUE),
  projectTo   = rasterToMatch,
  cropTo      = rasterToMatch,
  destinationPath = "inputs"
)

spuRaster <- terra::rasterize(
  terra::vect(spuSF),
  rasterToMatch,
  field = "spu_id"
)

canfi_species <- reproducible::prepInputs(
  url = "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo",
  fun = "data.table::fread",
  filename2 = "canfi_species.csv",
  destinationPath = "inputs"
)
cbmAdmin <- reproducible::prepInputs(
  url = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz",
  fun = "data.table::fread",
  filename2 = "cbmAdmin.csv",
  destinationPath = "inputs"
)

out <- matchCurveToCohort(
  CBM_speciesCodes = CBM_speciesCodes,
  pixelGroupMap = pixelGroupMap,
  spuRaster = spuRaster,
  cbmAdmin = cbmAdmin,
  sp_canfi = canfi_species,
  cohortData = NULL
)
fwrite(out, "tests/testthat/fixtures/allInfoYieldTables.csv")
expect_is(out, "data.table")
expect_equal(ncol(out), 6)
expect_equal(length(unique(out$cohort_id)), nrow(out))
