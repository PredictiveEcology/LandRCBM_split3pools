CBM_AGB <- reproducible::prepInputs("data/yield_module_sample/CBM_AGB.csv", fun = "data.table::fread")
CBM_AGB <- CBM_AGB[CBM_AGB$pixelGroup %in% c(1,2,3),]
CBM_speciesCodes <- reproducible::prepInputs("data/yield_module_sample/CBM_speciesCodes.csv", fun = "data.table::fread")
CBM_speciesCodes <- CBM_speciesCodes[CBM_speciesCodes$pixelGroup %in% c(1,2,3),]
pixelGroupMap <- reproducible::prepInputs("data/yield_module_sample/pixelGroupMap.tiff", fun = "terra::rast")
rasterToMatch = pixelGroupMap

spuSF <- prepInputs(
  url         = "https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed",
  filename1   = "spUnit_Locator.zip",
  targetFile  = "spUnit_Locator.shp",
  alsoExtract = "similar",
  fun         = sf::st_read(targetFile, quiet = TRUE),
  projectTo   = rasterToMatch,
  cropTo      = rasterToMatch
)

spuRaster <- terra::rasterize(
  terra::vect(spuSF),
  rasterToMatch,
  field = "spu_id"
)

canfi_species <- prepInputs(url = "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo",
                            fun = "data.table::fread",
                            filename2 = "canfi_species.csv")
cbmAdmin <- prepInputs(url = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz",
           fun = "data.table::fread",
           filename2 = "cbmAdmin.csv")

matchCurveToCohort(
  CBM_speciesCodes = CBM_speciesCodes,
  pixelGroupMap = pixelGroupMap,
  spuRaster = spuRaster,
  cbmAdmin = cbmAdmin,
  sp_canfi = canfi_species,
  cohortData = NULL
)
