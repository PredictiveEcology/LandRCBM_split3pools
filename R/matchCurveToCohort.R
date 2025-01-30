matchCurveToCohort <- function(pixelGroupMap, spuRaster, cbmAdmin, sp_canfi, cohortData = NULL, CBM_speciesCodes = NULL){
  if(!is.null(cohortData)){
    if (!is.null(CBM_speciesCodes)) stop("either cohortData or CBM_speciesCodes need to be NULL")
    cohort_info <- unique(cohortData[, .(speciesCode, pixelGroup)])
    cohort_info <- cohort_info[,cohort_id := frank(speciesCode, pixelGroup)]
  }
  if(!is.null(CBM_speciesCodes)){
    cohort_info <- CBM_speciesCodes
  }
  
  # Spatial matching
  ### need to match the pixel groups with the ecozones and juris_id
  if (length(pixelGroupMap[]) != length(spuRaster[])) {
    stop("There is a problem: the spuRaster and the pixelGroupMap are not equal")
  }
  pixelGroupEco <- data.table(pixelGroup = as.integer(pixelGroupMap[]),
                              SpatialUnitID = as.integer(spuRaster[]))
  pixelGroupEco <- na.omit(pixelGroupEco, cols = "pixelGroup")
  ### matching the ecozone to the admin
  pixelGroupEco <- merge(pixelGroupEco, cbmAdmin, by = "SpatialUnitID")
  pixelGroupEco[, c("SpatialUnitID", "AdminBoundaryID", "stump_parameter_id", "adminName") := NULL]
  pixelGroupEco <- unique(pixelGroupEco)
  
  # Species matching
  sp_canfi <- matchCanfi(unique(cohort_info$speciesCode), canfi_species)
  
  # putting it together
  allCohortInfo <- merge(cohort_info, sp_canfi, by = "speciesCode")
  # adding other columns
  allCohortInfo <- merge(allCohortInfo, pixelGroupEco, by = "pixelGroup", allow.cartesian = TRUE)
  
  return(allCohortInfo)
}

matchCanfi <- function(LandR_species, canfi_species){
  speciesCode <- LandR_species
  NFI <- LandR::sppEquivalencies_CA$NFI[match(speciesCode, LandR::sppEquivalencies_CA$LandR)]
  canfi_species[, NFI := .(paste(genus, species, sep = "_"))]
  canfi_code <- canfi_species$canfi_species[match(NFI, canfi_species$NFI)]
  return(data.table(speciesCode = speciesCode,
                    canfi_species = canfi_code))
}

#Test
# LandR_species = c("Abie_las", "Betu_pap", "Pice_gla", "Pice_mar", "Pinu_con", "Popu_tre")
# canfi_species = c(304, 1303, 105, 101, 204, 1201)