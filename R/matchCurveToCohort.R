matchCurveToCohort <- function(pixelGroupMap, spuRaster, cbmAdmin, canfi_species, cohortData = NULL, yieldSpeciesCodes = NULL){
  if(!is.null(cohortData)){
    if (!is.null(yieldSpeciesCodes)) stop("either cohortData or yieldSpeciesCodes need to be NULL") else {
      cohort_info <- cohortData
      pixGrColumn <- "pixelGroup"
      obj <- "cohortData"
    }
  }
  if(!is.null(yieldSpeciesCodes)){
    cohort_info <- yieldSpeciesCodes
    pixGrColumn <- "yieldPixelGroup"
    obj <- "yieldSpeciesCodes"
  }
  
  if(any(!(c(pixGrColumn, "speciesCode") %in% colnames(cohort_info)))) {
    stop("The object ", obj, " needs at least the variables ", pixGrColumn, " and speciesCode")
  }
  
  # 1. Spatial matching
  ### need to match the pixel groups with the ecozones and juris_id
  if (length(pixelGroupMap[]) != length(spuRaster[])) {
    stop("There is a problem: the spuRaster and the pixelGroupMap are not equal")
  }
  pixelGroupEco <- data.table(pixelGroup = as.integer(pixelGroupMap[]),
                              SpatialUnitID = as.integer(spuRaster[]))
  pixelGroupEco <- na.omit(pixelGroupEco, cols = "pixelGroup")
  if(!is.null(yieldSpeciesCodes)){
    setnames(pixelGroupEco, old = "pixelGroup", new = pixGrColumn)
  }
  
  ### matching the ecozone to the admin
  if(any(!(c("SpatialUnitID", "abreviation", "EcoBoundaryID") %in% colnames(cbmAdmin)))) {
    stop("The object cbmAdmin needs at least the variables `SpatialUnitID`, `abreviation`, `EcoBoundaryID`")
  }
  pixelGroupEco <- merge(pixelGroupEco, cbmAdmin, by = "SpatialUnitID")
  colToKeep <- c(pixGrColumn, "abreviation", "EcoBoundaryID")
  pixelGroupEco <- pixelGroupEco[, ..colToKeep]
  pixelGroupEco <- unique(pixelGroupEco)
  
  # 2. Species matching
  if(any(!(c("genus", "species", "canfi_species") %in% colnames(canfi_species)))) {
    stop("The object canfi_species needs at least the variables `genus`, `species`, `canfi_species`")
  }
  sp_canfi <- matchCanfi(unique(cohort_info$speciesCode), canfi_species)
  
  # 3. putting it together
  allCohortInfo <- merge(cohort_info, sp_canfi, by = "speciesCode")
  # adding other columns
  allCohortInfo <- merge(allCohortInfo, pixelGroupEco, by = pixGrColumn, allow.cartesian = TRUE)
  
  return(allCohortInfo)
}

matchCanfi <- function(LandR_species, canfi_species){
  speciesCode <- LandR_species
  NFIcode <- LandR::sppEquivalencies_CA$NFI[match(speciesCode, LandR::sppEquivalencies_CA$LandR)]
  canfi_species$NFIcode = paste(canfi_species$genus, canfi_species$species, sep = "_")
  canfi_code <- canfi_species$canfi_species[match(NFIcode, canfi_species$NFIcode)]
  return(data.table(speciesCode = speciesCode,
                    canfi_species = canfi_code))
}

