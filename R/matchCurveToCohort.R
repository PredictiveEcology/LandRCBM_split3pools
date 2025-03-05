matchCurveToCohort <- function(pixelGroupMap, spuRaster, cbmAdmin, cohortData = NULL, yieldSpeciesCodes = NULL){
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
  
  # add new pixelGroup for when the ecolocation for Boudewyn and for LandR do not fit.
  if (pixGrColumn == "yieldPixelGroup"){
    setorder(pixelGroupEco, yieldPixelGroup, abreviation, EcoBoundaryID)
    pixelGroupEco[, poolsPixelGroup := .GRP, by = .(yieldPixelGroup, abreviation, EcoBoundaryID)] 
  } else {
    setorder(pixelGroupEco, pixelGroup, abreviation, EcoBoundaryID)
    pixelGroupEco[, poolsPixelGroup := .GRP, by = .(pixelGroup, abreviation, EcoBoundaryID)] 
  }
  
  # 2. Species matching
  speciesCode <- unique(cohort_info$speciesCode)
  canfi_species <- LandR::sppEquivalencies_CA[match(speciesCode, LandR), CanfiCode]
  
  if(any(is.na(canfi_species))) {
    missing_species <- speciesCode[which(is.na(canfi_species))]
    stop("no species match found for in Boudewyn tables: ", paste(missing_species, collapse = ", "))
  }
  
  sp_canfi <- data.table(speciesCode = speciesCode,
                         canfi_species = canfi_species)
  
  # 3. putting it together
  allCohortInfo <- merge(cohort_info, sp_canfi, by = "speciesCode")
  # adding other columns
  allCohortInfo <- merge(allCohortInfo, pixelGroupEco, by = pixGrColumn)
  
  return(allCohortInfo)
}


