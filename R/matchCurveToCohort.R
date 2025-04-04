spatialMatch <- function(pixelGroupMap, jurisdictions, ecozones){
  if(is.data.table(pixelGroupMap)) {
    if(all(c("pixelIndex", "yieldTableIndex") %in% names(pixelGroupMap))) {
      spatialMatch <- pixelGroupMap
    } else {
      stop("The data table pixelGroupMap need to have the column `pixelIndex`and `yieldTableIndex`")
    }
  } else if (inherits(pixelGroupMap, "SpatRaster")) {
    spatialMatch <- data.table(
      pixelGroup = as.integer(pixelGroupMap[])
    ) 
    spatialMatch <- spatialMatch[, pixelIndex := .I] |> na.omit()
  } else {
    stop("The object pixelGroupMap needs to be a data.table or a SpatRaster")
  }
  if (any(!(spatialMatch$pixelIndex %in% ecozones$pixelIndex))) {
    stop("There is a problem: some pixels cannot be matched to an ecozone...")
  }
  spatialMatch <- spatialMatch[ecozones, on = "pixelIndex"]
  
  if (any(!(spatialMatch$pixelIndex %in% jurisdictions$pixelIndex))) {
    stop("There is a problem: some pixels cannot be matched to a jurisdiction...")
  }
  spatialMatch <- spatialMatch[jurisdictions, on = "pixelIndex"]
  return(spatialMatch)
}

addSpatialUnits <- function(cohortData, spatialUnits) {
  if ("yieldTableIndex" %in% names(spatialUnits)) {
    if("yieldTableIndex" %in% names(cohortData)){
      allInfoCohortData <- merge(cohortData, spatialUnits, by = "yieldTableIndex", allow.cartesian = TRUE)
      allInfoCohortData[, yieldTableIndex := NULL]
      setnames(allInfoCohortData, old = "newytid", new = "yieldTableIndex")
    } else {
      stop("The object cohortData need the column yieldTableIndex")
    }
  } else if ("pixelGroup" %in% colnames(spatialUnits)) {
    if("pixelGroup" %in% names(cohortData)){
      allInfoCohortData <- merge(cohortData, spatialUnits, by = "pixelGroup", allow.cartesian = TRUE)
      allInfoCohortData[, pixelGroup := NULL]
      setnames(allInfoCohortData, old = "newPixelGroup", new = "pixelGroup")
    } else {
      stop("The object cohortData need the column pixelGroup")
    }
  } else {
    stop("The object spatialUnits need the column pixelGroup OR yieldTableIndex")
  }
  return(allInfoCohortData)
}

addCanfiCode <- function(cohortData){
  speciesCode <- unique(cohortData$speciesCode)
  canfi_species <- LandR::sppEquivalencies_CA[match(speciesCode, LandR), CanfiCode]
  
  if(any(is.na(canfi_species))) {
    missing_species <- speciesCode[which(is.na(canfi_species))]
    stop("no species match found for in Boudewyn tables: ", paste(missing_species, collapse = ", "))
  }
  
  sp_canfi <- data.table(speciesCode = speciesCode,
                         canfi_species = canfi_species)
  allCohortInfo <- merge(cohortData, sp_canfi, by = "speciesCode")
  return(allCohortInfo)
}

addForestType <- function(cohortData){
  speciesCode <- unique(cohortData$speciesCode)
  sw_hw <- LandR::sppEquivalencies_CA[match(speciesCode, LandR), Broadleaf]
  sw_hw <- ifelse(sw_hw, "hw", "sw")
  
  if(any(is.na(sw_hw))) {
    missing_species <- speciesCode[which(is.na(sw_hw))]
    stop("no species match found for in Boudewyn tables: ", paste(missing_species, collapse = ", "))
  }
  
  refTable <- data.table(speciesCode = speciesCode,
                         sw_hw = sw_hw)
  allCohortInfo <- merge(cohortData, refTable, by = "speciesCode")
  return(allCohortInfo)
}
