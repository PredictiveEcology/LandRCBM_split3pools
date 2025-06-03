
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

addSpeciesCode <- function(cohortData, code = "CanfiCode"){
  if(!(code %in% colnames(LandR::sppEquivalencies_CA))){
    stop("The code argument needs to be a column in the data.table LandR::sppEquivalencies_CA")
  }
  speciesCode <- unique(cohortData$speciesCode)
  newCode <- LandR::sppEquivalencies_CA[match(speciesCode, LandR), get(code)]
  
  if(any(is.na(newCode))) {
    missing_species <- speciesCode[which(is.na(newCode))]
    stop("no species match found for in Boudewyn tables: ", paste(missing_species, collapse = ", "))
  }
  
  sp_code <- data.table(speciesCode = speciesCode,
                        newCode = newCode)
  allCohortInfo <- cohortData[sp_code, on = "speciesCode"]
  
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
  allCohortInfo <- cohortData[refTable ,on = "speciesCode"]
  return(allCohortInfo)
}
