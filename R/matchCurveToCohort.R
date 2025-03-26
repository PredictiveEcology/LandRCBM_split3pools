spatialMatch <- function(pixelGroupMap, juridictions, ecozones){
  if(is.data.table(pixelGroupMap)) {
    if(all(c("pixelIndex", "gcid") %in% names(pixelGroupMap))) {
      spatialMatch <- pixelGroupMap
    } else {
      stop("The data table pixelGroupMap need to have the column `pixelIndex`and `gcid`")
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
  
  if (any(!(spatialMatch$pixelIndex %in% juridictions$pixelIndex))) {
    stop("There is a problem: some pixels cannot be matched to a juridiction...")
  }
  spatialMatch <- spatialMatch[juridictions, on = "pixelIndex"]
  return(spatialMatch)
}

addSpatialUnits <- function(cohortData, spatialUnits) {
  if ("gcid" %in% names(spatialUnits)) {
    if("gcid" %in% names(cohortData)){
      allInfoCohortData <- merge(cohortData, spatialUnits, by = "gcid", allow.cartesian = TRUE)
      allInfoCohortData[, gcid := NULL]
      setnames(allInfoCohortData, old = "newgcid", new = "gcid")
    } else {
      stop("The object cohortData need the column gcid")
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
    stop("The object spatialUnits need the column pixelGroup OR gcid")
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
