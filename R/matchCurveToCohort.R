spatialMatch <- function(pixelGroupMap, juridictions, ecozones){
  if(is.data.table(pixelGroupMap)) {
    if(all(c("pixelId", "gcid") %in% names(pixelGroupMap))) {
      spatialMatch <- pixelGroupMap
    } else {
      stop("The data table pixelGroupMap need to have the column `pixelId`and `gcid`")
    }
  } else if (inherits(pixelGroupMap, "SpatRaster")) {
    spatialMatch <- data.table(
      pixelGroup = as.integer(pixelGroupMap[])
    ) 
    spatialMatch[, pixelId := .I] |> na.omit()
  } else {
    stop("The object pixelGroupMap needs to be a data.table or a SpatRaster")
  }
  if (any(!(spatialMatch$pixelId %in% ecozones$pixelId))) {
    stop("There is a problem: some pixels cannot be matched to an ecozone...")
  }
  spatialMatch <- spatialMatch[ecozones, on = "pixelId"]
  
  if (any(!(spatialMatch$pixelId %in% juridictions$pixelId))) {
    stop("There is a problem: some pixels cannot be matched to a juridiction...")
  }
  spatialMatch <- spatialMatch[juridictions, on = "pixelId"]
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
      setnames(allInfoCohortData, old = "poolPixelGroup", new = "pixelGroup")
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



matchCurveToCohort <- function(pixelGroupMap, ecozones, juridictions, cohortData = NULL, yieldTablesCumulative = NULL){
  if(!is.null(cohortData)){
    if (!is.null(yieldTablesCumulative)) stop("either cohortData or yieldTablesCumulative need to be NULL") else {
      spatialMatch <- data.table(
        pixelGroup = as.integer(pixelGroupMap[]),
      ) 
      spatialMatch[, pixelId := .I] |> na.omit()
      allCohortInfo <- cohortData
      pixGrColumn <- "pixelGroup"
    }
  }
  if(!is.null(yieldTablesCumulative)){
    if(!is.data.table(pixelGroupMap) | (c("gcid", "pixelId") %in% colnames(pixelGroupMap))) {
      stop("For splitting yield tables, the object pixelGroupMap needs to be a ",
           "data.table with columns `gcid` and `pixelId`.")
    }
    spatialMatch <- pixelGroupMap
    allCohortInfo <- yieldTablesCumulative
    pixGrColumn <- "gcid"
  }
  
  # 1. Spatial matching
  ### need to match the pixel groups with the ecozones and juris_id
  if (any(!(spatialMatch$pixelId %in% ecozones$pixelId))) {
    stop("There is a problem: some pixels cannot be matched to an ecozone...")
  }
  spatialMatch <- spatialMatch[ecozones, on = "pixelId"]
  
  if (any(!(spatialMatch$pixelId %in% juridictions$pixelId))) {
    stop("There is a problem: some pixels cannot be matched to a juridiction...")
  }
  spatialMatch <- spatialMatch[juridictions, on = "pixelId"]
  
  # if(!is.null(yieldSpeciesCodes)){
  #   setnames(pixelGroupEco, old = "pixelGroup", new = pixGrColumn)
  # }
  
  # ### matching the ecozone to the admin
  # if(any(!(c("SpatialUnitID", "abreviation", "EcoBoundaryID") %in% colnames(cbmAdmin)))) {
  #   stop("The object cbmAdmin needs at least the variables `SpatialUnitID`, `abreviation`, `EcoBoundaryID`")
  # }
  # pixelGroupEco <- merge(pixelGroupEco, cbmAdmin, by = "SpatialUnitID")
  # colToKeep <- c(pixGrColumn, "abreviation", "EcoBoundaryID")
  # pixelGroupEco <- pixelGroupEco[, ..colToKeep]
  # pixelGroupEco <- unique(pixelGroupEco)
  # 
  # # add new pixelGroup for when the ecolocation for Boudewyn and for LandR do not fit.
  # if (pixGrColumn == "yieldPixelGroup"){
  #   setorder(pixelGroupEco, yieldPixelGroup, abreviation, EcoBoundaryID)
  #   pixelGroupEco[, poolsPixelGroup := .GRP, by = .(yieldPixelGroup, abreviation, EcoBoundaryID)] 
  # } else {
  #   setorder(pixelGroupEco, pixelGroup, abreviation, EcoBoundaryID)
  #   pixelGroupEco[, poolsPixelGroup := .GRP, by = .(pixelGroup, abreviation, EcoBoundaryID)] 
  # }
  
  # 2. Species matching
  speciesCode <- unique(allCohortInfo$speciesCode)
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


