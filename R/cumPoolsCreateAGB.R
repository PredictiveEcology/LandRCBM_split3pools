cumPoolsCreateAGB <- function(CBM_yieldOut, pixelGroupEco, table6, table7){
#
#   fullSpecies, gcMeta, userGcM3,
#                            stable3, stable4, stable5, stable6, stable7, thisAdmin) {

  counter <- 0L
  cumBiomList <- list()
  browser()

  allInfoAGBin <- merge(CBM_yieldOut,pixelGroupEco, by = "pixelGroup")

  for (i in 1:length(unique(allInfoAGBin$canfi_species))) { #fullSpecies
    # matching on species name
    oneSpecies <- allInfoAGBin[canfi_species == unique(CBM_yieldOut$canfi_species)[i], ]#gcMeta[species == fullSpecies #speciesMeta
    # for each species name, process one gcID at a time
browser()
1:NROW(unique(speciesMeta, on = "gcids")
       unique(oneSpecies$id)
    for (j in 1:NROW(unique(oneSpecies, on = "id"))) {#speciesMeta
      counter <- counter + 1L
      if(counter == 4){
        browser()
      }

            oneCurve <- oneSpecies[id == unique(oneSpecies$id)[j], ] ## check if this works oneSpecies[j,]
      #ecozone <- meta$ecozones
      #id <- userGcM3$GrowthCurveComponentID[which(userGcM3$GrowthCurveComponentID == meta$growth_curve_component_id)][-1]

      ## IMPORTANT BOURDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ##
      oneCurve <- oneCurve[which(age>0),]
      # age <- userGcM3[GrowthCurveComponentID == meta$growth_curve_component_id, Age]
      # age <- age[which(age>0)]

      # series of fncts results in curves of merch, foliage and other (SW or HW)

      cumBiom <- as.matrix(convertAGB2pools(oneCurve, table6, table7))

      # going from tonnes of biomass/ha to tonnes of carbon/ha here
      ### HARD CODED VALUE ####################
      cumBiom <- cumBiom * 0.5 ## this value is in sim$cbmData@biomassToCarbonRate
      # calculating the increments per year for each of the three pools (merch,
      # foliage and other (SW or HW))
      # inc <- diff(cumBiom)
      # CBM processes half the growth before turnover and OvermatureDecline, and
      # half after.
      # names(outInputs$allProcesses)
      # [1] "Disturbance"       "Growth1"           "DomTurnover"       "BioTurnover"
      # [5] "OvermatureDecline" "Growth2"           "DomDecay"          "SlowDecay"
      # [9] "SlowMixing"
      cumBiomList[[counter]] <- data.table(id = oneCurve$id, pixelGroup = oneCurve$pixelGroup,
                                           age = oneCurve$age, cumBiom)

      # cumPools <- rbind(cumPools, cumBiom)
    }
  }
  cumPools <- rbindlist(cumBiomList)
  return(cumPools)
}
