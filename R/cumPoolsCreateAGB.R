cumPoolsCreateAGB <- function(allInfoAGBin, table6, table7){
  counter <- 0L
  cumBiomList <- list()
  #cumList <- list()

  # matching on species name
  for (i in 1:length(unique(allInfoAGBin$canfi_species))) {
    oneSpecies <- allInfoAGBin[canfi_species == unique(CBM_yieldOut$canfi_species)[i], ]

## the 1:NROW seems to not work (have not determined why it does not cycle
## through past the first species

##TODO
##fix this work around
  idj <- unique(oneSpecies$id)
    #for (j in 1:NROW(unique(oneSpecies, on = "id"))) {
  for(j in 1:length(idj)){
    #for (l in 1:nrow(oneSpecies[id == idj[j],])) {
      counter <- counter + 1L
  # this line is for if we are using j in 1:NROW above
  #    oneCurve <- oneSpecies[id == unique(oneSpecies$id)[j], ] ## check if this works oneSpecies[j,]

      oneCurve <- oneSpecies[id == idj[j],]
      ## IMPORTANT BOURDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ##
      oneCurve <- oneCurve[which(age>0),]

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

      # changed this to not have two things called cumBiomList
      #cumBiomList[[counter]] <- data.table(id = oneCurve$id, pixelGroup = oneCurve$pixelGroup,
      #                                     age = oneCurve$age, cumBiom)
      cumBiomList[[counter]] <- data.table(id = oneCurve$id, pixelGroup = oneCurve$pixelGroup,
                                       age = oneCurve$age, cumBiom)
    }
  #cumBiomList <- append(cumBiomList,cumList)
  }
  cumPools <- rbindlist(cumBiomList)
  return(cumPools)
}
