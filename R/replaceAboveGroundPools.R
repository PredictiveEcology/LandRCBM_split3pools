# Replace the above ground pools of the spinup output with the LandR biomass.
# `pools` and `state` have one row per cohort, in the order of `key`. The rows
# are matched to `aboveGroundBiomass` on pixelIndex, speciesCode and age, not by
# position. Age-0 cohorts (which CBM does not simulate) keep their pools.
replaceAboveGroundPools <- function(pools, state, key, aboveGroundBiomass){
  cohorts <- data.table(
    pixelIndex  = key$pixelIndex,
    speciesCode = as.character(state$speciesCode),
    age         = as.numeric(state$age)
  )
  agb <- copy(aboveGroundBiomass)[, `:=`(speciesCode = as.character(speciesCode), age = as.numeric(age))]
  agb <- agb[cohorts, on = c("pixelIndex", "speciesCode", "age")]
  
  nonAge0 <- state$age > 0
  if (anyNA(agb$merch[nonAge0])) {
    stop("Some CBM cohorts have no above ground biomass from LandR.")
  }
  pools[nonAge0, c("Merch", "Foliage", "Other")] <- agb[nonAge0, .(merch, foliage, other)]
  return(pools)
}
