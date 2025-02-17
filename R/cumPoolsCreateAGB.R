## Questions for Celine, DC 2025-01-30
# 1. The calculation seems to refer to vol2biomass, make sure that this is correct.
# 2. FORCS parameters are hard coded: minimum merchantable age, a, and b (used 
#    to calculate the proportion of merchantable Stemwood)

cumPoolsCreateAGB <- function(allInfoAGBin, table6, table7, pixGroupCol = "pixelGroup"){
  counter <- 0L
  cumBiomList <- list()
  
  # identify unique sp, juris_id, ecozone
  curves <- unique(allInfoAGBin[, .(canfi_species, juris_id, ecozone)])
  curves[,curve_id := .I]
  
  AGB <- merge(allInfoAGBin, curves, by = c("canfi_species", "juris_id", "ecozone"), all.x = TRUE)
  # do one set of parameters at a time
  for (i_curve in curves$curve_id) {
    counter <- counter + 1L
    oneCurve <- AGB[curve_id == i_curve, ]
    
    ## IMPORTANT BOURDEWYN PARAMETERS FOR NOT HANDLE AGE 0 ##
    oneCurve <- oneCurve[which(age>0),]
    
    # series of fncts results in curves of merch, foliage and other (SW or HW)
    cumBiom <- as.matrix(convertAGB2pools(oneCurve, table6, table7))
    
    # going from tonnes of biomass/ha to tonnes of carbon/ha here
    ### HARD CODED VALUE ####################
    cumBiom <- cumBiom * 0.5 ## this value is in sim$cbmData@biomassToCarbonRate
    
    ##############DC 2025-01-30 Not sure what these comments refer to.
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
    #cumBiomList[[counter]] <- data.table(id = oneCurve$cohort_id, pixelGroup = oneCurve$pixelGroup,
    #                                     age = oneCurve$age, cumBiom)
    #################
    if(pixGroupCol != "yieldPixelGroup") cohort_id <- NULL
    
    cumBiomList[[counter]] <- oneCurve[, 
                                       .(gcids = cohort_id,
                                         species = speciesCode,
                                         age = age,
                                         pixGroupColValue = get(pixGroupCol))]  # Use get() to refer to pixGroupCol dynamically
    setnames(cumBiomList[[counter]], "pixGroupColValue", pixGroupCol)
    cumBiomList[[counter]] <- cbind(cumBiomList[[counter]],
                                    cumBiom)
    
  }
  cumPools <- rbindlist(cumBiomList)
  return(cumPools)
}

convertAGB2pools <- function(oneCurve, table6, table7){
  
  # get the parameters
  spec <- as.integer(unique(oneCurve$canfi_species))
  ez <- unique(oneCurve$ecozone)
  admin <- unique(oneCurve$juris_id)
  params6 <- table6[canfi_spec == spec & ecozone == ez & juris_id == admin,][1]
  params7 <- table7[canfi_spec == spec & ecozone == ez & juris_id == admin,][1]
  
  ###### DC 2025-01-30 All this section is to convert vol2biomass, but we want AGB2pools,
  ###### make sure with Celine these are the correct parameters/functions to do so.
  
  # Equations are numbered following the flowchart of the biomass model application in
  # Boudewyn et al. 2007 p7 (Fig3)
  # eq1 returns the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in table3
  # eq1 <- b_m(params3, oneCurve$MerchVolume)
  # eq2 returns a two column matrix giving the biomass of the non-merch sized
  # trees (b_n) and b_nm which is the sum of the total stem wood biomass of merch size
  # live plus, the stem wood live of non merch-sized trees, given the total
  # stem wood biomass per ha of live merch size trees (in tonnes/ha)
  # eq2 <- nmfac(params4, eq1 = eq1, vol = oneCurve$MerchVolume)
  # eq3 is for biomass of the saplings, the smallest of the non-merch trees. The
  # non-merch biomass from eq2, is needed. eq3 returns b_s, stem wood biomass of
  # live sapling-sized trees in tonnes/ha
  # eq3 <- sapfac(params5, eq2 = eq2, vol = oneCurve$MerchVolume)
  # eq3[which(is.na(eq3))] <- 0
  # middle box flowchart3: total stem wood biomass (tonnes) /ha for all live trees
  
  # totalStemWood <- eq1 + eq2[,1] + eq3
  # totalStemWood[which(is.nan(totalStemWood))] <- NA
  
  # # calculate the 4 proportions that should be returned: proportion for
  # # stemwood, prop for bark, prop for branches, and prop for foliage.
  
  # translating this into biomass values for the carbon pools
  # totMerch <- eq1
  # totTree <- totalStemWood / pVect[, 1]
  #################
  
  # get the proportions of each pool
  pVect <- CBMutils::biomProp(table6 = params6, table7 = params7, vol = oneCurve$B)
  
  totTree <-  oneCurve$B
  totalStemWood <- totTree * pVect[, 1]
  
  ##TODO
  # find actual data on the proportion of totTree that is merch
  # Problem: CBM currently uses "merch" and "other" as C-pools. In these
  # equations (this function that matches the Boudewyn et al 2007 workflow),
  # totalStemwood is the sum of totMerch (eq1), b_n (eq2[,1] - stem wood biomass
  # of live, nonmerchantable-sized trees) and b_s (eq3 - stem wood biomass of
  # live, sapling-sized trees). The "merch" and the "other" C-pool requires us
  # to know the proportion of totalStemWood that is "merch" and "other"
  ##### IMPORTANT HARD CODING INFORMATION #######
  ## current fix: using the same parameters as FORCS (Forest Carbon Succession
  ## Extension V3.1). Eq 1 on p20 is PropStem = a *(1-b^Age) where a is 0.7546
  ## and b is 0.983. FORCS also sets a minimum merchantable age per species.
  ## Because we are in the RIA, I am setting that at 15. This needs to be a
  ## parameter either from LandR or set by the user (by provinces by species? -
  ## this is usually a diamter not an age)
  
  ### HARD CODED minimum merchantable age, a, b
  minMerchAge <-  15
  a <- 0.7546
  b <- 0.983
  
  # if age < MinMerchAge, the propMerch is 0, otherwise use FORCS, until we find actual data.
  propMerch <- (oneCurve$age >= minMerchAge) * a * (1-b^oneCurve$age)
  
  totMerch <- propMerch * totalStemWood
  # otherStemWood is everything that is not totMerch
  otherStemWood <- totalStemWood - totMerch
  
  bark <- totTree * pVect[, 2]
  branch <- totTree * pVect[, 3]
  fol <- totTree * pVect[, 4]
  other <- branch + bark + otherStemWood
  biomCumulative <- as.matrix(cbind(totMerch,fol,other))
  return(biomCumulative)
}

