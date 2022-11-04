convertAGB2pools <- function(oneCurve, table6, table7){

  spec <- as.integer(unique(oneCurve$canfi_species))

  ## might have to put in a loop here for each ecozone?
  ez <- unique(oneCurve$ecozone)
  admin <- unique(oneCurve$juris_id)


  table6 <- table6[juris_id == admin,]
  table7 <- table7[juris_id == admin,]

  #gen <- unique(spsMatch[species == meta$species,]$genus)

  params6 <- table6[canfi_spec == spec & ecozone == ez,][1]
  params7 <- table7[canfi_spec == spec & ecozone == ez,][1]
  # Equations are numbered following the flowchart of the biomass model application in
  # Boudewyn et al. 2007 p7 (Fig3)
  # eq1 returns the total stem wood biomass in metric tonnes/ha, when you give it
  # the gross merchantable volume/ha. Parameters a and b are in table3
  #eq1 <- b_m(params3, oneCurve$MerchVolume)
  # eq2 returns a two column matrix giving the biomass of the non-merch sized
  # trees (b_n) and b_nm which is the sum of the total stem wood biomass of merch size
  # live plus, the stem wood live of non merch-sized trees, given the total
  # stem wood biomass per ha of live merch size trees (in tonnes/ha)
  #eq2 <- nmfac(params4, eq1 = eq1, vol = oneCurve$MerchVolume)
  # eq3 is for biomass of the saplings, the smallest of the non-merch trees. The
  # non-merch biomass from eq2, is needed. eq3 returns b_s, stem wood biomass of
  # live sapling-sized trees in tonnes/ha
  #eq3 <- sapfac(params5, eq2 = eq2, vol = oneCurve$MerchVolume)
  #eq3[which(is.na(eq3))] <- 0
  # middle box flowchart3: total stem wood biomass (tonnes) /ha for all live trees

  # totalStemWood <- eq1 + eq2[,1] + eq3
  # totalStemWood[which(is.nan(totalStemWood))] <- NA
  # # calculate the 4 proportions that should be returned: proportion for
  # # stemwood, prop for bark, prop for branches, and prop for foliage.
  pVect <- CBMutils::biomProp(table6 = params6, table7 = params7, vol = oneCurve$B)
  # translating this into biomass values for the carbon pools
  # totMerch <- eq1
  # totTree <- totalStemWood / pVect[, 1]

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
  ### HARD CODED minimum merchantable age
  minMerchAge <-  15
  oneCurve[, minMerchAge := minMerchAge]
  otherStemWood <- vector(length = length(oneCurve$age))
  totMerch<- vector(length = length(oneCurve$age))

  ##TODO
  ## show Celine how to write this in simple DT language please

  for(i in 1:nrow(oneCurve)){
        if(oneCurve$age[i] < oneCurve$minMerchAge[i]){
      # otherStemWood is everything that is not totMerch, so b_n (eq2[,1] - stem
      # wood biomass of live, nonmerchantable-sized trees) plus b_s (eq3 - stem
      # wood biomass of live, sapling-sized trees). If younger then merch age, all
      # the totalStemWood is other. If not, it is calculated the same way as in
      # FORCS, until we find actual data.
      otherStemWood[i] <- totalStemWood[i]
      totMerch[i] <- 0
    } else{
      ### HARD CODED a and b parameters
      propMerch <- 0.7546 * (1-0.983^oneCurve$age[i])
      totMerch[i] <- propMerch * totalStemWood[i]
      otherStemWood[i] <- totalStemWood[i] - totMerch[i]
    }

  }

  bark <- totTree * pVect[, 2]
  branch <- totTree * pVect[, 3]
  fol <- totTree * pVect[, 4]
  other <- branch + bark + otherStemWood #eq2[, 1] + eq3
  biomCumulative <- as.matrix(cbind(totMerch,fol,other))
  return(biomCumulative)
}

