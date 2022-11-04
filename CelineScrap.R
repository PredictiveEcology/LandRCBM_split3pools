##Celine's scrapt to make it work before putting it in a module

## Need packages?
###running up to line 39 in global

##############################################################
# 1. This is all input data - expectsInputs() and .inputObject
    ## Read in two parameter tables
    table6 <- prepInputs(url = "https://drive.google.com/file/d/1gvtV-LKBNbqD7hmlL4X0i40P3du75oJc/view?usp=sharing",
                         fun = "data.table::fread",
                                         destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                                         #purge = 7,
                                         filename2 = "appendix2_table6_tb.csv")
    table7 <- prepInputs(url = "https://drive.google.com/file/d/16nQgTGW2p_IYF_Oavcc7WbWbgWl5uywt/view?usp=sharing",
                         fun = "data.table::fread",
                         destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                         #purge = 7,
                         filename2 = "appendix2_table7_tb.csv")

    # this gives us the link to the spatialUnitID - CBM consctruct that crosses the
    # AdminBoundaryID and EcoBoundary which are used for parameter matching

    cbmAdmin <-  prepInputs(url = "https://drive.google.com/file/d/1NwVPqMho8fOcf9mVVzm5SlfDbbyQmQlh/view?usp=sharing",
                                fun = "data.table::fread",
                                destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                                #purge = 7,
                                filename2 = "cbmAdmin.csv")

    ## we need the studyArea
    RIArtm <- Cache(prepInputs, url = "https://drive.google.com/file/d/1h7gK44g64dwcoqhij24F2K54hs5e35Ci/view?usp=sharing",
                         destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs")


    ### ecozones
    ###Line below failed
    #installGithubPackage("PredictiveEcology/CBMutils@development")

    ecozone <- Cache(CBMutils::prepInputsEcozones, url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                     dPath =  "C:/Celine/github/LandRCBM_split3pools/inputs",
                     rasterToMatch = RIArtm)

    ## canfi_species list
    ##TODO add these canfi species codes to LandR::sppEquivalencies_CA

    canfi_species <- Cache(prepInputs, url = "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo",
                                fun = "data.table::fread",
                                destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                                filename2 = "canfi_species.csv")

    ## these two next tables will be coming from the Yield module
    ## this one is the actual yields
    CBM_AGB <- Cache(prepInputs, url = "https://drive.google.com/file/d/1xxfG-8ZJKPlO5HguHpVtqio5TXdcRGy7",
                                fun = "data.table::fread",
                                destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                                filename2 = "CBM_AGB.csv")

    ## not sure why that extra column is added
    CBM_AGB <- CBM_AGB[,2:6]

    ## this is the species matching the Sp1 Sp2 and Sp3 of the previous table
    CBM_speciesCodes <- Cache(prepInputs, url = "https://drive.google.com/file/d/1sVsDoT1E-CDgo2hnCU2pgqV6PpVic2Fe",
                          fun = "data.table::fread",
                          destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                          filename2 = "CBM_speciesCodes.csv",
                          useCache = TRUE
                          )
    ## I have not used cache and it still got cached?? Why? because it is big?
    # Loading object into R
    # Saving large object (cacheId: 0732761348383c83) to Cache: 46.4 Mb Done!

    ## not sure why that extra column is added
    CBM_speciesCodes <- CBM_speciesCodes[,2:4]

    ### SOMETHING IS MISSING
    # there are multiple species that are Sp1 depending on the
    unique(CBM_speciesCodes[,c("speciesCode", "Sp")])

    ## lines 140 in Biomass_yieldTables.R switches the pixelGroup to an id
      # cds[, Sp := paste0("Sp", Sp1)]
      # cds <- cds[Sp %in% paste0("Sp", 1:numSpeciesKeep)] # keep only most abundant X species
      # cdSpeciesCodes <- cds[, list(Sp = Sp[1]), by = c("speciesCode", "pixelGroup")]
      # set(cds, NULL, c("Sp1", "maxB", "speciesCode"), NULL)
      #
      # # Convert to wide format
      # cdWide <- dcast(cds, pixelGroup + age ~ Sp, value.var = "B")
      # setnames(cdWide, old = "pixelGroup", new = "id")
    #### BECAUSE OF THE ABOVE I AM GOING TO USE "id" like pixelGroup

    ## need the location to match parameters - juris_id ecozone canfi_spec in both
    ## table6 and table 7

    ## read-in the pixelGroupMap
    pixelGroupMap <- Cache(prepInputs, url = "https://drive.google.com/file/d/1Pso52N9DFVJ46OFxtvtqrVX9VzOVhcf3",
                         destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                         rasterToMatch = RIArtm,
                         useCache = TRUE)

    ### need to match the pixel groups with the ecozones and juris_id
    ## lines of code to help
      # dtRasters <- as.data.table(cbind(growth_curve_component_id = gcIDRaster[], ages = ageRaster[], ecozones = ecozone[], spatialUnitID))
      # sim$allPixDT <- as.data.table(cbind(dtRasters, pixelIndex = 1:ncell(gcIDRaster), growth_curve_id = gcIDRaster[]))
      # setnames(sim$allPixDT,"SpatialUnitID", "spatial_unit_id")

    ##TODO
    ## Are pixelGroupMap and ecozone the same RTM?
    length(pixelGroupMap[])
    length(ecozone[])
    pixelGroupEco <- as.data.table(cbind(pixelIndex = 1:ncell(pixelGroupMap),
                                         pixelGroup = pixelGroupMap[],
                                         ecozone = ecozone[]))

    ### limiting this to our pixelGroups
    pixelGroupEco <- pixelGroupEco[, "pixelIndex" := NULL][pixelGroup %in% 1:3,]
    pixelGroupEco <- unique(pixelGroupEco)

    ### matching the ecozone to the admin, for now this is done by hand...
    ##TODO make the matching to the admin abreviation automatic.
    juris_id <- cbmAdmin[(EcoBoundaryID %in% pixelGroupEco$ecozone) & abreviation == "BC",]$abreviation

    pixelGroupEco[, juris_id := juris_id]


    ###################################### Make up my data from from Yield module


    setnames(CBM_AGB,"id","pixelGroup")
    CBM_AGB <- CBM_AGB[pixelGroup %in% 1:3,]

    cbm1 <- melt.data.table(CBM_AGB, id.vars = c("pixelGroup", "age"), variable.name = "Sp", value.name = "B")
    keycols1 = c("pixelGroup", "Sp")
    setkeyv(cbm1, keycols1)
    setkeyv(CBM_speciesCodes, keycols1)
    CBM_yieldOut <- merge(cbm1, CBM_speciesCodes, by = c("pixelGroup", "Sp"))
    CBM_yieldOut <- CBM_yieldOut[,-"Sp"]

    #### I think this is the table we need out of the Yield module ###################
    # > CBM_yieldOut
    # pixelGroup age  B speciesCode
    #         1   0  1    Betu_pap
    #         1   1  2    Betu_pap
    #         1   2  4    Betu_pap
    #         1   3  8    Betu_pap
    #         1   4 14    Betu_pap
    # ---
    #         3 249  2    Pice_mar
    #         3 250  1    Pice_mar
    #         3 251  1    Pice_mar
    #         3 252  1    Pice_mar
    #         3 253  1    Pice_mar


# 2. Operations to clean up data before matching species, jurisdiction and ecozone

    # Match the multimomial logit parameters (table6) and their caps (table7)
    # with the pixelGroup and speciesCode in CBM_yieldOut.
    # CBM_yieldOut$pixelGroup gives us the location. Location let's us figure
    # out which ecozone, and admin. These will be used to match with juris_id
    # (abreviation - can do this using cbmAdmin or a raster) and ecozone. The
    # canfi_species have numbers which we need to match with the parameters.


    ##TODO
    # might have to do that by hand for now and maybe add the canfi_species
    # numbers to the LandR::sppEquivalencies_CA

    ## just working with what we have
    matchCanfi <- as.data.table(cbind(speciesCode = unique(CBM_yieldOut$speciesCode),
                                      canfi_species = c(1303, 105, 101)))

    CBM_yieldOut <- merge(CBM_yieldOut, matchCanfi, by = "speciesCode")

    ##TODO
    #### NOTE SURE IS THIS IS NEEDED. I THINK IT IS, TO MATCH SCOTT'S MORKEN's ID SCHEMA
    # there can be more than one cohort in a pixelGroup - more than one cohort
    # on a pixel. So, we need a unique identifier for each curve on each pixel.
    # I am therefore adding a column which in this case will be the same as the
    # pixelGroup

    CBM_yieldOut[, id := pixelGroup]

    # adding ecozone column
    allInfoAGBin <- merge(CBM_yieldOut,pixelGroupEco, by = "pixelGroup")

##############################################################
# END - 1. This is all input data - expectsInputs() and .inputObject


##########################################################
    #2. START processing curves from AGB to 3 pools

    #2.1 Calculating the cumPools

    ### three functions are involved:
    # - cumPoolsCreate
    # - convertM3biom
    # - biomProp

    # the cumPoolsCreate and convertM3biom are different enough to warrant new
    # functions but we should be able to use biomProp

    ##testing with 3 pixelGroups
    # > unique(allInfoAGBin[,.(speciesCode,pixelGroup)])
    # speciesCode pixelGroup
    # 1:    Betu_pap          1
    # 2:    Betu_pap          2
    # 3:    Betu_pap          3
    # 4:    Pice_gla          2
    # 5:    Pice_mar          2
    # 6:    Pice_mar          3


    ### Error - I think it is in the counter 1:NROW part
    # DT[,.(V4.Sum=sum(V4)),
    #    by=V1][
    # allInfoAGBin[,.(B.sum = sum(B)), by = c("pixelGroup", "speciesCode")]
    # allInfoAGBin[,.(B.sum = sum(B)), by = "pixelGroup"]
    # allInfoAGBin[,.(B.sum = sum(B)), by = "speciesCode"]


    cumPools <- cumPoolsCreateAGB(allInfoAGBin, table6, table7)

    # problem###
    dim(cumPools)
    #1872    6
    dim(allInfoAGBin)
    #1878    8

    cbmAboveGroundPoolColNames <- "totMerch|fol|other"
    colNames <- grep(cbmAboveGroundPoolColNames, colnames(cumPools), value = TRUE)

    #2.2 MAKE SURE THE PROVIDED CURVES ARE ANNUAL
    ### if not, we need to extrapolate to make them annual
    minAgeId <- cumPools[,.(minAge = max(0, min(age) - 1)), by = "id"]
    fill0s <- minAgeId[,.(age = seq(from = 0, to = minAge, by = 1)), by = "id"]
    # might not need this
    length0s <- fill0s[,.(toMinAge = length(age)), by = "id"]
    # these are going to be 0s
    carbonVars <- data.table(id = unique(fill0s$id),
                             totMerch = 0,
                             fol = 0,
                             other = 0 )
    ## not 7cols, that was for CBM_VOl2biom, we have 6cols
    fiveOf7cols <- fill0s[carbonVars, on = "id"]

    otherVars <- cumPools[,.(pixelGroup = unique(pixelGroup)), by = "id"]
    add0s <- fiveOf7cols[otherVars, on = "id"]
    cumPoolsRaw <- rbind(cumPools,add0s)
    set(cumPoolsRaw, NULL, "age", as.numeric(cumPoolsRaw$age))
    setorderv(cumPoolsRaw, c("id", "age"))

    # 2.3 Plot the curves that are directly out of the Boudewyn-translation
    # Usually, these need to be, at a minimum, smoothed out.
    figPath <- file.path(getwd(),"figures") #file.path(modulePath(sim), currentModule(sim), "figures")

    # plotting and save the plots of the raw-translation in the sim$ don't really
    # need this b/c the next use of m3ToBiomPlots fnct plots all 6 curves, 3
    # raw-translation and 3-smoothed curves resulting from the Chapman-Richards
    # parameter finding in the cumPoolsSmooth fnct. Leaving these lines here as
    # exploration tools.
    # if (!is.na(P(sim)$.plotInitialTime))
    # sim$plotsRawCumulativeBiomass <- Cache(m3ToBiomPlots, inc = cumPoolsRaw,
    #                                        path = figPath,
    #                                        filenameBase = "rawCumBiomass_")

    # Fixing of non-smooth curves

    cumPoolsClean <- Cache(cumPoolsSmooth, cumPoolsRaw)

    # a[, totMerch := totMerchNew]
    if (!is.na(P(sim)$.plotInitialTime))
      figs <- Cache(m3ToBiomPlots, inc = cumPoolsClean,
                    path = figPath,
                    filenameBase = "cumPools_smoothed_postChapmanRichards")

    set(cumPoolsClean, NULL, colNames, NULL)
    colNamesNew <- grep(cbmAboveGroundPoolColNames, colnames(cumPoolsClean), value = TRUE)
    setnames(cumPoolsClean, old = colNamesNew, new = colNames)














