defineModule(sim, list(
  name = "LandRCBM_split3pools",
  description = paste("Takes total aboveground biomass provided by LandR and divides",
                      "it into the 3 required CBM pools."),
  keywords = "",
  authors = c(
    person("Celine", "Boisvenue", email = "cboivenue@gmail.com", role = c("aut", "cre")),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(LandRCBM_split3pools = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandRCBM_split3pools.Rmd"), ## same file
  reqdPkgs = list("data.table", "ggplot2", "terra",
                  "PredictiveEcology/CBMutils@development (>= 0.0.7.9006)",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9003)"),
  parameters = bindrows(
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter("numPlots", "integer", 10, NA, NA,
                    "When plotting the yield curves, this is how many unique pixel groups will ",
                    "be randomly selected and plotted"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    ## NOTE: the required parameter tables are not yet hosted on the NFIS (Oct., 2022).
    ## The links "in the examples"table6" and "table7" below are for the
    ## parameters in the equations using "volume" instead of "total tree biomass"
    ## as a dependent variable. For now these two are replaced by a table
    ## containing both the parameters and the caps for BC provided in an email to
    ## cboivenue by Paul Boudewyn.
    expectsInput(
      objectName = "table6", objectClass = "data.frame",
      desc = c("Proportion model parameters similar to Boudewyn et al 2007,",
               "but recalculated using total biomass (metric tonnes of tree biomass/ha) instead of vol/ha"),
      sourceURL = "https://drive.google.com/file/d/1gvtV-LKBNbqD7hmlL4X0i40P3du75oJc"
      ## NOTE: current link is to a google drive but parameters will eventually
      ## be on the NFIS site as per the volume-based parameters. For now, I put
      ## a copy of the biomass-basedones provided by Paul Boudewyn on the (FOR-CAST) WBI/Carbon drive
    ),
    expectsInput(
      objectName = "table7", objectClass = "data.frame",
      desc = paste("Caps on proportion models similar to Boudewyn et al. 2007",
                   "but recalculated using total biomass (metric tonnes of tree biomass/ha)",
                   "instead of vol/ha"),
      sourceURL = "https://drive.google.com/file/d/16nQgTGW2p_IYF_Oavcc7WbWbgWl5uywt"
      ## NOTE: current link is to a google drive but parameters will eventually
      ## be on the NFIS site as per the volume-based parameters. For now, I put
      ## a copy of the biomass-basedones provided by Paul Boudewyn on the (FOR-CAST) WBI/Carbon drive
    ),
    expectsInput(
      objectName = "cbmAdmin", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries,",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"
    ),
    expectsInput(
      objectName = "ecozone", objectClass = "RasterLayer",
      desc = "Ecozones of Canada",
      sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
    ),
    expectsInput(
      objectName = "canfi_species",
      objectClass = "data.frame",
      desc = "File containing the possible species in the Boudewyn table",
      sourceURL = "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo"
    ),
    expectsInput(
      objectName = "spuRaster", objectClass = "SpatRaster",
      desc = "Raster has spatial units for each pixel",
      sourceURL = "https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed"
    ),
    expectsInput(
      ## TODO Yield module will be modified to provide required format
      objectName = "CBM_AGB", objectClass = "data.frame",
      desc = "",
      sourceURL = "https://drive.google.com/file/d/1xxfG-8ZJKPlO5HguHpVtqio5TXdcRGy7"
      ## TODO: table created in the Yield module, will eventually get it from the module,
      ##       for now, sitting in the WBI/Carbon/LandrCBM folder
    ),
    expectsInput(
      objectName = "CBM_speciesCodes", objectClass = "data.frame",
      desc = paste(""),
      sourceURL = "https://drive.google.com/file/d/1sVsDoT1E-CDgo2hnCU2pgqV6PpVic2Fe"
      ## TODO: table created in the Yield module, will eventually get it from the module,
      ##       for now, sitting in the WBI/Carbon/LandrCBM folder
    ),
    expectsInput(
      objectName = "pixelGroupMap", objectClass = "RasterLayer",
      desc = "PixelGroup map from LandR",
      sourceURL = "https://drive.google.com/file/d/1Pso52N9DFVJ46OFxtvtqrVX9VzOVhcf3"
    ),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "RasterLayer",
      desc = "template raster to use for simulations; defaults to RIA study area", ## TODO
      sourceURL = "https://drive.google.com/file/d/1h7gK44g64dwcoqhij24F2K54hs5e35Ci"
    ),
    expectInput(
      objectName = "cohortData", objectClass = "data.frame",
      desc = "Above ground biomass of cohorts in pixel groups",
      sourceURL = "" ## TODO
    )
  ),
  outputObjects = bindrows(
    createsOutput(objectName = "CBM_yieldOut",
                  objectClass = "data.frame",
                  desc = "AGB values by pixel/pixelGroup, cohort (species and age) will be provided by the Yield module"),
    createsOutput(objectName = "CBM_AGBplots",
                  objectClass = "plot",
                  desc = "Plot of the AGB values per cohort provided by the Yield module"),
    createsOutput(objectName = "cumPools",
                  objectClass = "data.frame",
                  desc = "Cumulative carbon in three pools, totMerch, fol, and other per cohort"),
    createsOutput(objectName = "pixelGroupEco",
                  objectClass = "data.table",
                  desc = "Data table made from the pixelGroupMap raster and the ecozone raster with pixel number added"),
    createsOutput(objectName = "allInfoAGBin",
                  objectClass = "data.table",
                  desc = "Data table with all cohort information necessary for coefficient matching and calculations"),
    createsOutput(objectName = "cumPoolsRaw",
                  objectClass = "data.table",
                  desc = "Data table with the cumulative carbon for all three pools per cohort."),
    createsOutput(objectName = "plotsRawCumulativeBiomass",
                  objectClass = "plots",
                  desc = "Plots of the cumPoolsRaw"),
    createsOutput(objectName = "rawIncPlots",
                  objectClass = "plots",
                  desc = "Plots of the increments in each of the three pools."),
    createsOutput(objectName = "incHalf",
                  objectClass = "matrix",
                  desc = "Matrix of the 1/2 increment for every age provided per cohort")
  )
))

doEvent.LandRCBM_split3pools = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # split yield tables into AGB pools
      sim <- SplitYieldTables(sim)
      
      # spit AGB of cohorts into pools 
      sim <- scheduleEvent(sim, start(sim), "scheduling", "annualIncrements")
    },
    annualIncrements = {
      
      # split AGB of cohorts into pools
      sim <- SplitCohortData(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, "scheduling", "annualIncrements")
      
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

SplitYieldTables <- function(sim) {
  
  ##############################################################################
  # 1. Matching species, jurisdiction and ecozone
  # Match the multimomial logit parameters (table6) and their caps (table7)
  # with the pixelGroup and speciesCode. pixelGroup gives us the location. 
  # Location let's us figure out which ecozone, and admin. These will be used to
  # match with juris_id (abreviation - can do this using cbmAdmin or a raster) 
  # and ecozone. The canfi_species have numbers which we need to match with the 
  # parameters.
  
  allInfoYieldTables <- matchCurveToCohort(
    CBM_speciesCodes = sim$CBM_speciesCodes,
    pixelGroupMap = sim$pixelGroupMap,
    spuRaster = sim$spuRaster,
    cbmAdmin = sim$cbmAdmin,
    sp_canfi = sim$canfi_species,
    cohortData = NULL
  )
  
  sim$allInfoYieldTables <- merge(CBM_AGB, allInfoYieldTables, allow.cartesian = TRUE)
  
  setnames(sim$allInfoYieldTables, c("abreviation", "EcoBoundaryID"), c("juris_id", "ecozone"))
  
  ##TODO Need to plot the incoming AGB values.
  # pixelGroupsToPlot <- unique(sim$allInfoAGBin$pixelGroup)
  # if(Par$numPlots < length(pixelGroupsToPlot)){
  #   pixelGroupsToPlot <- sample(pixelGroupsToPlot, size = Par$numPlots)
  # }
  # 
  # sim$AGBinPlot <- pltfn(allInfoAGBin = sim$allInfoAGBin, pixelGroupsToPlot = pixelGroupsToPlot)
  ##############################################################################
  #2. START processing curves from AGB to 3 pools
  
  #2.1 Calculating the cumPools
  
  ##TODO
  ## add new functions to CBMutils. Only two of the three functions needed to be
  ## modified to adapt to AGB instead of vol inputs. These are in the R/ of this
  ## module
  
  ### three functions are involved:
  # - cumPoolsCreateAGB.R (modified from cumPoolsCreate)
  # - convertM3biom (modified from cumPoolsCreate)
  # - biomProp (as is from CBMutils)
  
  ##testing done with 3 pixelGroups
  # > unique(allInfoAGBin[,.(speciesCode,pixelGroup)])
  # speciesCode pixelGroup
  # 1:    Betu_pap          1
  # 2:    Betu_pap          2
  # 3:    Betu_pap          3
  # 4:    Pice_gla          2
  # 5:    Pice_mar          2
  # 6:    Pice_mar          3
  
  sim$cumPools <- cumPoolsCreateAGB(allInfoAGBin = sim$allInfoYieldTables,
                                    table6 = sim$table6,
                                    table7 = sim$table7)
  
  cbmAboveGroundPoolColNames <- "totMerch|fol|other"
  colNames <- grep(cbmAboveGroundPoolColNames, colnames(sim$cumPools), value = TRUE)
  
  #2.2 MAKE SURE THE PROVIDED CURVES ARE ANNUAL (probably not needed for LandR
  #connection, but might be needed for future connection to other sources of
  #AGB).
  ### if not, we need to extrapolate to make them annual
  minAgeId <- sim$cumPools[,.(minAge = max(0, min(age) - 1)), by = "gcids"]
  fill0s <- minAgeId[,.(age = seq(from = 0, to = minAge, by = 1)), by = "gcids"]
  # might not need this
  # length0s <- fill0s[,.(toMinAge = length(age)), by = "gcids"]
  # these are going to be 0s
  carbonVars <- data.table(gcids = unique(fill0s$gcids),
                           totMerch = 0,
                           fol = 0,
                           other = 0 )
  ## not 7cols, that was for CBM_VOl2biom, we have 6cols
  fiveOf7cols <- fill0s[carbonVars, on = "gcids"]
  
  otherVars <- sim$cumPools[,.(pixelGroup = unique(pixelGroup)), by = "gcids"]
  add0s <- fiveOf7cols[otherVars, on = "gcids"]
  sim$cumPoolsRaw <- rbind(sim$cumPools,add0s)
  set(sim$cumPoolsRaw, NULL, "age", as.numeric(sim$cumPoolsRaw$age))
  setorderv(sim$cumPoolsRaw, c("gcids", "age"))
  
  # problem check: the difference between these two should only be the 0s that
  # got removed and the id columns
  # if(dim(cumPools)[1] != dim(sim$allInfoAGBin)){
  #   stop("There is a mismatch between the information that was given for translation and the results")
  # }
  
  # 2.3 Plot the curves that are directly out of the Boudewyn-translation
  # TODO
  # check that this is working. We only need to plot these when we are at the
  # beginning of a sim. Plotting the yearly translations will not be useful.
  # plotting and save the plots of the raw-translation
  ## plotting is off - maybe turn it on?
  if (!is.na(P(sim)$.plotInitialTime)){
    cumPoolsRawToPlot <- sim$cumPoolsRaw[pixelGroup %in% pixelGroupsToPlot]
    sim$plotsRawCumulativeBiomass <- m3ToBiomPlots(inc = sim$cumPoolsRaw,
                                                   id_col = c("gcids","pixelGroup"),
                                                   path = figurePath(sim),
                                                   filenameBase = "rawCumBiomass_")
  }
  
  # Some of these curves may still be wonky. But there is not much that can be
  # done unless we get better pool-splitting methods. The "matching" made in
  # Biomass_speciesParameters to the PSP makes this as good as the data we
  # have (PSPs).
  # Note: Fixing of non-smooth curves done in CBM_vol2biomass would not help
  # here. The smoothing to match PSP is done and cohort-level growth will only
  # match a Chapman-Richard form is there is only one cohort on the pixel.
  
  # 2.4 Calculating Increments
  cbmAboveGroundPoolColNames <- "totMerch|fol|other"
  colNames <- grep(cbmAboveGroundPoolColNames, colnames(sim$cumPools), value = TRUE)
  
  incCols <- c("incMerch", "incFol", "incOther")
  sim$cumPoolsRaw[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = colNames,
                  by = eval("gcids")]
  colsToUse33 <- c("age", "gcids", incCols)
  if (!is.na(P(sim)$.plotInitialTime))
    sim$rawIncPlots <- m3ToBiomPlots(inc = sim$cumPoolsRaw[, ..colsToUse33],
                                     path = figurePath(sim),
                                     title = "Increments merch fol other by gc id",
                                     filenameBase = "Increments")
  message(crayon::red("User: please inspect figures of the raw translation of your increments in: ",
                      figurePath(sim)))
  
  ## half the growth increments in tonnes of C/ha
  increments <- sim$cumPoolsRaw[,.(gcids, pixelGroup, age, incMerch, incFol, incOther)]
  
  sim$incHalf <- increments[, (colNames) := list(
    incMerch / 2,
    incFol / 2,
    incOther / 2
  )][, (incCols) := NULL]
  
  return(invisible(sim))
}

SplitCohortData <- function(sim) {
  allInfoCohortData <- matchCurveToCohort(
    CBM_speciesCodes = sim$CBM_speciesCodes,
    pixelGroupMap = sim$pixelGroupMap,
    spuRaster = sim$spuRaster,
    cbmAdmin = sim$cbmAdmin,
    sp_canfi = sim$canfi_species,
    cohortData = NULL
  )
  sim$allInfoCohortData <- merge(cohortData, allInfoYieldTables, allow.cartesian = TRUE)
}


.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- prepInputs(url = extractURL("rasterToMatch"),
                                    fun = "raster::raster",
                                    destinationPath = dPath,
                                    filename2 = "rtm.tif") ## TODO: confirm
  }
  
  # 1. NFIparams
  if (!suppliedElsewhere("table6", sim)) {
    sim$table6 <- prepInputs(url = extractURL("table6"),
                             fun = "data.table::fread",
                             destinationPath = dPath,
                             filename2 = "appendix2_table6_tb.csv")
  }
  
  if (!suppliedElsewhere("table7", sim)) {
    sim$table7 <- prepInputs(url = extractURL("table7"),
                             fun = "data.table::fread",
                             destinationPath = dPath,
                             filename2 = "appendix2_table7_tb.csv")
  }
  
  # 2. CBM and NFI admin
  if (!suppliedElsewhere("cbmAdmin", sim)) {
    sim$cbmAdmin <- prepInputs(url = extractURL("cbmAdmin"),
                               fun = "data.table::fread",
                               destinationPath = dPath,
                               filename2 = "cbmAdmin.csv")
  }
  
  if (!suppliedElsewhere("canfi_species", sim)) {
    sim$canfi_species <- prepInputs(url = extractURL("canfi_species"),
                                    fun = "data.table::fread",
                                    destinationPath = dPath,
                                    filename2 = "canfi_species.csv")
  }
  
  if (!suppliedElsewhere("spuRaster", sim)){
    
    if (!suppliedElsewhere("spuRasterURL", sim, where = "user")) message(
      "User has not supplied a spatial units raster ('spuRaster' or 'spuRasterURL'). ",
      "Default for Canada will be used.")
    
    spuSF <- prepInputs(
      destinationPath = inputPath(sim),
      url         = extractURL("spuRaster"),
      filename1   = "spUnit_Locator.zip",
      targetFile  = "spUnit_Locator.shp",
      alsoExtract = "similar",
      fun         = sf::st_read(targetFile, quiet = TRUE),
      projectTo   = sim$rasterToMatch,
      cropTo      = sim$rasterToMatch
    ) |> Cache()
    
    sim$spuRaster <- terra::rasterize(
      terra::vect(spuSF),
      sim$rasterToMatch,
      field = "spu_id"
    ) |> Cache()
  }
  
  # 3. Information from LandR
  ## these two next tables will be coming from the Yield module
  ## this one is the actual yields that are needed for the CBM spinup
  
  if (!suppliedElsewhere("CBM_AGB", sim)) {
    sim$CBM_AGB <- prepInputs(url = extractURL("CBM_AGB"),
                              fun = "data.table::fread",
                              destinationPath = dPath,
                              filename2 = "CBM_AGB.csv")
  }
  
  if (!suppliedElsewhere("CBM_speciesCodes", sim)) {
    sim$CBM_speciesCodes <- prepInputs(url = extractURL("CBM_speciesCodes"),
                                       fun = "data.table::fread",
                                       destinationPath = dPath,
                                       filename2 = "CBM_speciesCodes.csv")
  }
  
  ## pixel to pixelGroup map that gets updated annually
  if (!suppliedElsewhere("pixelGroupMap", sim))
    sim$pixelGroupMap <- prepInputs(url = extractURL("pixelGroupMap"),
                                    destinationPath = dPath,
                                    rasterToMatch = sim$rasterToMatch,
                                    useCache = TRUE)
  
  ## biomass per cohort and pixel group that gets updated annually
  if (!suppliedElsewhere("cohortData", sim))
    sim$cohortData <- prepInputs(url = extractURL("cohortData"),
                                 destinationPath = dPath,
                                 useCache = TRUE)

  return(invisible(sim))
}

pltfn <- function(allInfoAGBin, pixelGroupsToPlot) {
  id2 <- allInfoAGBin[pixelGroup %in% pixelGroupsToPlot]
  setnames(id2, "B", "AGB")
  sp <- unique(allInfoAGBin[pixelGroup %in% pixelGroupsToPlot]$speciesCode)
  gg <- ggplot(id2, aes(age, AGB, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~pixelGroup)
  return(invisible(gg))
}
