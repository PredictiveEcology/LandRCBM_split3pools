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
  reqdPkgs = list("PredictiveEcology/SpaDES.core@box", "PredictiveEcology/reproducible@AI", "data.table", "ggplot2", "terra",
                  "PredictiveEcology/CBMutils@development",
                  "PredictiveEcology/LandR@development"),
  parameters = bindrows(
    defineParameter("numCohortPlots", "integer", 3, NA, NA,
                    "When plotting the yield curves, this is how many unique cohorts per ",
                    "pixelGroup plotted."),
    defineParameter("numPixGroupPlots", "integer", 10, NA, NA,
                    "When plotting the yield curves, this is how many unique pixel groups will ",
                    "be randomly selected and plotted."),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
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
    expectsInput(
      objectName = "canfi_species",
      objectClass = "data.frame",
      desc = "File containing the possible species in the Boudewyn table",
      sourceURL = "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo"
    ),
    expectsInput(
      objectName = "cbmAdmin", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries,",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"
    ),    
    expectsInput(
      ## TODO Yield module will be modified to provide required format
      objectName = "CBM_AGB", objectClass = "data.frame",
      desc = "",
      sourceURL = "https://drive.google.com/file/d/1ANziym1UWZyDHPoVdRR5WHwrNw6b9Ms7/view?usp=sharing"
      ## TODO: table created in the Yield module, will eventually get it from the module,
      ##       for now, sitting in the WBI/Carbon/LandrCBM folder
    ),
    expectsInput(
      objectName = "CBM_speciesCodes", objectClass = "data.frame",
      desc = paste(""),
      sourceURL = "https://drive.google.com/file/d/1GunHO8hN54WeMVgCh-MgWvxRaguPYuMJ/view?usp=drive_link"
      ## TODO: table created in the Yield module, will eventually get it from the module,
      ##       for now, sitting in the WBI/Carbon/LandrCBM folder
    ),
    expectsInput(
      objectName = "cohortData", objectClass = "data.frame",
      desc = "Above ground biomass of cohorts in pixel groups",
      sourceURL = "https://drive.google.com/file/d/17VSBMgnvJtcDYgeaLXZWUA36DbsnLDyF/view?usp=drive_link" 
      ## TODO: table created in the Yield module, will eventually get it from the module,
      ##       for now, sitting in the WBI/Carbon/LandrCBM folder
    ),
    expectsInput(
      objectName = "pixelGroupMap", objectClass = "SpatRaster",
      desc = "PixelGroup map from LandR",
      sourceURL = "https://drive.google.com/file/d/18FuRnQHPgY9-K3jkhKKQFTpGGbs0PmOT/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "SpatRaster",
      desc = "template raster to use for simulations; defaults to RIA study area", ## TODO
      sourceURL = "https://drive.google.com/file/d/18FuRnQHPgY9-K3jkhKKQFTpGGbs0PmOT/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "spuRaster", objectClass = "SpatRaster",
      desc = "Raster has spatial units for each pixel",
      sourceURL = "https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed"
    ),
    ## NOTE: the required parameter tables are not yet hosted on the NFIS (Oct., 2022).
    ## The links "in the examples"table6" and "table7" below are for the
    ## parameters in the equations using "volume" instead of "total tree biomass"
    ## as a dependent variable. For now these two are replaced by a table
    ## containing both the parameters and the caps for BC provided in an email to
    ## cboivenue by Paul Boudewyn.
    expectsInput(
      objectName = "table6", objectClass = "data.frame",
      desc = paste("Proportion model parameters similar to Boudewyn et al 2007,",
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
    )
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "allInfoCohortData",
      objectClass = "data.table",
      desc = paste("Above ground biomass (in tonnes of g/m2) of each cohort per",
                   "pixelGroup provided by LandR with additionnal information to",
                   "match with Boudewyn et al. equations. Gets updated each timestep")
    ),
    createsOutput(
      objectName = "allInfoYieldTables",
      objectClass = "data.table",
      desc = paste("Yield table provided by the biomass_yieldTables module with",
                   "additionnal information to match with Boudewyn et al. equations.")
    ),
    createsOutput(
      objectName = "cohortPools",
      objectClass = "data.table",
      desc = "Cumulative biomass in each aboveground pool for each cohort per pixelGroup."
    ),
    createsOutput(
      objectName = "cumPools",
      objectClass = "data.table",
      desc = paste("Cumulative biomass in each aboveground biomass pool for each",
                   "yield curve (in tonnes of carbon/ha).")
    ),
    # Do we need cumPools if we have cumPoolsRaw?
    createsOutput(
      objectName = "cumPoolsRaw",
      objectClass = "data.table",
      desc = "Same as cumPools with additionnal lines for age 0 of each cohort."
    ),
    createsOutput(
      objectName = "increments",
      objectClass = "data.table",
      desc = "Increments for each yield curve (in tonnes of carbon/ha)."
    ),
    createsOutput(
      objectName = "rawIncPlots",
      objectClass = "ggplot",
      desc = "Plot of the increments for yield curves of randomly selected pixelGroup"
    ),
    createsOutput(
      objectName = "yieldCurvePlots",
      objectClass = "ggplot",
      desc = paste("Plot of the yield curves of randomly selected pixelGroup provided",
                   "by the biomass_yieldTables module")
    ),
    # Same as yieldCurvePlots, but curve are stacked to visualize total biomass
    createsOutput(
      objectName = "yieldCurvePlotsStacked",
      objectClass = "ggplot",
      desc = paste("Plot of the AGB values per cohort of randomly selected pixelGroup",
                   "provided by the biomass_yieldTables module")
    ),
    createsOutput(
      objectName = "yieldCurvePoolPlots",
      objectClass = "ggplot",
      desc = paste("Plot of the cumulative biomass of the three AGB pools for randomly",
                   "selected pixelGroup.")
    )
  )
))

doEvent.LandRCBM_split3pools = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # plot the yield tables
      sim <- PlotYieldTables(sim)
      
      # split yield tables into AGB pools
      sim <- SplitYieldTables(sim)
      
      # plot the yield tables with pools seperated
      sim <- PlotYieldTablesPools(sim)
      
      # spit AGB of cohorts into pools 
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools","annualIncrements")
    },
    annualIncrements = {
      
      # split AGB of cohorts into pools
      sim <- SplitCohortData(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, "LandRCBM_split3pools", "annualIncrements")
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

PlotYieldTables <- function(sim){
  nPixGroups <- length(unique(sim$CBM_speciesCodes$pixelGroup))
  nPlots <- P(sim)$numPixGroupPlots
  if (nPlots <= 0){
    stop("numPlots needs to be a positive integer")
  } else if (nPlots > nPixGroups) {
    message("numPixGroupPlots is greater than the number of pixel groups, ",
            "plotting all pixelgroups.")
    nPlots <- nPixGroups
  } 
  pixGroupToPlot <- sample(unique(sim$CBM_speciesCodes$pixelGroup), nPlots)
  # numCohortPlots: we want to plot this number of cohorts per pixelGroup, 
  # keeping species that reach the highest biomass
  max_B_per_cohort <- sim$CBM_AGB[, .(max_B = max(B)), by = .(pixelGroup, cohort_id)]
  top_cohort_per_pixel <- max_B_per_cohort[, {
    # Order by max_B in descending order and keep the top 3, or all if fewer than 3
    .SD[order(-max_B)][1:min(P(sim)$numCohortPlots, .N)]
  }, by = pixelGroup]
  # dt for plotting.
  plot_dt <- sim$CBM_AGB[cohort_id %in% top_cohort_per_pixel$cohort_id]
  plot_dt <- plot_dt[pixelGroup %in% pixGroupToPlot]
  plot_dt <- merge(plot_dt, sim$CBM_speciesCodes, by = c("cohort_id", "pixelGroup"))
  
  # plot
  sim$yieldCurvePlots <- ggplot(plot_dt, aes(age, B, color = speciesCode)) + geom_line() + theme_bw() +
    facet_wrap(~pixelGroup)
  sim$yieldCurvePlotsStacked <- ggplot(plot_dt, aes(age, B, fill = speciesCode)) + geom_area(position = position_stack()) + theme_bw() +
    facet_wrap(~pixelGroup)
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
    canfi_species = sim$canfi_species,
    cohortData = NULL
  )
  
  sim$allInfoYieldTables <- merge(sim$CBM_AGB, allInfoYieldTables, allow.cartesian = TRUE)
  
  setnames(sim$allInfoYieldTables, c("abreviation", "EcoBoundaryID"), c("juris_id", "ecozone"))
  
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
  carbonVars <- data.table(gcids = unique(fill0s$gcids),
                           totMerch = 0,
                           fol = 0,
                           other = 0 )
  
  fiveOf7cols <- fill0s[carbonVars, on = "gcids"]
  
  otherVars <- sim$cumPools[,.(pixelGroup = unique(pixelGroup), species = unique(species)), by = "gcids"]
  add0s <- fiveOf7cols[otherVars, on = "gcids"]
  sim$cumPoolsRaw <- rbind(sim$cumPools,add0s)
  set(sim$cumPoolsRaw, NULL, "age", as.numeric(sim$cumPoolsRaw$age))
  setorderv(sim$cumPoolsRaw, c("gcids", "age"))
  
  # 2.4 Calculating Increments
  incCols <- c("incMerch", "incFol", "incOther")
  # This line calculates the first difference of each colNames, shifting it down 
  # by one row and filling the first entry with NA.
  sim$cumPoolsRaw[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = colNames,
                  by = eval("gcids")]
  colsToUse33 <- c("age", "gcids", incCols)
  if (!is.na(P(sim)$.plotInitialTime)){
    plot_dt <- sim$cumPoolsRaw[gcids %in% unique(sim$yieldCurvePlots$data$cohort_id)]
    sim$rawIncPlots <- m3ToBiomPlots(inc = plot_dt[, ..colsToUse33],
                                     path = figurePath(sim),
                                     title = "Increments merch fol other by gc id",
                                     filenameBase = "Increments")
    message(crayon::red("User: please inspect figures of the raw translation of your increments in: ",
                        figurePath(sim)))
  }
  sim$increments <- sim$cumPoolsRaw[,.(gcids, pixelGroup, age, incMerch, incFol, incOther)]
  
  
  ### DC: I believe this is not necessary since we already divided by two in the 
  ### cumPoolsCreateAGB function.
  
  # sim$incHalf <- increments[, (colNames) := list(
  #   incMerch / 2,
  #   incFol / 2,
  #   incOther / 2
  # )][, (incCols) := NULL]
  
  return(invisible(sim))
}


# 2.3 Plot the curves that are directly out of the Boudewyn-translation
# TODO
# check that this is working. We only need to plot these when we are at the
# beginning of a sim. Plotting the yearly translations will not be useful.
# plotting and save the plots of the raw-translation
## plotting is off - maybe turn it on?
# if (!is.na(P(sim)$.plotInitialTime)){
#   cumPoolsRawToPlot <- sim$cumPoolsRaw[pixelGroup %in% pixelGroupsToPlot]
#   sim$plotsRawCumulativeBiomass <- m3ToBiomPlots(inc = sim$cumPoolsRaw,
#                                                  id_col = c("gcids","pixelGroup"),
#                                                  path = figurePath(sim),
#                                                  filenameBase = "rawCumBiomass_")
# }
# Some of these curves may still be wonky. But there is not much that can be
# done unless we get better pool-splitting methods. The "matching" made in
# Biomass_speciesParameters to the PSP makes this as good as the data we
# have (PSPs).
# Note: Fixing of non-smooth curves done in CBM_vol2biomass would not help
# here. The smoothing to match PSP is done and cohort-level growth will only
# match a Chapman-Richard form is there is only one cohort on the pixel.
PlotYieldTablesPools <- function(sim){
  #### HERE: What if the pixel groups cross across NFI spatial units?! The same
  #### Yield curve would produce different pools
  cohortToPlot <- unique(sim$yieldCurvePlots$data$cohort_id)
  
  plot_dt <- sim$cumPoolsRaw[gcids %in% cohortToPlot]
  
  plot_dt <- melt(
    plot_dt, 
    id.vars = c("gcids", "species", "pixelGroup", "age"),
    measure.vars = c("totMerch", "fol", "other"),
    variable.name = "pool",
    value.name = "B"
  )
  # plot
  sim$yieldCurvePoolPlots <- ggplot(plot_dt, aes(age, B, fill = pool)) + 
    geom_area(position = position_stack()) + 
    theme_bw() +
    facet_grid(species~pixelGroup) +
    theme(panel.background = element_rect(fill = "white", color = NA),
          panel.grid = element_blank())
  return(invisible(sim))
  
}

SplitCohortData <- function(sim) {
  sim$allInfoCohortData <- matchCurveToCohort(
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    spuRaster = sim$spuRaster,
    cbmAdmin = sim$cbmAdmin,
    canfi_species = sim$canfi_species,
    CBM_speciesCodes = NULL
  )
  setnames(sim$allInfoCohortData, c("abreviation", "EcoBoundaryID"), c("juris_id", "ecozone"))
  
  sim$cohortPools <- cumPoolsCreateAGB(allInfoAGBin = sim$allInfoCohortData,
                                       table6 = sim$table6,
                                       table7 = sim$table7)
  
  #### TODO: there is probably ages (e.g., age = 0) for which we loss data.
  return(invisible(sim))
  
}


.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- prepInputs(
      url = extractURL("rasterToMatch"),
      fun = "terra::rast",
      destinationPath = inputPath(sim),
      overwrite = TRUE
    ) |> Cache() ## TODO: confirm
  }
  
  # 1. NFIparams
  if (!suppliedElsewhere("table6", sim)) {
    sim$table6 <- prepInputs(url = extractURL("table6"),
                             fun = "data.table::fread",
                             destinationPath = inputPath(sim),
                             filename2 = "appendix2_table6_tb.csv")
  }
  
  if (!suppliedElsewhere("table7", sim)) {
    sim$table7 <- prepInputs(url = extractURL("table7"),
                             fun = "data.table::fread",
                             destinationPath = inputPath(sim),
                             filename2 = "appendix2_table7_tb.csv")
  }
  
  # 2. CBM and NFI admin
  if (!suppliedElsewhere("cbmAdmin", sim)) {
    sim$cbmAdmin <- prepInputs(url = extractURL("cbmAdmin"),
                               fun = "data.table::fread",
                               destinationPath = inputPath(sim),
                               filename2 = "cbmAdmin.csv")
  }
  
  if (!suppliedElsewhere("canfi_species", sim)) {
    sim$canfi_species <- prepInputs(url = extractURL("canfi_species"),
                                    fun = "data.table::fread",
                                    destinationPath = inputPath(sim),
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
      alsoExtract = "similar",
      fun         = "sf::st_read",
      to = sim$rasterToMatch,
      overwrite      = TRUE
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
                              destinationPath = inputPath(sim),
                              filename2 = "CBM_AGB.csv")
  }
  
  if (!suppliedElsewhere("CBM_speciesCodes", sim)) {
    sim$CBM_speciesCodes <- prepInputs(url = extractURL("CBM_speciesCodes"),
                                       fun = "data.table::fread",
                                       destinationPath = inputPath(sim),
                                       filename2 = "CBM_speciesCodes.csv")
  }
  
  ## pixel to pixelGroup map that gets updated annually
  if (!suppliedElsewhere("pixelGroupMap", sim))
    sim$pixelGroupMap <- prepInputs(url = extractURL("pixelGroupMap"),
                                    destinationPath = inputPath(sim),
                                    fun = "terra::rast",
                                    rasterToMatch = sim$rasterToMatch,
                                    useCache = TRUE,
                                    overwrite = TRUE)
  
  ## biomass per cohort and pixel group that gets updated annually
  if (!suppliedElsewhere("cohortData", sim))
    sim$cohortData <- prepInputs(url = extractURL("cohortData"),
                                 destinationPath = inputPath(sim),
                                 fun = "data.table::fread",
                                 overwrite = TRUE,
                                 filename2 = "cohortData.csv")
  
  return(invisible(sim))
}
