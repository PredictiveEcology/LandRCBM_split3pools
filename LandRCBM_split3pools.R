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
  reqdPkgs = list("PredictiveEcology/SpaDES.core@box", "reproducible (>= 2.1.0)", "data.table", "ggplot2", "terra",
                  "SpaDES.tools (>= 1.0.0.9001)", "PredictiveEcology/CBMutils@development", "PredictiveEcology/LandR@development"),
  parameters = bindrows(
    defineParameter("numCohortPlots", "integer", 3L, NA, NA,
                    "When plotting the yield curves, this is how many unique cohorts per ",
                    "pixelGroup plotted."),
    defineParameter("numPixGroupPlots", "integer", 10L, NA, NA,
                    "When plotting the yield curves, this is how many unique pixel groups will ",
                    "be randomly selected and plotted."),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".plotMaps", "logical", TRUE, NA, NA,
                    desc = "Controls whether maps should be plotted or not. Set to `FALSE` if `P(sim)$.plots == NA`"),
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
      objectName = "yieldTables", objectClass = "data.frame",
      desc = "",
      sourceURL = "https://drive.google.com/file/d/1IP2MxX3QYnH-D9eO_q0VATwvKw72xvDi/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "yieldSpeciesCodes", objectClass = "data.frame",
      desc = paste(""),
      sourceURL = "https://drive.google.com/file/d/1cFvyTMQVdpSC3efnq5289iZ8jXNgAx-7/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "yieldPixelGroupMap", objectClass = "SpatRaster",
      desc = paste(""),
      sourceURL = "https://drive.google.com/file/d/1Tk2ubV3Pe87SliDSwwkoaNxbzn2nqojB/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "cohortData", objectClass = "data.frame",
      desc = "Above ground biomass (g/m^2) of cohorts in pixel groups",
      sourceURL = "https://drive.google.com/file/d/17VSBMgnvJtcDYgeaLXZWUA36DbsnLDyF/view?usp=drive_link" 
    ),
    expectsInput(
      objectName = "pixelGroupMap", objectClass = "SpatRaster",
      desc = "PixelGroup map from LandR",
      sourceURL = "https://drive.google.com/file/d/1zJRi968_FPD68fY6v_8-_kgAIOAYUyJ2/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "SpatRaster",
      desc = "template raster to use for simulations; defaults to RIA study area", ## TODO
      sourceURL = "https://drive.google.com/file/d/1zJRi968_FPD68fY6v_8-_kgAIOAYUyJ2/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "spuRaster", objectClass = "SpatRaster",
      desc = "Raster has spatial units for each pixel",
      sourceURL = "https://drive.google.com/file/d/1D3O0Uj-s_QEgMW7_X-NhVsEZdJ29FBed"
    ),
    expectsInput(
      objectName = "table6", objectClass = "data.frame",
      desc = paste("Proportion model parameters similar to Boudewyn et al 2007,",
                   "but recalculated using total biomass (metric tonnes of tree biomass/ha) instead of vol/ha"),
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv"
    ),
    expectsInput(
      objectName = "table7", objectClass = "data.frame",
      desc = paste("Caps on proportion models similar to Boudewyn et al. 2007",
                   "but recalculated using total biomass (metric tonnes of tree biomass/ha)",
                   "instead of vol/ha"),
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv"
    )
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "allInfoCohortData",
      objectClass = "data.table",
      desc = paste("Above ground biomass (metric tonnes of tree biomass/ha) of each cohort per",
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
      objectName = "annualIncrements",
      objectClass = "data.table",
      desc = paste("Increments for each cohort and pixelGroups (in tonnes of carbon/ha).")
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
    createsOutput(
      objectName = "incrementPixelGroupMap",
      objectClass = "spatRaster",
      desc = "Raster of the pixelGroups for the annual increments."
    ),
    createsOutput(
      objectName = "rawIncPlots",
      objectClass = "ggplot",
      desc = "Plot of the increments for yield curves of randomly selected pixelGroup"
    ),
    createsOutput(
      objectName = "summaryAGBPoolsSpecies",
      objectClass = "data.table",
      desc = "Biomass of each of the AG pools per species and year."
    ),
    createsOutput(
      objectName = "summaryAGBPoolsLandscape",
      objectClass = "ggplot",
      desc = "Sum biomass for each of the three pools on the landscape per year."
    ),
    createsOutput(
      objectName = "yieldCurvePlots",
      objectClass = "ggplot",
      desc = paste("Plot of the yield curves of randomly selected pixelGroup provided",
                   "by the biomass_yieldTables module")
    ),
    createsOutput(
      objectName = "yieldCurvePoolPlots",
      objectClass = "ggplot",
      desc = paste("Plot of the cumulative biomass of the three AGB pools for randomly",
                   "selected pixelGroup.")
    ),    
    createsOutput(
      objectName = "yieldIincrements",
      objectClass = "data.table",
      desc = "Increments for each yield curve (in tonnes of carbon/ha)."
    )
  )
))

doEvent.LandRCBM_split3pools = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # split yield tables into AGB pools
      sim <- SplitYieldTables(sim)
      
      # split AGB of cohorts into pools 
      sim <- scheduleEvent(sim, start(sim), eventPriority = 9, "LandRCBM_split3pools","annualIncrements")
      
      # summarize simulation 
      sim <- scheduleEvent(sim, start(sim), eventPriority = 10, "LandRCBM_split3pools","summarizeAGBPools")
      
      # plots
      if (anyPlotting(P(sim)$.plots)) {
        sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                             "LandRCBM_split3pools", "plotYC", eventPriority = 5)
        if (P(sim)$.plotMaps) {
          sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                               "LandRCBM_split3pools", "plotMaps", eventPriority = 11)
        }
        sim <- scheduleEvent(sim, end(sim),
                             "LandRCBM_split3pools", "plotSummaries", eventPriority = 12)
      }
    },
    plotYC = {
      # plot the yield tables
      sim <- PlotYieldTables(sim)
      
      # plot the yield tables with pools seperated
      sim <- PlotYieldTablesPools(sim)
    },
    annualIncrements = {
      
      # split AGB of cohorts into pools
      sim <- AnnualIncrements(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 9, "LandRCBM_split3pools", "annualIncrements")
    },
    summarizeAGBPools = {
      sumLandscape <- sim$cohortPools[, lapply(.SD, sum), .SDcols = c("totMerch", "fol", "other")]
      sumLandscape$year <- time(sim)[1]
      sumBySpecies <- sim$cohortPools[, lapply(.SD, sum), by = species, .SDcols = c("totMerch", "fol", "other")]
      sumBySpecies$year <- time(sim)[1]
      
      if (time(sim) == start(sim)){
        sim$summaryAGBPoolsLandscape <- sumLandscape
        sim$summaryAGBPoolsSpecies <- sumBySpecies
      } else {
        sim$summaryAGBPoolsLandscape <- rbind(
          sim$summaryAGBPoolsLandscape,
          sumLandscape
        )
        sim$summaryAGBPoolsSpecies <- rbind(
          sim$summaryAGBPoolsSpecies,
          sumBySpecies
        )
      }
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 10, "LandRCBM_split3pools", "summarizeAGBPools")
    },
    plotMaps = {
      # get the sum of each pool per pixelGroups
      poolSum <- sim$cohortPools[, lapply(.SD, sum), by = pixelGroup, .SDcols = c("totMerch", "fol", "other")]
      # rasterize
      totMerchRast <- rasterizeReduced(poolSum, sim$pixelGroupMap, newRasterCols = "totMerch", mapcode = "pixelGroup")
      folRast <- rasterizeReduced(poolSum, sim$pixelGroupMap, newRasterCols = "fol", mapcode = "pixelGroup")
      otherRast <- rasterizeReduced(poolSum, sim$pixelGroupMap, newRasterCols = "other", mapcode = "pixelGroup")
      
      # plot
      Plots(totMerchRast,
            fn = gg_agbpools,
            types = P(sim)$.plots,
            filename = paste0("totMerch", "_year_", round(time(sim))),
            title = paste("Total merchantable biomass", "year", round(time(sim))))
      Plots(folRast,
            fn = gg_agbpools,
            types = P(sim)$.plots,
            filename = paste0("fol", "_year_", round(time(sim))),
            title = paste("Foliage biomass", "year", round(time(sim))))
      Plots(otherRast,
            fn = gg_agbpools,
            types = P(sim)$.plots,
            filename = paste0("other", "_year_", round(time(sim))),
            title = paste("Other above ground biomass", "year", round(time(sim))))
      
      # map increments
      if (time(sim) != start(sim)){
        incrementSum  <- sim$annualIncrements[, lapply(.SD, sum), by = incrementPixelGroup, .SDcols = c("totMerch", "fol", "other")]
        # rasterize
        totMerchRast <- rasterizeReduced(incrementSum, sim$incrementPixelGroupMap, newRasterCols = "totMerch", mapcode = "incrementPixelGroup")
        folRast <- rasterizeReduced(incrementSum, sim$incrementPixelGroupMap, newRasterCols = "fol", mapcode = "incrementPixelGroup")
        otherRast <- rasterizeReduced(incrementSum, sim$incrementPixelGroupMap, newRasterCols = "other", mapcode = "incrementPixelGroup")
        
        # plot
        Plots(totMerchRast,
              fn = gg_agbpools,
              types = P(sim)$.plots,
              filename = paste0("totMerchInc", "_year_", round(time(sim))),
              title = paste("Total increment", "year", round(time(sim))))
        Plots(folRast,
              fn = gg_agbpools,
              types = P(sim)$.plots,
              filename = paste0("folInc", "_year_", round(time(sim))),
              title = paste("Foliage increment", "year", round(time(sim))))
        Plots(otherRast,
              fn = gg_agbpools,
              types = P(sim)$.plots,
              filename = paste0("otherInc", "_year_", round(time(sim))),
              title = paste("Other above increment", "year", round(time(sim))))
      }
      
      
      # schedule next maps
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                           "LandRCBM_split3pools", "plotMaps", eventPriority = 11)
    },
    plotSummaries = {
      if (time(sim) > start(sim)){
        # Landscape summary
        Plots(sim$summaryAGBPoolsLandscape,
              fn = gg_landscapesummary,
              types = P(sim)$.plots,
              filename = paste0("LandscapeAGBPoolSummary")
        )
        # Species summary
        Plots(sim$summaryAGBPoolsSpecies,
              fn = gg_speciessummary,
              types = P(sim)$.plots,
              filename = paste0("SpeciesAGBPoolSummary")
        )
      }
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

PlotYieldTables <- function(sim){
  nPixGroups <- length(unique(sim$yieldSpeciesCodes$yieldPixelGroup))
  nPlots <- P(sim)$numPixGroupPlots
  if (nPlots <= 0){
    stop("numPlots needs to be a positive integer")
  } else if (nPlots > nPixGroups) {
    message("numPixGroupPlots is greater than the number of pixel groups, ",
            "plotting all pixelgroups.")
    nPlots <- nPixGroups
  } 
  pixGroupToPlot <- sample(unique(sim$yieldSpeciesCodes$yieldPixelGroup), nPlots)
  # numCohortPlots: we want to plot this number of cohorts per yieldPixelGroup, 
  # keeping species that reach the highest biomass
  max_B_per_cohort <- sim$yieldTables[, .(max_B = max(B)), by = .(yieldPixelGroup, cohort_id)]
  top_cohort_per_pixel <- max_B_per_cohort[, {
    # Order by max_B in descending order and keep the top 3, or all if fewer than 3
    .SD[order(-max_B)][1:min(P(sim)$numCohortPlots, .N)]
  }, by = yieldPixelGroup]
  # dt for plotting.
  plot_dt <- sim$yieldTables[cohort_id %in% top_cohort_per_pixel$cohort_id]
  plot_dt <- plot_dt[yieldPixelGroup %in% pixGroupToPlot]
  plot_dt <- merge(plot_dt, sim$yieldSpeciesCodes, by = c("cohort_id", "yieldPixelGroup"))
  
  # convert g/m^2 into tonnes/ha
  plot_dt$B <- plot_dt$B/100
  mod$cohortPlotted <- unique(plot_dt$cohort_id)
  
  # plot
  Plots(plot_dt, 
        fn = gg_yieldCurves,
        types = P(sim)$.plots,
        filename = paste("yieldCurves"),
        title = paste("Yield curves for", nPlots, "randomly selected pixel groups")
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
    yieldSpeciesCodes = sim$yieldSpeciesCodes,
    pixelGroupMap = sim$yieldPixelGroupMap,
    spuRaster = sim$spuRaster,
    cbmAdmin = sim$cbmAdmin,
    canfi_species = sim$canfi_species,
    cohortData = NULL
  )
  sim$allInfoYieldTables <- merge(sim$yieldTables, allInfoYieldTables, allow.cartesian = TRUE)
  
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
  
  # convert m^2 into tonnes/ha
  sim$allInfoYieldTables$B <- sim$allInfoYieldTables$B/100
  
  cumPools <- cumPoolsCreateAGB(allInfoAGBin = sim$allInfoYieldTables,
                                table6 = sim$table6,
                                table7 = sim$table7,
                                pixGroupCol = "yieldPixelGroup")
  
  cbmAboveGroundPoolColNames <- "totMerch|fol|other"
  colNames <- grep(cbmAboveGroundPoolColNames, colnames(cumPools), value = TRUE)
  
  #2.2 MAKE SURE THE PROVIDED CURVES ARE ANNUAL (probably not needed for LandR
  #connection, but might be needed for future connection to other sources of
  #AGB).
  ### if not, we need to extrapolate to make them annual
  
  minAgeId <- cumPools[,.(minAge = max(0, min(age) - 1)), by = "gcids"]
  fill0s <- minAgeId[,.(age = seq(from = 0, to = minAge, by = 1)), by = "gcids"]
  carbonVars <- data.table(gcids = unique(fill0s$gcids),
                           totMerch = 0,
                           fol = 0,
                           other = 0 )
  
  fiveOf7cols <- fill0s[carbonVars, on = "gcids"]
  
  otherVars <- cumPools[,.(yieldPixelGroup = unique(yieldPixelGroup), species = unique(species)), by = "gcids"]
  add0s <- fiveOf7cols[otherVars, on = "gcids"]
  sim$cumPools <- rbind(cumPools,add0s)
  set(sim$cumPools, NULL, "age", as.numeric(sim$cumPools$age))
  setorderv(sim$cumPools, c("gcids", "age"))
  
  # 2.4 Calculating Increments
  incCols <- c("incMerch", "incFol", "incOther")
  # This line calculates the first difference of each colNames, shifting it down 
  # by one row and filling the first entry with NA.
  sim$cumPools[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = colNames,
               by = eval("gcids")]
  sim$yieldIncrements <- sim$cumPools[,.(gcids, yieldPixelGroup, age, species, incMerch, incFol, incOther)]
  
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
  cohortToPlot <- mod$cohortPlotted

  plot_dt <- sim$cumPools[gcids %in% cohortToPlot]
  plot_dt <- melt(
    plot_dt, 
    id.vars = c("gcids", "species", "yieldPixelGroup", "age"),
    measure.vars = c("totMerch", "fol", "other"),
    variable.name = "pool",
    value.name = "B"
  )
  # plot
  Plots(plot_dt, 
        fn = gg_yieldCurvesPools,
        types = P(sim)$.plots,
        filename = "yieldCurvePools",
        title = paste("Yield curves for", length(unique(plot_dt$yieldPixelGroup)), "randomly selected pixel groups")
  )
  
  plot_dt <- sim$yieldIncrements[gcids %in% mod$cohortPlotted]
  plot_dt <- melt(
    plot_dt, 
    id.vars = c("gcids", "species", "yieldPixelGroup", "age"),
    measure.vars = c("incMerch", "incFol", "incOther"),
    variable.name = "pool",
    value.name = "B"
  )
  Plots(plot_dt, 
        fn = gg_yieldCurvesPools,
        types = P(sim)$.plots,
        filename = "YieldIncrements",
        title = "Increments merch fol other by species and pixel groups"
        )
    message(crayon::red("User: please inspect figures of the raw translation of your increments in: ",
                        figurePath(sim)))
  
  return(invisible(sim))
}

AnnualIncrements <- function(sim){
  if (time(sim) != start(sim)){
    
    # 1. match pixelGroups of previous year and of this year to create pixelGroups
    # for increments
    sim$incrementPixelGroupMap <- rast(mod$pixelGroupMapTminus1)
    pixGr <- data.table(pixelGroupTminus1 = c(mod$pixelGroupMapTminus1[]),
                        pixelGroupT = c(sim$pixelGroupMap[])) |>
      setorder(pixelGroupT, na.last = TRUE)
    pixGr[, incrementPixelGroup := .GRP, by = .(pixelGroupTminus1, pixelGroupT)]
    sim$incrementPixelGroupMap[order(sim$pixelGroupMap[])] <- pixGr$incrementPixelGroup 
    sim$incrementPixelGroupMap <- mask(sim$incrementPixelGroupMap, mod$pixelGroupMapTminus1)
    pixGr <- na.omit(pixGr) |> unique()
    
    # 2. append the cohortPools of the previous year
    annualIncrements <- merge(pixGr, sim$cohortPools, by.x = "pixelGroupTminus1", by.y = "pixelGroup")
    setnames(annualIncrements, old = c("totMerch", "fol", "other"), new = c("totMerchTminus1", "folTminus1", "otherTminus1"))
  }
  
  # 3. split cohort data of current year
  sim$allInfoCohortData <- matchCurveToCohort(
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    spuRaster = sim$spuRaster,
    cbmAdmin = sim$cbmAdmin,
    canfi_species = sim$canfi_species,
    yieldSpeciesCodes = NULL
  )
  setnames(sim$allInfoCohortData, c("abreviation", "EcoBoundaryID"), c("juris_id", "ecozone"))
  
  # convert m^2 into tonnes/ha
  sim$allInfoCohortData$B <- sim$allInfoCohortData$B/100
  
  sim$cohortPools <- cumPoolsCreateAGB(allInfoAGBin = sim$allInfoCohortData,
                                       table6 = sim$table6,
                                       table7 = sim$table7)
  
  # 4. append the cohortPools of the previous year
  if (time(sim) != start(sim)){
    annualIncrements <- merge(annualIncrements, 
                              sim$cohortPools, 
                              by.x = c("pixelGroupT", "species"), 
                              by.y = c("pixelGroup", "species"),
                              allow.cartesian = TRUE)
    # adds biomass 0 when there is a new species in a pixelGroup
    setnafill(annualIncrements, fill = 0, 
              cols=c("totMerch", "fol", "other", "totMerchTminus1", "folTminus1", "otherTminus1"))
    
    # 5. take the difference
    annualIncrements[, `:=`(totMerch = totMerch - totMerchTminus1,
                            fol = fol - folTminus1,
                            other = other - otherTminus1)]
    
    sim$annualIncrements <- annualIncrements[,.(incrementPixelGroup, 
                                                species,
                                                age = age.y, 
                                                totMerch, 
                                                fol, 
                                                other)]    
  }
  mod$pixelGroupMapTminus1 <- sim$pixelGroupMap
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
                             filename2 = "appendix2_table6_tb.csv",
                             overwrite = TRUE)
  }
  
  if (!suppliedElsewhere("table7", sim)) {
    sim$table7 <- prepInputs(url = extractURL("table7"),
                             fun = "data.table::fread",
                             destinationPath = inputPath(sim),
                             filename2 = "appendix2_table7_tb.csv",
                             overwrite = TRUE)
  }
  
  # 2. CBM and NFI admin
  if (!suppliedElsewhere("cbmAdmin", sim)) {
    sim$cbmAdmin <- prepInputs(url = extractURL("cbmAdmin"),
                               fun = "data.table::fread",
                               destinationPath = inputPath(sim),
                               filename2 = "cbmAdmin.csv",
                               overwrite = TRUE)
  }
  
  if (!suppliedElsewhere("canfi_species", sim)) {
    sim$canfi_species <- prepInputs(url = extractURL("canfi_species"),
                                    fun = "data.table::fread",
                                    destinationPath = inputPath(sim),
                                    filename2 = "canfi_species.csv",
                                    overwrite = TRUE)
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
  
  if (!suppliedElsewhere("yieldTables", sim)) {
    sim$yieldTables <- prepInputs(url = extractURL("yieldTables"),
                                  fun = "data.table::fread",
                                  destinationPath = inputPath(sim),
                                  filename2 = "yieldTables.csv",
                                  overwrite = TRUE)
  }
  
  if (!suppliedElsewhere("yieldSpeciesCodes", sim)) {
    sim$yieldSpeciesCodes <- prepInputs(url = extractURL("yieldSpeciesCodes"),
                                        fun = "data.table::fread",
                                        destinationPath = inputPath(sim),
                                        filename2 = "yieldSpeciesCodes.csv",
                                        overwrite = TRUE)
  }
  
  ## pixel to pixelGroup map that gets updated annually
  if (!suppliedElsewhere("yieldPixelGroupMap", sim))
    sim$yieldPixelGroupMap <- prepInputs(url = extractURL("yieldPixelGroupMap"),
                                    destinationPath = inputPath(sim),
                                    fun = "terra::rast",
                                    rasterToMatch = sim$rasterToMatch,
                                    useCache = TRUE,
                                    overwrite = TRUE)
  
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
