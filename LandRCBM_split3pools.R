defineModule(sim, list(
  name = "LandRCBM_split3pools",
  description = paste("Takes total aboveground biomass provided by LandR and divides",
                      "it into the 3 required CBM pools."),
  keywords = "",
  authors = c(
    person("Celine", "Boisvenue", email = "cboivenue@gmail.com", role = c("aut", "cre")),
    person("Dominique", "Caron", email = "dominique.caron@nrcan-rncan.gc.ca", role = c("aut")),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(LandRCBM_split3pools = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandRCBM_split3pools.Rmd"), ## same file
  reqdPkgs = list("PredictiveEcology/SpaDES.core", "reproducible (>= 2.1.2)", "data.table", "ggplot2", "terra",
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
      objectName = "cohortData", objectClass = "data.table",
      desc = "Above ground biomass (g/m^2) of cohorts in pixel groups.",
      sourceURL = "https://drive.google.com/file/d/1vwyp_i4rLncT2L1ukOvOI20DFxfYuni5/view?usp=drive_link" 
    ),
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      desc = paste("Table defining the disturbance event types.", 
                   "This associates CBM-CFS3 disturbances with the",
                   "event IDs in the 'disturbanceEvents' table."),
      sourceURL("https://drive.google.com/file/d/11nIiLeRwgA7R7Lw685WIfb6HPGjM6kiB/view?usp=drive_link")
    ),
    expectsInput(
      objectName = "ecozones", objectClass = "data.table",
      desc = paste("A data.table with the ecozone for each pixelId. Used to determine",
                   "the equation parameters to split the above ground biomass into",
                   "carbon pools."),
      sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
    ),
    expectsInput(
      objectName = "juridictions", objectClass = "data.table",
      desc = paste("A data.table with the province/territory for each pixelId.", 
                   "Used to determine the equation parameters to split the above", 
                   "ground biomass into carbon pools."),
      sourceURL = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpr_000a21a_e.zip"
    ),
    expectsInput(
      objectName = "pixelGroupMap", objectClass = "SpatRaster",
      desc = paste("PixelGroup map from LandR. Group of pixels that shares the same.",
                   "cohort composition"),
      sourceURL = "https://drive.google.com/file/d/1oCL8EgZ6l8Bn0q1iJuu2yP3M7khUtTO4/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "SpatRaster",
      desc = "Template raster to use for simulations; defaults is the RIA study area.", 
      sourceURL = "https://drive.google.com/file/d/1LUEiVMUWd_rlG9AAFan7zKyoUs22YIX2/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "rstCurrentBurn", objectClass = "SpatRaster",
      desc = "Raster of fires with 1 indicating burned pixels."
    ),
    expectsInput(
      objectName = "studyArea", objectClass =  "sfc",
      desc = "Polygon to use as the study area; default is the RIA study area.", 
      sourceURL = "https://drive.google.com/file/d/1M8jGAuq1wuavSb40c-s9HKyigCuTt9Rf/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "table6", objectClass = "data.table",
      desc = paste("Proportion model parameters similar to Boudewyn et al 2007,",
                   "but recalculated using total biomass (metric tonnes of tree biomass/ha) instead of vol/ha."),
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv"
    ),
    expectsInput(
      objectName = "table7", objectClass = "data.table",
      desc = paste("Caps on proportion models similar to Boudewyn et al. 2007",
                   "but recalculated using total biomass (metric tonnes of tree biomass/ha)",
                   "instead of vol/ha."),
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv"
    ),
    expectsInput(
      objectName = "yieldTablesCumulative", objectClass = "data.table",
      desc = paste("Yield Tables intended to supply the requirements for a CBM spinup.",
                   "Columns are `gcid`, `age`, `speciesCode`, `biomass`. `gcid` is the",
                   "growth curve identifier that depends on species combination.",
                   "`biomass` is the biomass for the given species at the pixel age."),
      sourceURL = "https://drive.google.com/file/d/1ePPc_a8u6K_Sefd_wVS3E9BiSqK9DOnO/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "yieldTablesId", objectClass = "data.table",
      desc = paste("A data.table linking spatially the `gcid`. Columns are `pixelId` and `gcid`."),
      sourceURL = "https://drive.google.com/file/d/1OExYMhxDvTWuShlRoCMeJEDW2PgSHofW/view?usp=drive_link"
    )
    # expectsInput("sppColorVect", "character",
    #              desc = paste("A named vector of colors to use for plotting."))
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "aboveGroundBiomass",
      objectClass = "data.table",
      desc = paste("Above ground biomass (metric tonnes of tree biomass/ha) in each pool",
                   "for each pixel and cohort. Gets updated at each timestep.",
                   "Columns are `pixelId`, `speciesCode`, `age`, `merch`, `foliage`, and `other`.")
    ),
    createsOutput(
      objectName = "aboveGroundIncrements",
      objectClass = "data.table",
      desc = paste("Increments (metric tonnes of tree biomass/ha) in each pool",
                   "for each pixel and cohort. Gets updated at each timestep.",
                   "Columns are `pixelId`, `speciesCode`, `age`, `merchInc`, `foliageInc`, and `otherInc`.")
    ),
    createsOutput(
      objectName = "disturbanceEvents",
      objectClass = "data.table",
      desc = paste("Table with disturbance events for each simulation year.",
                   "Events types are defined in the 'disturbanceMeta' table.")
    ),
    createsOutput(
      objectName = "summaryAGB",
      objectClass = "data.table",
      desc = paste("Sum of biomass and increments for each species and above ground", 
                   "pool at each timestep across the landscape. Columns are `year`,",
                   "`speciesCode`, `merch`, `foliage`, `other`, merchInc`,",
                   "`foliageInc`, and `otherInc`.")
    ),
    createsOutput(
      objectName = "yieldTablesCumulative",
      objectClass = "data.table",
      desc = paste("Yield tables divided into above ground pools. Columns are",
                   "`gcid`, `age`, `speciesCode`, `merch`, `foliage`, `other`.)")
    ),
    createsOutput(
      objectName = "yieldTablesId",
      objectClass = "data.table",
      desc = paste("A data.table linking spatially the `gcid`. Columns are `pixelId` and `gcid`.")
    ),
    createsOutput(
      objectName = "yieldTablesIncrements",
      objectClass = "data.table",
      desc = paste("Yield tables divided into above ground pools and represented",
                   "yearly increments. Columns are `gcid`, `age`, `speciesCode`,",
                   "`merch`, `foliage`, `other`.")
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
      sumBySpecies <- sim$aboveGroundBiomass[, lapply(.SD, sum, na.rm = TRUE), by = speciesCode, .SDcols = c("merch", "foliage", "other")]
      sumBySpecies$year <- time(sim)[1]
      
      if (time(sim) == start(sim)){
        sim$summaryAGB <- sumBySpecies
      } else {
        sim$summaryAGB <- rbind(
          sim$summaryAGB,
          sumBySpecies
        )
      }
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 10, "LandRCBM_split3pools", "summarizeAGBPools")
    },
    plotMaps = {
      
      # get the sum of each pool per pixelGroups
      poolSum <- sim$aboveGroundBiomass[, lapply(.SD, sum, na.rm = TRUE), by = pixelId, .SDcols = c("merch", "foliage", "other")]
      # rasterize
      merchRast <- rast(sim$rasterToMatch, names = "merchantable")
      merchRast[poolSum$pixelId] <- poolSum$merch
      foliageRast <- rast(sim$rasterToMatch, names = "foliage")
      foliageRast[poolSum$pixelId] <- poolSum$foliage
      otherRast <- rast(sim$rasterToMatch, names = "other")
      otherRast[poolSum$pixelId] <- poolSum$other
      
      # plot
      Plots(merchRast,
            fn = gg_agbpools,
            types = P(sim)$.plots,
            filename = paste0("merch", "_year_", round(time(sim))),
            title = paste("Total merchantable biomass", "year", round(time(sim))))
      Plots(foliageRast,
            fn = gg_agbpools,
            types = P(sim)$.plots,
            filename = paste0("foliage", "_year_", round(time(sim))),
            title = paste("Foliage biomass", "year", round(time(sim))))
      Plots(otherRast,
            fn = gg_agbpools,
            types = P(sim)$.plots,
            filename = paste0("other", "_year_", round(time(sim))),
            title = paste("Other above ground biomass", "year", round(time(sim))))
      
      # map increments
      if (time(sim) != start(sim)){
        incrementSum  <- sim$aboveGroundIncrements[, lapply(.SD, sum, na.rm = TRUE), by = pixelId, .SDcols = c("merchInc", "foliageInc", "otherInc")]
        # rasterize
        merchIncRast <- rast(sim$rasterToMatch, names = "merchantable increments")
        merchIncRast[incrementSum$pixelId] <- incrementSum$merchInc
        foliageIncRast <- rast(sim$rasterToMatch, names = "foliage increments")
        foliageIncRast[incrementSum$pixelId] <- incrementSum$foliageInc
        otherIncRast <- rast(sim$rasterToMatch, names = "other increments")
        otherIncRast[incrementSum$pixelId] <- incrementSum$otherInc
        
        # plot
        Plots(merchIncRast,
              fn = gg_agbpools,
              types = P(sim)$.plots,
              filename = paste0("merchInc", "_year_", round(time(sim))),
              title = paste("Total merchantable increment", "year", round(time(sim))))
        Plots(foliageIncRast,
              fn = gg_agbpools,
              types = P(sim)$.plots,
              filename = paste0("foliageInc", "_year_", round(time(sim))),
              title = paste("Foliage increment", "year", round(time(sim))))
        Plots(otherIncRast,
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
        summaryAGBPoolsLandscape <- sim$summaryAGB[, lapply(.SD, sum, na.rm = TRUE), by = year, .SDcols = c("merch", "foliage", "other")]
        # Landscape summary
        Plots(summaryAGBPoolsLandscape,
              fn = gg_landscapesummary,
              types = P(sim)$.plots,
              filename = paste0("LandscapeAGBPoolSummary")
        )
        
        # Species summary
        Plots(sim$summaryAGB,
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
  nPixGroups <- length(unique(sim$yieldTablesId$gcid))
  nPlots <- P(sim)$numPixGroupPlots
  if (nPlots <= 0){
    stop("numPlots needs to be a positive integer")
  } else if (nPlots > nPixGroups) {
    message("numPixGroupPlots is greater than the number of pixel groups, ",
            "plotting all pixelgroups.")
    nPlots <- nPixGroups
  } 
  pixGroupToPlot <- sample(unique(sim$yieldTablesId$gcid), nPlots)

  # dt for plotting.
  plot_dt <- sim$yieldTablesCumulative[gcid %in% pixGroupToPlot]
  plot_dt[, totB := merch + foliage + other]
  
  mod$gcIdPlotted <- pixGroupToPlot
  
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
  # with the yieldTablesId and speciesCode. yieldTablesId gives us the location. 
  # Location let's us figure out which ecozone, and admin. The canfi_species have 
  # numbers which we need to match with the parameters.
  
  # Spatial Matching
  spatialDT <- spatialMatch(
    pixelGroupMap = sim$yieldTablesId,
    juridictions = sim$juridictions,
    ecozones = sim$ecozones
  ) |> na.omit()
  
  setorderv(spatialDT, cols = c("gcid", "ecozone", "juris_id"))
  spatialDT[, newgcid := .GRP, by = .(gcid, ecozone, juris_id)]
  
  # Update yieldTablesId. When a gcid cross a CBM spatial units, there is a bifurcation.
  # We should get a number of gcid >= than the number before the spatial matching.
  sim$yieldTablesId <- spatialDT[, .(pixelId, gcid = newgcid)] 
  
  spatialUnits <- unique(spatialDT[, pixelId := NULL])
  
  allInfoYieldTables <- addSpatialUnits(
    cohortData = sim$yieldTablesCumulative,
    spatialUnits = spatialUnits
  )
  
  # add the species code in canfi
  allInfoYieldTables <- addCanfiCode(
    cohortData = allInfoYieldTables
  )
  
  ##############################################################################
  #2. START processing curves from AGB to 3 pools
  
  # Calculating the yieldTablesCumulative
  
  # set correct column names
  setnames(allInfoYieldTables, c("biomass"), c("B"))
  
  # convert m^2 into tonnes/ha
  allInfoYieldTables$B <- allInfoYieldTables$B/100
  
  cumPools <- CBMutils::cumPoolsCreateAGB(allInfoAGBin = allInfoYieldTables,
                                table6 = sim$table6,
                                table7 = sim$table7,
                                pixGroupCol = "gcid")
  
  #TODO
  # MAKE SURE THE PROVIDED CURVES ARE ANNUAL (probably not needed for LandR
  # connection, but might be needed for future connection to other sources of
  # AGB).
  ### if not, we need to extrapolate to make them annual
  
  # add missing years (e.g., Boudewyn equation do not handle age 0)
  minAgeId <- cumPools[,.(minAge = max(0, min(age) - 1)), by = c("gcid", "speciesCode")]
  fill0s <- minAgeId[,.(age = seq(from = 0, to = minAge, by = 1)), by = c("gcid", "speciesCode")]
  add0s <- data.table(gcid = fill0s$gcid,
                           speciesCode = fill0s$speciesCode,
                           age = fill0s$age,
                           merch = 0,
                           foliage = 0,
                           other = 0 )
  
  sim$yieldTablesCumulative <- rbind(cumPools,add0s)
  setcolorder(sim$yieldTablesCumulative, c("gcid", "speciesCode", "age"))
  setorderv(sim$yieldTablesCumulative, c("gcid", "speciesCode", "age"))
  
  # 3 Calculating Increments
  incCols <- c("merchInc", "foliageInc", "otherInc")
  poolCols <- c("merch", "foliage", "other")
  # This line calculates the first difference of each colNames, shifting it down 
  # by one row and filling the first entry with NA.
  yieldIncrements <- copy(sim$yieldTablesCumulative)
  yieldIncrements[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = poolCols,
               by = c("gcid", "speciesCode")]
  sim$yieldTablesIncrements <- yieldIncrements[,.(gcid, speciesCode, age, merchInc, foliageInc, otherInc)]
  
  return(invisible(sim))
}


# Plot the curves that are directly out of the Boudewyn-translation
PlotYieldTablesPools <- function(sim){
  
  # We want to plot the same cohorts across figures
  pixGroupToPlot <- mod$gcIdPlotted
  plot_dt <- sim$yieldTablesCumulative[gcid %in% pixGroupToPlot]
  plot_dt <- melt(
    plot_dt, 
    id.vars = c("gcid", "speciesCode", "age"),
    measure.vars = c("merch", "foliage", "other"),
    variable.name = "pool",
    value.name = "B"
  )
  # plot total yield curves
  Plots(plot_dt, 
        fn = gg_yieldCurvesPools,
        types = P(sim)$.plots,
        filename = "yieldCurvePools",
        title = paste("Yield curves for", length(unique(plot_dt$yieldPixelGroup)), "randomly selected pixel groups")
  )
  
  # plot increments
  plot_dt <- sim$yieldTablesIncrements[gcid %in% pixGroupToPlot]
  plot_dt <- melt(
    plot_dt, 
    id.vars = c("gcid", "speciesCode", "age"),
    measure.vars = c("merchInc", "foliageInc", "otherInc"),
    variable.name = "pool",
    value.name = "B"
  )
  plot_dt <- plot_dt[plot_dt$age > 0,]
  Plots(plot_dt, 
        fn = gg_yieldCurvesPools,
        types = P(sim)$.plots,
        filename = "yieldCurveIncrements",
        title = "Increments merch fol other by species and pixel groups"
        )
    message(crayon::red("User: please inspect figures of the raw translation of your increments in: ",
                        figurePath(sim)))
  
  return(invisible(sim))
}

# Process yearly vegetation inputs
AnnualIncrements <- function(sim){
  if (time(sim) != start(sim)){
    annualIncrements <- sim$aboveGroundBiomass
    annualIncrements$age <- annualIncrements$age + 1
    setnames(annualIncrements, old = c("merch", "foliage", "other"), new = c("merchTminus1", "foliageTminus1", "otherTminus1"))
  }
  
  # 3. split cohort data of current year
  spatialDT <- spatialMatch(
    pixelGroupMap = sim$pixelGroupMap,
    juridictions = sim$juridictions,
    ecozones = sim$ecozones
  ) |> na.omit()
  
  spatialDT[, newPixelGroup := .GRP, by = .(pixelGroup, ecozone, juris_id)]
  spatialUnits <- unique(spatialDT[, !("pixelId")])
  
  allInfoCohortData <- addSpatialUnits(
    cohortData = sim$cohortData,
    spatialUnits = spatialUnits
  )
  
  # add the species code in canfi
  allInfoCohortData <- addCanfiCode(
    cohortData = allInfoCohortData
  )
  
  # convert m^2 into tonnes/ha
  allInfoCohortData$B <- allInfoCohortData$B/100
  cohortPools <- CBMutils::cumPoolsCreateAGB(allInfoAGBin = allInfoCohortData,
                                       table6 = sim$table6,
                                       table7 = sim$table7,
                                       "pixelGroup")
  spatialDT[, pixelGroup := NULL]
  sim$aboveGroundBiomass <- merge(spatialDT, cohortPools, by.x = "newPixelGroup", by.y = "pixelGroup")
  sim$aboveGroundBiomass <- sim$aboveGroundBiomass[, .(pixelId, speciesCode, age, merch, foliage, other)]
  setorderv(sim$aboveGroundBiomass, c("pixelId", "speciesCode", "age"))
  
  # 4. append the cohortPools of the previous year
  if (time(sim) != start(sim)){
    annualIncrements <- merge(annualIncrements, 
                              sim$aboveGroundBiomass, 
                              by = c("pixelId", "speciesCode", "age"),
                              all = TRUE)
    
    # adds biomass 0 when there is a new species in a pixelGroup
    setnafill(annualIncrements, fill = 0, 
              cols=c("merch", "foliage", "other", "merchTminus1", "foliageTminus1", "otherTminus1"))
    
    # 5. take the difference
    annualIncrements[, `:=`(merchInc = merch - merchTminus1,
                            foliageInc = foliage - foliageTminus1,
                            otherInc = other - otherTminus1)]
    
    sim$aboveGroundIncrements <- annualIncrements[,.(pixelId, 
                                                speciesCode,
                                                age = age, 
                                                merchInc, 
                                                foliageInc, 
                                                otherInc)]    
  }
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  # 1. Spatial information
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- prepInputs(
      url = extractURL("rasterToMatch"),
      fun = "terra::rast",
      destinationPath = inputPath(sim),
      overwrite = TRUE
    ) |> Cache()
  }
  
  if (!suppliedElsewhere("studyArea", sim)) {
    sim$studyArea <- prepInputs(
      url = extractURL("studyArea"),
      fun = "terra::vect",
      destinationPath = inputPath(sim),
      overwrite = TRUE
    ) |> Cache()
  }
  
  # pixel groups from vegetation data that gets updated annually
  if (!suppliedElsewhere("pixelGroupMap", sim)) {
    sim$pixelGroupMap <- prepInputs(
      url = extractURL("pixelGroupMap"),
      destinationPath = inputPath(sim),
      fun = "terra::rast",
      to = sim$rasterToMatch,
      overwrite = TRUE
    ) |> Cache()
  }
  
  if(inherits(sim$studyArea, "SpatVector")){
    studyAreaBuffered <- terra::buffer(sim$studyArea, res(sim$rasterToMatch)[1])
  } else if (inherits(sim$studyArea, "sf")) {
    studyAreaBuffered <- sf::st_buffer(sim$studyArea, res(sim$rasterToMatch)[1])
  } else {
    stop("studyArea needs to be a SpatVector or a sf polygon")
  }

  # ecozones
  if (!suppliedElsewhere("ecozones", sim)) {
    ecozones <- prepInputs(
      url = extractURL("ecozones"),
      destinationPath = inputPath(sim),
      fun = "terra::vect",
      to = studyAreaBuffered,
      overwrite = TRUE
    ) |> Cache()
    ez <- rasterize(ecozones, sim$rasterToMatch, field = "ECOZONE")
    sim$ecozones <- data.table(ecozone = as.integer(ez[]))
    sim$ecozones <- sim$ecozones[, pixelId := .I] |> na.omit()
    setcolorder(sim$ecozones, c("pixelId", "ecozone"))
  }
  
  if (!suppliedElsewhere("juridictions", sim)) {
    dt <- data.table(
      PRUID = c(10, 11, 12, 13, 24, 35, 46, 47, 48, 59, 60, 61, 62),
      juris_id = c("NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC", "YT", "NT", "NU")
    )
    
    juridictions <- prepInputs(
      url = extractURL("juridictions"),
      destinationPath = inputPath(sim),
      fun = "terra::vect",
      to = studyAreaBuffered,
      overwrite = TRUE
    ) |> Cache()
    juris_id <- rasterize(juridictions, sim$rasterToMatch, field = "PRUID")
    juris_id <- as.data.table(juris_id, na.rm = FALSE)
    juris_id$PRUID <- as.integer(as.character(juris_id$PRUID))
    sim$juridictions <- dt[juris_id, on = "PRUID"]
    sim$juridictions <- sim$juridictions[, pixelId := .I] |> na.omit()
    setcolorder(sim$juridictions, c("pixelId", "PRUID", "juris_id"))
  }
  
  # 2. NFI params
  if (!suppliedElsewhere("table6", sim)) {
    sim$table6 <- prepInputs(url = extractURL("table6"),
                             fun = "data.table::fread",
                             destinationPath = inputPath(sim),
                             filename2 = "appendix2_table6_tb.csv",
                             overwrite = TRUE) |> Cache()
  }
  
  if (!suppliedElsewhere("table7", sim)) {
    sim$table7 <- prepInputs(url = extractURL("table7"),
                             fun = "data.table::fread",
                             destinationPath = inputPath(sim),
                             filename2 = "appendix2_table7_tb.csv",
                             overwrite = TRUE) |> Cache()
  }
  
  
  # 3. Yield curve data
  
  # reference for species to cohort_id
  if (!suppliedElsewhere("yieldTablesId", sim)) {
    sim$yieldTablesId <- prepInputs(url = extractURL("yieldTablesId"),
                                    fun = "data.table::fread",
                                    destinationPath = inputPath(sim),
                                    filename2 = "yieldTablesId.csv",
                                    overwrite = TRUE) |> Cache()
  }
  
  # actual yield curves
  if (!suppliedElsewhere("yieldTablesCumulative", sim)) {
    sim$yieldTablesCumulative <- prepInputs(url = extractURL("yieldTablesCumulative"),
                                  fun = "data.table::fread",
                                  destinationPath = inputPath(sim),
                                  filename2 = "yieldTablesCumulative.csv",
                                  overwrite = TRUE) |> Cache()
  }

  #4. Cohort data. Information on biomass for each cohort and pixel group. Gets updated
  # annually.
  if (!suppliedElsewhere("cohortData", sim)){
    sim$cohortData <- prepInputs(url = extractURL("cohortData"),
                                 destinationPath = inputPath(sim),
                                 fun = "data.table::fread",
                                 overwrite = TRUE,
                                 filename2 = "cohortData.csv") |> Cache()
  }

  
  # TODO will be used for plotting to keep the same colors of species as in LandR modules
  # if (!suppliedElsewhere("sppColorVect", sim)){
  #   sp <- sort(unique(sim$yieldSpeciesCodes$SpeciesCode))
  #   sim$sppColorVect <- RColorBrewer::brewer.pal(n = length(sp), name = 'Accent')
  #   names(sim$sppColorVect) <- sp
  # }
  
  # 5. Disturbance meta
  if (!suppliedElsewhere("disturbanceMeta", sim)) {
    sim$cohortData <- prepInputs(url = extractURL("disturbanceMeta"),
                                 destinationPath = inputPath(sim),
                                 fun = "data.table::fread",
                                 overwrite = TRUE,
                                 filename2 = "disturbanceMeta.csv") |> Cache()
  }

  if (!suppliedElsewhere("rstCurrentBurn", sim)) {
    sim$rstCurrentBurn <- sim$rasterToMatch
    sim$rstCurrentBurn[] <- NA
  }
  
  return(invisible(sim))
}
