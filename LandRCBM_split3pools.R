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
    defineParameter("numPixGroupPlots", "integer", 10L, NA, NA,
                    "When plotting the yield curves, this is how many unique pixel groups will ",
                    "be randomly selected and plotted."),
    defineParameter("simulateDisturbances", "character", "none", NA, NA,
                    paste("Controls which disturbances are simulated by other modules.",
                          "As of now, this can be 'none' when there are no disturbances module or fire`.")
                    ),
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
      desc = "Total above ground biomass (g/m^2) of each cohorts by pixel groups.",
      columns = c(
        speciesCode = "Species code used by LandR",
        ecoregionGroup = "The LandR spatial units (i.e., ecoregion).",
        age = "Age of the cohort.",
        B = "Total above ground biomass in (g/m^2).",
        pixelGroup = "Id of the group of pixels sharing the same cohort composition and ecoregion, used in LandR.",
        totalBiomass = "Total above ground biomass in the pixel group."
      ),
      sourceURL = "https://drive.google.com/file/d/1vwyp_i4rLncT2L1ukOvOI20DFxfYuni5/view?usp=drive_link" 
    ),
    expectsInput(
      objectName = "cbmAdmin",
      objectClass = "data.table",
      desc = paste("Provides equivalent between provincial boundaries,",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      columns = c(
        AdminBoundaryID = "Integer id for the administrative region.",
        stump_parameter_id = "Integer id for the administrative region.",
        adminName = "Name of the administrative region.",
        abreviation = "Two-letter abreviation of the administrative region.",
        SpatialUnitID = "Integer id of the CBM-spatial unit ids.",
        EcoBoundaryID = "Integer id of the ecozones."
      ),
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"
    ), 
    expectsInput(
      objectName = "disturbanceMeta", objectClass = "data.table",
      desc = paste("Table defining the disturbance event types.", 
                   "This associates CBM-CFS3 disturbances with the",
                   "event IDs in the 'disturbanceEvents' table."),
      columns = c(
        name = "Name of the disturbance.",
        eventID = "Disturbance event Id.",
        priority = "Optional: Control which events gets precedence over others in the case where multiple events happen."
      ),
      sourceURL = "https://drive.google.com/file/d/11nIiLeRwgA7R7Lw685WIfb6HPGjM6kiB/view?usp=drive_link"
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
                   "Columns are `yieldTableIndex`, `age`, `speciesCode`, `biomass`. `yieldTableIndex` is the",
                   "growth curve identifier that depends on species combination.",
                   "`biomass` is the biomass for the given species at the pixel age."),
      columns = c(
        yieldTableIndex = "Id of the group of pixels sharing yield tables.",
        age = "Age of species going from 0 to their longevity.",
        speciesCode = "Species code used by LandR.",
        biomass = "Above ground biomass in g/m^2."
      ),
      sourceURL = "https://drive.google.com/file/d/1ePPc_a8u6K_Sefd_wVS3E9BiSqK9DOnO/view?usp=drive_link"
    ),
    expectsInput(
      objectName = "yieldTablesId", objectClass = "data.table",
      desc = paste("A data.table linking spatially the `yieldTableIndex`. Columns are `pixelIndex` and `yieldTableIndex`."),
      columns = c(
        pixelIndex = "Integer id of the pixel.",
        yieldTableIndex = "Id of the group of pixels sharing yield tables."
      ),
      sourceURL = "https://drive.google.com/file/d/1OExYMhxDvTWuShlRoCMeJEDW2PgSHofW/view?usp=drive_link"
    )
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "aboveGroundBiomass",
      objectClass = "data.table",
      desc = paste("Above ground biomass (metric tonnes of carbon/ha) in each pool",
                   "for each pixel and cohort. Gets updated at each timestep.",
                   "Columns are `pixelIndex`, `speciesCode`, `age`, `merch`, `foliage`, and `other`.")
    ),
    createsOutput(
      objectName = "cohortDT",
      objectClass = "data.table",
      desc = paste("Cohort-level information.",
                   "Columns are `cohortID`, `pixelIndex`, `age`, and `gcids`.")
    ),
    createsOutput(
      objectName = "disturbanceEvents",
      objectClass = "data.table",
      desc = paste("Table with disturbance events for each simulation year.",
                   "Events types are defined in the 'disturbanceMeta' table.",
                   "Columns are `pixelIndex`, `year`, `eventID`.")
    ),
    createsOutput(
      objectName = "growth_increments",
      objectClass = "data.table",
      desc = paste("Increments (metric tonnes of carbon/ha) in each pool",
                   "for each pixel and cohort. Gets updated at each timestep.",
                   "Columns are `gcids`, `age`,`merch_inc`, `foliage_inc`, and `other_inc`.")
    ),
    createsOutput(
      objectName = "gcMeta",
      objectClass = "data.table",
      desc = paste("Growth curve-level information.",
                   "Columns are `gcids`, `species_id`, `speciesCode`, and `sw_hw`")
    ),  
    createsOutput(
      objectName = "masterRaster",
      objectClass = "SpatRaster",
      desc = paste("The template raster for the CBM simulation. Is equivalent to rasterToMatch.")
    ),
    createsOutput(
      objectName = "standDT",
      objectClass = "data.table",
      desc = paste("A data table with spatial information for the CBM spinup.",
                   "Columns are `pixelIndex`, `area`, and `spatial_unit_id`.")
    ),
    createsOutput(
      objectName = "summaryAGB",
      objectClass = "data.table",
      desc = paste("Sum of carbon mass for each species and above ground", 
                   "pool at each timestep across the landscape. Columns are `year`,",
                   "`speciesCode`, `merch`, `foliage`, `other`.")
    ),
    createsOutput(
      objectName = "yieldTablesCumulative",
      objectClass = "data.table",
      desc = paste("Yield tables divided into above ground pools. Columns are",
                   "`yieldTableIndex`, `age`, `speciesCode`, `merch`, `foliage`, `other`.)")
    ),
    createsOutput(
      objectName = "yieldTablesId",
      objectClass = "data.table",
      desc = paste("A data.table linking spatially the yield tables. Columns are `pixelIndex` and `yieldTableIndex`.")
    )
  )
))

doEvent.LandRCBM_split3pools = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # Create masterRaster. Identical to rasterToMatch.
      sim$masterRaster <- sim$rasterToMatch
      names(sim$masterRaster) <- "ldSp_TestArea"
      #### temporary
      standDT <- merge(sim$standDT,
                       sim$cbmAdmin[,c("SpatialUnitID", "abreviation")],
                       by.x = "spatial_unit_id",
                       by.y = "SpatialUnitID")
      setnames(standDT, old = "abreviation", new = "juris_id")
      setkey(standDT, pixelIndex)
      ####
      # split initial above ground biomass
      sim$aboveGroundBiomass <- splitCohortData(
        cohortData = sim$cohortData,
        pixelGroupMap = sim$pixelGroupMap,
        standDT = standDT,
        table6 = sim$table6,
        table7 = sim$table7
      )
      
      # split yield tables into AGB pools
      sim <- SplitYieldTables(sim)
      
      # adjust that the live biomass post-CBM spinup with the biomass in LandR
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "postSpinupAdjustBiomass", eventPriority = 5.5)
      
      # format disturbance events 
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools","annualDisturbances", eventPriority = 5)
      
      # split AGB of cohorts into pools 
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools","annualIncrements", eventPriority = 7)
      
      # Post annual checks
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "postAnnualChecks", eventPriority = 8.5)
      
      # summarize simulation 
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools","summarizeAGBPools", eventPriority = 10)
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
    postSpinupAdjustBiomass = {

      spinupOut <- sim$spinupResults
      
      # 1. Expand spinup output to have 1 row per cohort
      spinupOut$output <- lapply(spinupOut$output, function(tbl) {
        tbl <- tbl[spinupOut$key$cohortGroupID, ]
      })
      
      # 2. Replace above ground pools with the LandR biomass.
      spinupOut$output$pools[, c("Merch", "Foliage", "Other")] <- sim$aboveGroundBiomass[, .(merch, foliage, other)]
      
      # 3. Update below ground live pools.
      #### DC 06-05-2025 VALUES ARE HARDCODED - TODO get values from cbm_exn_get_default_parameters?
      #### Confirm equation are correct and wrap into a CBMutils function?
      totAGB <- rowSums(spinupOut$output$pools[, c("Merch", "Foliage", "Other")])
      # convert to mg/ha of total biomass
      totAGB <- totAGB * 2
      rootTotBiom <- ifelse(spinupOut$output$state$sw_hw == 1,
                            0.222 * totAGB,
                            1.576 * totAGB^0.615)
      # reconvert to carbon tonnes/ha
      rootTotC <- rootTotBiom * 0.5
      fineRootProp <- 0.072 + 0.354 * exp(-0.060212 * rootTotC)
      spinupOut$output$pools$CoarseRoots <- rootTotC * (1 - fineRootProp)
      spinupOut$output$pools$FineRoots <- rootTotC * fineRootProp
      
      # 4. Update cohortGroupID
      spinupOut$key$cohortGroupID <- spinupOut$key$cohortIDspinupOut$key$cohortGroupID <- spinupOut$key$cohortID
      
      sim$spinupResults <- spinupOut
      
      # 5. Prepare cohort groups and cbm_vars
      # Save cohort group key
      sim$cohortGroupKeep <- merge(spinupOut$key, sim$cohortDT, by = "cohortID")[, .(cohortID, pixelIndex, cohortGroupID)]
      sim$cohortGroupKeep[, cohortGroupPrev := NA_integer_]
      sim$cohortGroupKeep[, spinup          := cohortGroupID]
      data.table::setkey(sim$cohortGroupKeep, cohortID)
      
      # Prepare cohort group attributes for annual event
      sim$cohortGroups <- unique(merge(
        sim$cohortGroupKeep[, .(cohortID, cohortGroupID)],
        merge(sim$standDT[, .(pixelIndex, spatial_unit_id)], sim$cohortDT, by = "pixelIndex"),
        by = "cohortID"
      )[, .SD, .SDcols = !c("cohortID", "pixelIndex")])
      data.table::setkey(sim$cohortGroups, cohortGroupID)
      
      # Prepare spinup output data for annual event
      ## data.table with row_idx to match cohortGroupID
      sim$cbm_vars <- lapply(spinupOut$output, function(tbl){
        tbl <- data.table::data.table(row_idx = sort(unique(spinupOut$key$cohortGroupID)), tbl)
        data.table::setkey(tbl, row_idx)
        tbl
      })
      
    }, 
    annualIncrements = {
      
      # split AGB of cohorts into pools
      sim <- AnnualIncrements(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 7, "LandRCBM_split3pools", "annualIncrements")
    },
    
    postAnnualChecks = {
      # Check if the above ground biomass are synchronize
      LandR_AGB <- sim$aboveGroundBiomass[, .(merch, foliage, other)]
      cbm_AGB <- as.data.table(cbm_vars$pools[, c("Merch", "Foliage", "Other")])
      
      # Filtered to remove 0s and artifacts
      if (any(abs(cbm_AGB[Merch > 10^-10] - LandR_AGB[merch > 10^-10]) > 10^-6)) {
        stop("LandR above ground biomass do not match CBM above ground biomass")
      }
      
    }, 
    annualDisturbances = {
      
      # process annual disturbances
      sim <- AnnualDisturbances(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 5, "LandRCBM_split3pools", "annualDisturbances")
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
      poolSum <- sim$aboveGroundBiomass[, lapply(.SD, sum, na.rm = TRUE), by = pixelIndex, .SDcols = c("merch", "foliage", "other")]
      # rasterize
      merchRast <- rast(sim$rasterToMatch, names = "merchantable")
      merchRast[poolSum$pixelIndex] <- poolSum$merch
      foliageRast <- rast(sim$rasterToMatch, names = "foliage")
      foliageRast[poolSum$pixelIndex] <- poolSum$foliage
      otherRast <- rast(sim$rasterToMatch, names = "other")
      otherRast[poolSum$pixelIndex] <- poolSum$other
      
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
        increments <- sim$growth_increments[sim$cohortDT, on = c("gcids", "age")]
        incrementSum  <- increments[, lapply(.SD, sum, na.rm = TRUE), by = pixelIndex, .SDcols = c("merch_inc", "foliage_inc", "other_inc")]
        # rasterize
        merchIncRast <- rast(sim$rasterToMatch, names = "merchantable increments")
        merchIncRast[incrementSum$pixelIndex] <- incrementSum$merch_inc
        foliageIncRast <- rast(sim$rasterToMatch, names = "foliage increments")
        foliageIncRast[incrementSum$pixelIndex] <- incrementSum$foliage_inc
        otherIncRast <- rast(sim$rasterToMatch, names = "other increments")
        otherIncRast[incrementSum$pixelIndex] <- incrementSum$other_inc
        
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
  nPixGroups <- length(unique(sim$yieldTablesId$yieldTableIndex))
  nPlots <- P(sim)$numPixGroupPlots
  if (nPlots <= 0){
    stop("numPlots needs to be a positive integer")
  } else if (nPlots > nPixGroups) {
    message("numPixGroupPlots is greater than the number of pixel groups, ",
            "plotting all pixelgroups.")
    nPlots <- nPixGroups
  } 
  pixGroupToPlot <- sample(unique(sim$yieldTablesId$yieldTableIndex), nPlots)
  
  # dt for plotting.
  plot_dt <- sim$yieldTablesCumulative[yieldTableIndex %in% pixGroupToPlot]
  plot_dt[, totB := merch + foliage + other]
  
  mod$yieldTableIndexPlotted <- pixGroupToPlot
  
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
  # Step 1: Spatial Matching and Cohort/Stand Data Preparation -----------------
  # Link yield curve IDs (yieldTableIndex) to CBM spatial units 
  # and generate initial cohort/stand data structures.

  # 1.1. Match pixel-level yield table indices with CBM spatial units.
  #      `spatialMatch` creates a data.table linking yieldTableIndex, ecozone, 
  #      and jurisdiction.
  #      `na.omit` removes pixels not forested.
  spatialDT <- merge(
    sim$standDT,
    sim$yieldTablesId) |> na.omit()
  # Ensure spatial matching produced results
  if (nrow(spatialDT) == 0) {
    stop("Spatial matching between yieldTablesId, standDT failed.")
  }
  
  # 1.2. Create a new, unique ID for each unique combination of original yield 
  #      table index and CBM spatial units. This handles cases
  #      where one yield curve spans multiple ecozones/jurisdictions.
  setorderv(spatialDT, cols = c("yieldTableIndex", "spatial_unit_id"))
  spatialDT[, newytid := .GRP, by = .(yieldTableIndex, spatial_unit_id)]
  
  # 1.3. Update the pixel-level yield table mapping (`sim$yieldTablesId`) to use
  #      the new yield table ids. 
  sim$yieldTablesId <- spatialDT[, .(pixelIndex, yieldTableIndex = newytid)] 
  
  # 1.4. Generate the cohort-level attributes (`cohortDT`).
  #      This links individual cohorts (pixelGroup x species combinations)
  #      to their corresponding growth curve IDs (`gcids`).
  #      Requires the original pixelGroupMap and the updated yieldTablesId.
  cohortDT <- generateCohortDT(sim$cohortData, sim$pixelGroupMap, sim$yieldTablesId)
  # Ensure cohort generation worked
  if (is.null(cohortDT) || nrow(cohortDT) == 0) {
    stop("generateCohortDT failed.")
  }
  
  # 1.5. Store essential cohort information (cohortID, pixelIndex, age, gcids) in simList.
  sim$cohortDT <- cohortDT[, .(cohortID, pixelIndex, age, gcids)]
  
  # 1.6. Create and store metadata about growth curves (`sim$gcMeta`).
  #      Links gcids to species information.
  sim$gcMeta <- unique(cohortDT[, .(gcids, species_id, speciesCode, sw_hw)])
  
  # 1.8. Prepare yield table data (`sim$yieldTablesCumulative`) for biomass pool splitting.
  #      Get unique combinations of original yieldTableIndex and spatial units.
  spatialUnits <- unique(spatialDT[, .(yieldTableIndex, newytid, spatial_unit_id)])
  # Add these unique spatial units to the raw yield curves.
  allInfoYieldTables <- addSpatialUnits(
    cohortData = sim$yieldTablesCumulative,
    spatialUnits = spatialUnits
  )
  # Add species codes (e.g., CanfiCode)
  allInfoYieldTables <- addSpeciesCode(
    cohortData = allInfoYieldTables,
    code = "CanfiCode"
  )
  setnames(allInfoYieldTables, old = "newCode", new = "canfi_species")
  
  # Step 2: Splitting AGB Curves into CBM Pools --------------------------------
  # Convert the total Above-Ground Biomass (AGB) yield curves into cumulative biomass
  # for the three CBM above ground pools: Merchantable (merch), Foliage, and Other.
  
  # 2.1. Prepare table for CBM pool splitting function.
  #      Rename the primary biomass column to 'B' as expected by CBMutils.
  setnames(allInfoYieldTables, c("biomass"), c("B"))
  # Convert biomass units from g/m^2 to tonnes/ha: 1 g/m^2 = 0.01 tonnes/ha
  allInfoYieldTables[, B := B / 100]
  ### temporary
  standDT <- merge(sim$standDT,
                   sim$cbmAdmin[,c("SpatialUnitID", "abreviation")],
                   by.x = "spatial_unit_id",
                   by.y = "SpatialUnitID")
  setnames(standDT, old = "abreviation", new = "juris_id")
  ###
  allInfoYieldTables <- merge(allInfoYieldTables, unique(standDT[,.(spatial_unit_id, ecozone, juris_id)]))
  # 2.2. Split AGB ('B') into cumulative CBM pools (merch, foliage, other).
  #      Uses equations from Boudewyn et al. 2007 adjusted to use total above
  #      ground biomass as input, implemented in CBMutils.
  cumPools <- CBMutils::cumPoolsCreateAGB(allInfoAGBin = allInfoYieldTables,
                                          table6 = sim$table6,
                                          table7 = sim$table7,
                                          pixGroupCol = "yieldTableIndex")
  
  # 2.3. Ensure annual resolution by filling missing ages (especially age 0).
  minAgeDT <- cumPools[,.(minAge = max(0, min(age) - 1)), by = c("yieldTableIndex", "speciesCode")]
  # Create sequences from 0 up to (but not including) the minimum age found.
  # Filter out cases where minAge is already 0.
  fillAgesDT <-  minAgeDT[,.(age = seq(from = 0, to = minAge, by = 1)), 
                          by = c("yieldTableIndex", "speciesCode")]
  # Only proceed if there are ages to fill
  if (nrow(fillAgesDT) > 0) {
    # Create a table with the missing ages and zero biomass for all pools.
    zeroBiomassDT <- fillAgesDT[, .(merch = 0, foliage = 0, other = 0),
                                by = .(yieldTableIndex, speciesCode, age)] 
    
    # Combine the original curves with the filled zero-biomass ages.
    sim$yieldTablesCumulative <- rbindlist(list(cumPools, zeroBiomassDT), use.names = TRUE)
    
    # Ensure final table is ordered correctly.
    setorderv(sim$yieldTablesCumulative, c("yieldTableIndex", "speciesCode", "age")) 
  } else {
    # If no filling needed, just assign the calculated pools
    sim$yieldTablesCumulative <- cumPools
    # Ensure order just in case
    setorderv(sim$yieldTablesCumulative, c("yieldTableIndex", "speciesCode", "age")) 
  }
  
  # Step 3: Calculating Annual Increments --------------------------------------
  # Calculate the year-to-year increment in biomass for each above ground 
  # biomass pool. These increments drive the spinup dynamics.
  
  # 3.1. Define pool and increment column names.
  poolCols <- c("merch", "foliage", "other")
  incCols <- c("merch_inc", "foliage_inc", "other_inc")
  
  # 3.2. Calculate increments using `diff`.
  # `copy()` is necessary here because we need the cumulative values later,
  # and the increment calculation modifies the table using `:=`.
  yieldIncrements <- copy(sim$yieldTablesCumulative)
  # Calculate difference between successive rows within each group.
  yieldIncrements[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = poolCols,
                  by = c("yieldTableIndex", "speciesCode")]
  
  # 3.3. Link increments back to growth curve IDs (`gcids`).
  map_gcid_yield <- unique(cohortDT[, .(yieldTableIndex, speciesCode, gcids)])
  sim$growth_increments <-  yieldIncrements[map_gcid_yield, 
                                            on = .(yieldTableIndex, speciesCode)]
  
  # 3.4. Final selection and ordering of columns for `sim$growth_increments`.
  sim$growth_increments <- sim$growth_increments[,.(gcids, yieldTableIndex, age, merch_inc, foliage_inc, other_inc)]

  return(invisible(sim))
}


# Plot the curves that are directly out of the Boudewyn-translation
PlotYieldTablesPools <- function(sim){
  
  # We want to plot the same cohorts across figures
  pixGroupToPlot <- mod$yieldTableIndexPlotted
  plot_dt <- sim$yieldTablesCumulative[yieldTableIndex %in% pixGroupToPlot]
  plot_dt <- melt(
    plot_dt, 
    id.vars = c("yieldTableIndex", "speciesCode", "age"),
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
  plot_dt <- sim$growth_increments[yieldTableIndex %in% pixGroupToPlot]
  plot_dt <- melt(
    plot_dt, 
    id.vars = c("yieldTableIndex", "age", "gcids"),
    measure.vars = c("merch_inc", "foliage_inc", "other_inc"),
    variable.name = "pool",
    value.name = "B"
  )
  plot_dt <- plot_dt[plot_dt$age > 0,]
  plot_dt <- sim$gcMeta[plot_dt, on = "gcids"]
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
  # Step 1: Store the above ground biomass of the previous time step.-----------
  biomassTminus1 <- copy(sim$aboveGroundBiomass)
  # Increment age to match the *current* age for joining later
  biomassTminus1[, age := age + 1]
  # Rename cols to indicate they are from the previous timestep
  setnames(biomassTminus1, old = c("merch", "foliage", "other"), 
           new = c("merchTminus1", "foliageTminus1", "otherTminus1"))
  # Keep only necessary columns for merging
  biomassTminus1 <- biomassTminus1[, .(pixelIndex, speciesCode, age,
                                       merchTminus1, foliageTminus1, otherTminus1)]
  setkey(biomassTminus1, pixelIndex, speciesCode, age)
  
  # Step 2: Split current total above ground.-----------------------------------
  ### temporary
  standDT <- merge(sim$standDT,
                   sim$cbmAdmin[,c("SpatialUnitID", "abreviation")],
                   by.x = "spatial_unit_id",
                   by.y = "SpatialUnitID")
  setnames(standDT, old = "abreviation", new = "juris_id")
  setkey(standDT, pixelIndexIndex)
  ###
  sim$aboveGroundBiomass <- splitCohortData(
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    standDT = standDT,
    table6 = sim$table6,
    table7 = sim$table7
  )
  
  # Step 3: Calculate this year's increments.-----------------------------------
  # Full outer join between current biomass and previous biomass (incremented age)
  incrementsDT <- merge(
    sim$aboveGroundBiomass,
    biomassTminus1,
    by = c("pixelIndex", "speciesCode", "age"),
    all = TRUE 
  )
  # Handle NA values resulting from the join:
  # - NAs in current pools (merch, foliage, other) mean cohort disappeared -> fill with 0
  # - NAs in previous pools (merchTminus1, etc.) mean cohort is new -> fill with 0
  fillCols <- c("merch", "foliage", "other", "merchTminus1", "foliageTminus1", "otherTminus1")
  setnafill(incrementsDT, fill = 0, cols = fillCols)
  # Calculate increments by subtracting previous from current
  incrementsDT[, `:=`(
    merch_inc   = merch   - merchTminus1,
    foliage_inc = foliage - foliageTminus1,
    other_inc   = other   - otherTminus1
  )]
  # Create gcids and cohortID (the same for annual increments).
  incrementsDT[, c("gcids", "cohortID") := .(.I, .I)]
  # Add CBM species_id for gcMeta (is named newCode in incrementsDT)
  incrementsDT <- addSpeciesCode(incrementsDT, code = "CBM_speciesID")
  # Add forest type ("sw" or "hw") for gcMeta
  incrementsDT <- addForestType(incrementsDT)
  # Create data.table with cohort-level information
  sim$cohortDT <- incrementsDT[, .(cohortID, pixelIndex, age, gcids)]
  # Creta data.table with growth curve-level information
  sim$gcMeta <- incrementsDT[, .(gcids, species_id = newCode, speciesCode, sw_hw)]
  # Create final growth increment data.table
  sim$growth_increments <- incrementsDT[, .(gcids, age, merch_inc, foliage_inc, other_inc)]
  setkey(sim$growth_increments, gcids)
  
  return(invisible(sim))
}

# process annual disturbances
AnnualDisturbances <- function(sim){
  
  # Create an empty list if disturbanceRasters is not defined
  if(is.null(sim$disturbanceRasters)){
    sim$disturbanceRasters <- list()
  }
  
  # Add them to the disturbanceRasters if fires are simulated
  if ("fire" %in% P(sim)$simulateDisturbances| (P(sim)$simulateDisturbances == "all")){
    # Gets the correct eventID
    fireID <- sim$disturbanceMeta$eventID[sim$disturbanceMeta$distName %in% c("wildfire", "Wildfire", "fire", "wildfire")]
    
    # Ensure there is a eventID for fire
    if (length(unique(fireID)) == 0) {
      stop("disturbanceMeta does not have fires amongst its disturbances...")
    } else if (length(unique(fireID)) > 1) {
      stop("there are multiple eventID for fires in disturbanceMeta...")
    }
    
    # Convert rstCurrentBurn into a disturbanceRasters
    disturbanceRaster <- ifel(sim$rstCurrentBurn == 1, fireID, sim$rstCurrentBurn)
    
    # Add fires to disturbanceRasters
    sim$disturbanceRasters[[as.character(fireID[1])]][[as.character(time(sim))]] <- disturbanceRaster
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
    ) |> Cache(userTags = "prepInputsRTM")
  }
  
  if (!suppliedElsewhere("studyArea", sim)) {
    sim$studyArea <- prepInputs(
      url = extractURL("studyArea"),
      fun = "terra::vect",
      destinationPath = inputPath(sim),
      overwrite = TRUE
    ) |> Cache(userTags = "prepInputsSA")
  }
  
  # Pixel groups from vegetation data.
  # Is used to link cohortData spatially.
  if (!suppliedElsewhere("pixelGroupMap", sim)) {
    sim$pixelGroupMap <- prepInputs(
      url = extractURL("pixelGroupMap"),
      destinationPath = inputPath(sim),
      fun = "terra::rast",
      to = sim$rasterToMatch,
      overwrite = TRUE
    ) |> Cache(userTags = "prepInputsPGM")
  }
  
  # 2. NFI params. Used to split total biomass into biomass of the three CBM
  #                above ground biomass pools.
  if (!suppliedElsewhere("table6", sim)) {
    sim$table6 <- prepInputs(url = extractURL("table6"),
                             fun = "data.table::fread",
                             destinationPath = inputPath(sim),
                             filename2 = "appendix2_table6_tb.csv",
                             overwrite = TRUE) |> Cache(userTags = "prepInputsTable6")
  }
  
  if (!suppliedElsewhere("table7", sim)) {
    sim$table7 <- prepInputs(url = extractURL("table7"),
                             fun = "data.table::fread",
                             destinationPath = inputPath(sim),
                             filename2 = "appendix2_table7_tb.csv",
                             overwrite = TRUE) |> Cache(userTags = "prepInputsTable7")
  }
  
  # 3. Yield tables data
  
  # Data table that links the yieldTableIndex (group of growth curves of coexisting
  # species) to pixelIndex (one yieldTableIndex per pixelIndex).
  if (!suppliedElsewhere("yieldTablesId", sim)) {
    sim$yieldTablesId <- prepInputs(url = extractURL("yieldTablesId"),
                                    fun = "data.table::fread",
                                    destinationPath = inputPath(sim),
                                    filename2 = "yieldTablesId.csv",
                                    overwrite = TRUE) |> Cache(userTags = "prepInputsYTId")
  } else if (!is.null(sim$yieldTablesId)) {
    if(!all(c("yieldTableIndex", "pixelIndex") %in% colnames(sim$yieldTablesId))) {
      stop("yieldTablesId needs the columns yieldTableIndex and pixelIndex")
    }
  }
  
  # Yield tables. Each yield tables as multiple growth curve (one per species)
  if (!suppliedElsewhere("yieldTablesCumulative", sim)) {
    sim$yieldTablesCumulative <- prepInputs(url = extractURL("yieldTablesCumulative"),
                                            fun = "data.table::fread",
                                            destinationPath = inputPath(sim),
                                            filename2 = "yieldTablesCumulative.csv",
                                            overwrite = TRUE) |> Cache(userTags = "prepInputsYTC")
  } else if (!is.null(sim$yieldTablesCumulative)) {
    if(!all(c("yieldTableIndex", "speciesCode", "biomass", "age") %in% colnames(sim$yieldTablesCumulative))){
      stop("yieldTablesCumulative needs the columns yieldTableIndex, age, biomass, and speciesCode")
    }
  }
  
  # 4. Cohort data. Information on biomass for each cohort and pixel group.
  if (!suppliedElsewhere("cohortData", sim)){
    sim$cohortData <- prepInputs(url = extractURL("cohortData"),
                                 destinationPath = inputPath(sim),
                                 fun = "data.table::fread",
                                 overwrite = TRUE,
                                 filename2 = "cohortData.csv") |> Cache(userTags = "prepInputsCD")
  } else if (!is.null(sim$cohortData)) {
    if(!all(c("pixelGroup", "speciesCode", "B", "age") %in% colnames(sim$cohortData))){
      stop("cohortData needs the columns pixelGroup, age, B, and speciesCode")
    }
  }
  
  # 5. Disturbance data
  
  # Metadata on disturbance. Links the eventID to the disturbance type.
  if (!suppliedElsewhere("disturbanceMeta", sim)) {
    sim$disturbanceMeta <- prepInputs(url = extractURL("disturbanceMeta"),
                                      destinationPath = inputPath(sim),
                                      fun = "data.table::fread",
                                      overwrite = TRUE,
                                      filename2 = "disturbanceMeta.csv") |> Cache(userTags = "prepInputsDistMeta")
  } else if (!is.null(sim$disturbanceMeta)) {
    if(!all(c("eventID", "distName") %in% colnames(sim$disturbanceMeta))) {
      stop("disturbanceMeta needs the columns eventID and distName")
    }
  }
  
  # Raster of the pixel currently burning. Pixels with values of 1 are burning.
  if (!suppliedElsewhere("rstCurrentBurn", sim)) {
    sim$rstCurrentBurn <- sim$rasterToMatch
    sim$rstCurrentBurn[] <- NA
  }
  
  if (!suppliedElsewhere("cbmAdmin", sim)) {
    sim$cbmAdmin <- prepInputs(url = extractURL("cbmAdmin"),
                               targetFile = "cbmAdmin.csv",
                               destinationPath = inputPath(sim),
                               fun = "data.table::fread",
                               overwrite = TRUE) |> Cache(userTags = "prepInputsCBMAdmin")
  }
  
  return(invisible(sim))
}
