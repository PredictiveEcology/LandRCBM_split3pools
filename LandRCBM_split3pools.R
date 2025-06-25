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
      )
    ),
    expectsInput(
      objectName = "cbm_vars",
      objectClass = "list",
      desc = paste("List of 4 data tables: parameters, pools, flux, and state.",
                    "This is created initially during the spinup and updated each year."),
    ), 
    expectsInput(
      objectName = "pixelGroupMap", objectClass = "SpatRaster",
      desc = paste("PixelGroup map from LandR. Group of pixels that shares the same.",
                   "cohort composition")
    ),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "SpatRaster",
      desc = "Template raster to use for simulations; defaults is the RIA study area."
    ),
    expectsInput(
      objectName = "standDT", objectClass =  "data.table",
      desc = paste0("A data table with spatial information of each pixel."),
      columns = c(
        pixelIndex         = "`masterRaster` cell index",
        area               = "`masterRaster` cell area in meters",
        admin_abbrev       = "Canada administrative abbreviation extracted from `adminLocator`",
        admin_boundary_id  = "CBM-CFS3 administrative boundary ID",
        ecozone            = "Canada ecozone ID extracted from `ecoLocator`",
        spatial_unit_id    = "CBM-CFS3 spatial unit ID"
      )
    ),
    expectsInput(
      objectName = "spinupResult", objectClass =  "list",
      desc = paste0("A list of data.tables with the results from the CBM spinup.")
    ),
    expectsInput(
      objectName = "spinupSQL", objectClass =  "dataset",
      desc = paste0("Table containing many necesary spinup parameters used in CBM_core")
    ),
    expectsInput(
      objectName = "studyArea", objectClass =  "sfc",
      desc = "Polygon to use as the study area; default is the RIA study area."
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
      )
    ),
    expectsInput(
      objectName = "yieldTablesId", objectClass = "data.table",
      desc = paste("A data.table linking spatially the `yieldTableIndex`. Columns are `pixelIndex` and `yieldTableIndex`."),
      columns = c(
        pixelIndex = "Integer id of the pixel.",
        yieldTableIndex = "Id of the group of pixels sharing yield tables."
      )
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
      objectName = "cbm_vars", objectClass = "list",
      desc = paste(
        "List of 4 data tables: parameters, pools, flux, and state.",
        "This is created initially during the spinup and updated each year.")
    ),
    createsOutput(
      objectName = "cohortDT",
      objectClass = "data.table",
      desc = paste("Cohort-level information.",
                   "Columns are `cohortID`, `pixelIndex`, `age`, and `gcids`.")
    ),
    createsOutput(
      objectName = "cohortGroupKeep",
      objectClass = "data.table",
      desc = paste("Key connecting `cohortID` with current and previous `cohortGroupID`",
                   "associations for each year of the simulation")
    ),
    createsOutput(
      objectName = "cohortGroups",
      objectClass = "data.table",
      desc = paste("Cohort group shared attributes")
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
      objectName = "spinupResult", objectClass = "data.frame",
      desc = "Spinup results updated with the live biomass matching the observed data."),
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

      # split initial above ground biomass
      sim$aboveGroundBiomass <- splitCohortData(
        cohortData = sim$cohortData,
        pixelGroupMap = sim$pixelGroupMap,
        standDT = sim$standDT[,.(pixelIndex, juris_id = admin_abbrev, ecozone, spatial_unit_id)],
        table6 = sim$table6,
        table7 = sim$table7
      )
      
      # split yield tables into AGB pools
      sim <- SplitYieldTables(sim)
      
      # adjust that the live biomass post-CBM spinup with the biomass in LandR
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "postSpinupAdjustBiomass", eventPriority = 5.5)
      
      # split AGB of cohorts into pools 
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools","annualIncrements", eventPriority = 7)
      
      # prepare inputs for CBM annual event (cbm_vars)
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools","prepareCBMvars", eventPriority = 8.25)
      
      # Post annual checks
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "postAnnualChecks", eventPriority = 8.75)
      
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
      spinupOut <- sim$spinupResult

      # 1. Expand spinup output to have 1 row per cohort
      spinupOut$output <- lapply(spinupOut$output, function(tbl) {
        tbl <- tbl[spinupOut$key$cohortGroupID, ]
      })
      
      # 2. Replace above ground pools with the LandR biomass.
      spinupOut$output$pools[, c("Merch", "Foliage", "Other")] <- sim$aboveGroundBiomass[, .(merch, foliage, other)]
      
      # 3. Update below ground live pools.
      rootsC <- CBMutils::calcRootC(spinupOut$output$pools, spinupOut$output$state$sw_hw)
      spinupOut$output$pools[, c("CoarseRoots", "FineRoots")] <- rootsC
      
      # 4. Update cohortGroupID
      spinupOut <- updateSpinupCohortGroups(spinupOut)
      
      sim$spinupResult <- spinupOut
      
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
      
      # prepare cohort groups
      sim <- UpdateCohortGroups(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 7, "LandRCBM_split3pools", "annualIncrements")
    },
    prepareCBMvars = {
      
      # split AGB of cohorts into pools
      sim <- PrepareCBMvars(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 8.25, "LandRCBM_split3pools", "prepareCBMvars")
    },
    
    postAnnualChecks = {
      # Check if the above ground biomass are synchronize
      LandR_AGB <- sim$aboveGroundBiomass[, .(merch, foliage, other)]
      cbm_AGB <- as.data.table(sim$cbm_vars$pools[, c("Merch", "Foliage", "Other")])
      
      # Filtered to remove 0s and artifacts
      # if (max(abs(cbm_AGB[Merch > 10^-10] - LandR_AGB[merch > 10^-10])) > 10^-6) {
      #   stop("LandR above ground biomass do not match CBM above ground biomass")
      # }
      
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
        increments <- sim$cohortDT[sim$growth_increments, on = c("gcids", "age")]
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
  allInfoYieldTables <- merge(allInfoYieldTables, unique(sim$standDT[,.(spatial_unit_id, ecozone, juris_id = admin_abbrev)]))
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
  sim$aboveGroundBiomass <- splitCohortData(
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    standDT = sim$standDT[,.(pixelIndex, juris_id = admin_abbrev, ecozone, spatial_unit_id)],
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
  groupCols <- c("speciesCode", "age", "merch_inc", "foliage_inc", "other_inc")
  incrementsDT[, gcids := .GRP, by = groupCols]
  incrementsDT[, cohortID := .I]
  # Add CBM species_id for gcMeta (is named newCode in incrementsDT)
  incrementsDT <- addSpeciesCode(incrementsDT, code = "CBM_speciesID")
  # Add forest type ("sw" or "hw") for gcMeta
  incrementsDT <- addForestType(incrementsDT)
  # Create data.table with cohort-level information
  sim$cohortDT <- incrementsDT[, .(cohortID, pixelIndex, age, gcids)]
  # Creta data.table with growth curve-level information
  sim$gcMeta <- unique(incrementsDT[, .(gcids, species_id = newCode, speciesCode, sw_hw)])
  # Create final growth increment data.table
  sim$growth_increments <- unique(incrementsDT[, .(gcids, age, merch_inc, foliage_inc, other_inc)])
  setkey(sim$growth_increments, gcids)
  
  return(invisible(sim))
}

# Update cohort groups for CBM annual event
UpdateCohortGroups <- function(sim){
  sim$cohortGroupKeep[, cohortGroupPrev := cohortGroupID]
  
  # Get the pools for the cohorts of the previous timestep
  cohorts <- merge(unique(sim$cohortGroupKeep[, .(pixelIndex, cohortGroupPrev)]),
                   sim$cbm_vars$state[, .(row_idx, age, species_id = species)],
                   by.x = "cohortGroupPrev",
                   by.y = "row_idx")
  
  # Match the cohort pools to this timestep cohorts based on pixel, age, and species.
  cohorts <- merge(
    cohorts[, age := age + 1],
    merge(sim$cohortDT[, .(pixelIndex, age, gcids)], sim$gcMeta[, .(gcids, species_id)]),
    by = c("pixelIndex", "age", "species_id"),
    allow.cartesian = TRUE
  )
  
  # Add spatial unit
  cohorts <- merge(cohorts, sim$standDT, by = "pixelIndex")
  cohorts[, cohortGroupID := .GRP, by = c("gcids", "row_idx")]
  
  # Handle DOM cohorts
  if(any(is.na(cohorts$gcids))){
    missingCohorts <- cohorts[is.na(gcids), ]
    # Check that the DOM cohorts have live pools close to 0
    if(any(sim$cbm_vars$pools[missingCohorts$cohortGroupPrev, c("Merch", "Foliage", "Other")] > 10^-6)) {
      stop("Some cohorts with positive above ground biomasses are missing.")
    }
    missingCohorts[, gcids := 0]
    missingCohorts[, age := 0]
    cohorts[is.na(gcids), ] <- missingCohorts
  }
  
  # Update cohortGroupKeep
  sim$cohortGroupKeep <- merge(
    sim$cohortGroupKeep[, cohortGroupID := NULL],
    cohorts[, .(pixelIndex, cohortGroupPrev, cohortGroupID)],
    by = c("pixelIndex", "cohortGroupPrev"),
    all.y = TRUE
  )
  setkey(sim$cohortGroupKeep, cohortID)
  
  # Update cohortGroups
  sim$cohortGroups <- unique(cohorts[, .(cohortGroupID, spatial_unit_id, age, gcids)])
  setkey(sim$cohortGroups, cohortGroupID)
  
  # Set cohort groups for the year
  sim$cohortGroupKeep[[as.character(time(sim))]] <- sim$cohortGroupKeep$cohortGroupID
  
  return(invisible(sim))

}
  
PrepareCBMvars <- function(sim){
  # 1. Prepare cbm pools
  # Get the pools of cohorts of the previous timestep
  new_cbm_pools <- merge(unique(sim$cohortGroupKeep[, .(cohortGroupID, cohortGroupPrev)]),
                         sim$cbm_vars$pools,
                         by.x = "cohortGroupPrev",
                         by.y = "row_idx",
                         all.x = TRUE)
  new_cbm_pools[, cohortGroupPrev := NULL]
  setnames(new_cbm_pools, old = "cohortGroupID", new = "row_idx")
  
  # Fill pools of new cohorts with 0s
  if(any(is.na(new_cbm_pools[, Merch]))) {
    new_cbm_pools$Input[is.na(new_cbm_pools$Input)] <- 1L
    setnafill(new_cbm_pools, fill = 0L)
  }
  
  # Aggregate DOM cohorts of the sharing pixel
  if(any(sim$cohortGroups$gcids == 0)){
    pool_columns <- setdiff(colnames(new_cbm_pools), "row_idx")
    new_cbm_pools <- new_cbm_pools[, lapply(.SD, sum), by = row_idx, .SDcols = pool_columns]
    new_cbm_pools$Input <- 1L
    # Set live pools to 0 for DOM cohorts.
    new_cbm_pools[row_idx %in% sim$cohortGroups[gcids == 0, cohortGroupID], c("Merch", "Foliage", "Other", "CoarseRoots", "FineRoots") := 0L]
  }
  new_cbm_pools <- unique(new_cbm_pools)
  setkey(new_cbm_pools, row_idx)
  
  # 2. Prepare cbm flux
  # Get the flux of the cohorts of the previous timestep
  new_cbm_flux <- merge(unique(sim$cohortGroupKeep[, .(cohortGroupID, cohortGroupPrev)]),
                        sim$cbm_vars$flux,
                        by.x = "cohortGroupPrev",
                        by.y = "row_idx",
                        all.x = TRUE)
  new_cbm_flux[, cohortGroupPrev := NULL]
  setnames(new_cbm_flux, old = "cohortGroupID", new = "row_idx")
  
  # Fill fluxes of new cohorts with 0s.
  if(any(is.na(new_cbm_flux))) {
    setnafill(new_cbm_flux, fill = 0L)
  }
  
  # Aggregate DOM cohorts of the sharing pixel
  if(any(sim$cohortGroups$gcids == 0)){
    flux_columns <- setdiff(colnames(new_cbm_flux), "row_idx")
    new_cbm_flux <- new_cbm_flux[, lapply(.SD, sum), by = row_idx, .SDcols = flux_columns]
  }
  new_cbm_flux <- unique(new_cbm_flux)
  setkey(new_cbm_flux, row_idx)
  
  # 3. Prepare cbm parameters
  # Get the mean annual temperature based on spatial unit.
  new_cbm_parameters <- merge(sim$cohortGroups,
                              sim$spinupSQL[, .(id, mean_annual_temperature)],
                              by.x = "spatial_unit_id",
                              by.y = "id",
                              all.x = TRUE)
  
  # Set no disturbance by default (will be changed later for disturbed cohorts)
  new_cbm_parameters[, disturbance_type := 0]
  
  # Get the increments
  new_cbm_parameters <- merge(new_cbm_parameters,
                              sim$growth_increments,
                              by = c("gcids", "age"),
                              all.x = TRUE)
  
  # For the DOM cohorts (gcids = 0) set increments to 0
  setnafill(new_cbm_parameters, fill = 0L, cols = c("merch_inc", "foliage_inc", "other_inc"))
  
  new_cbm_parameters <- new_cbm_parameters[, .(
    row_idx = cohortGroupID,
    mean_annual_temperature,
    disturbance_type,
    merch_inc,
    foliage_inc,
    other_inc
  )]
  setkey(new_cbm_parameters, row_idx)
  
  # Update parameters of disturbed cohorts
  distStands <- sim$standDT[!is.na(disturbance_type_id)]
  if (nrow(distStands) > 0) {
    
    # Get attributes for disturbed cohorts
    distCohorts <- merge(
      sim$cohortGroupKeep[, .(cohortID, pixelIndex, cohortGroupPrev, cohortGroupID)],
      distStands,
      by = "pixelIndex")
    # Remove new cohorts
    distCohorts <- distCohorts[!is.na(cohortGroupPrev)]
    
    new_cbm_parameters[unique(distCohorts$cohortGroupID), "disturbance_type"] <- distCohorts$disturbance_type_id
    # DC 29-04-2025: Not sure what should be the increments for disturbed cohorts.
    new_cbm_parameters[unique(distCohorts$cohortGroupID), merch_inc := 0L]
    new_cbm_parameters[unique(distCohorts$cohortGroupID), foliage_inc := 0L]
    new_cbm_parameters[unique(distCohorts$cohortGroupID), other_inc := 0L]
  }
  
  # 4. Prepare cbm state
  # Get the state of the cohorts of the previous timestep
  new_cbm_state <-  merge(unique(sim$cohortGroupKeep[, .(cohortGroupID, cohortGroupPrev)]),
                          sim$cbm_vars$state,
                          by.x = "cohortGroupPrev",
                          by.y = "row_idx",
                          all.x = TRUE)
  new_cbm_state[, cohortGroupPrev := NULL]
  setnames(new_cbm_state, old = "cohortGroupID", new = "row_idx")
  
  # Change the state of DOM cohorts
  if(any(sim$cohortGroups$gcids == 0)){
    DOMcohorts <- sim$cohortGroups[gcids == 0, cohortGroupID]
    new_cbm_state[row_idx %in% DOMcohorts, age := 0]
    new_cbm_state[row_idx %in% DOMcohorts, species := 0]
    new_cbm_state[row_idx %in% DOMcohorts, sw_hw := 0]
    new_cbm_state[row_idx %in% DOMcohorts, time_since_last_disturbance := 0]
    new_cbm_state[row_idx %in% DOMcohorts, time_since_land_use_change  := -1]
    new_cbm_state[row_idx %in% DOMcohorts, last_disturbance_type := -1]
    new_cbm_state <- unique(new_cbm_state)
  }
  
  # Set the state of the new cohorts
  if(any(is.na(new_cbm_state))) {
    newCohorts_cbm_state <- new_cbm_state[is.na(area), ]
    setnafill(newCohorts_cbm_state, fill = 1L, cols = c("area", "last_disturbance_type", "enabled"))
    setnafill(newCohorts_cbm_state, fill = -1L, cols = c("land_class_id", "time_since_land_use_change")) 
    
    # Get growth curve information
    newCohorts_cbm_state[sim$cohortGroups, on = c("row_idx" = "cohortGroupID"), `:=`(
      spatial_unit_id = fifelse(is.na(spatial_unit_id), i.spatial_unit_id, spatial_unit_id),
      age  = fifelse(is.na(age),  i.age - 1,  age) # age minus 1 because we added 1 in cohortGroups
    )]
    newCohort_gcids <- sim$cohortGroups[newCohorts_cbm_state$row_idx, gcids]
    newCohorts_gcMeta <- sim$gcMeta[match(newCohort_gcids, sim$gcMeta$gcids)]
    newCohorts_cbm_state[, species := newCohorts_gcMeta$species_id]
    newCohorts_cbm_state[, sw_hw := as.integer(newCohorts_gcMeta$sw_hw == "sw")]
    newCohorts_cbm_state[, time_since_last_disturbance := age]
    
    # Combine with cohorts that were present before
    new_cbm_state <- rbind(
      new_cbm_state[!is.na(area),],
      newCohorts_cbm_state
    )
  }
  new_cbm_state <- unique(new_cbm_state)
  setkey(new_cbm_state, row_idx)
  
  # 5. Put in cbm_vars
  cbm_vars <- list(
    pools = new_cbm_pools[!is.na(row_idx)],
    flux = new_cbm_flux[!is.na(row_idx)],
    parameters = new_cbm_parameters[!is.na(row_idx)],
    state = new_cbm_state[!is.na(row_idx)]
  )
  
  sim$cbm_vars <- cbm_vars
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  
  # NFI params. Used to split total biomass into biomass of the three CBM
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
  
  return(invisible(sim))
}
