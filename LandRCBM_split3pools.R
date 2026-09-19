defineModule(sim, list(
  name = "LandRCBM_split3pools",
  description = paste("Takes total aboveground biomass provided by LandR and divides",
                      "it into the 3 required CBM pools."),
  keywords = "",
  authors = c(
    person("Celine",    "Boisvenue", email = "cboivenue@gmail.com",               role = c("aut", "cre")),
    person("Dominique", "Caron",     email = "dominique.caron@nrcan-rncan.gc.ca", role = c("aut")),
    person("Susan",     "Murray",    email = "murray.e.susan@gmail.com",          role = c("ctb")),
    person("Camille",   "Giuliano",  email = "camsgiu@gmail.com",                 role = c("ctb")),
    person("Alex M",    "Chubaty",   email = "achubaty@for-cast.ca",              role = c("ctb"))
  ),
  childModules = character(0),
  version = list(LandRCBM_split3pools = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandRCBM_split3pools.Rmd"), ## same file
  reqdPkgs = list("PredictiveEcology/SpaDES.core", "reproducible (>= 2.1.2)", "data.table", "ggplot2", "terra",
                  "SpaDES.tools (>= 1.0.0.9001)", "PredictiveEcology/CBMutils@development (>= 2.5.6)"),
  parameters = bindrows(
    defineParameter("minMerchantableAge", "integer", 15L, NA, NA,
                    "Minimum age for which a cohort can have wood considered merchantable."),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".plotNumPixGroup", "integer", 10L, NA, NA,
                    "When plotting the yield curves, this is how many unique pixel groups will ",
                    "be randomly selected and plotted."),
    defineParameter(".plotMaps", "logical", TRUE, NA, NA,
                    "Controls whether maps should be plotted or not. Set to `FALSE` if `P(sim)$.plots == NA`"),
    defineParameter(".useCache", "character", "postSpinupAdjustBiomass", NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    expectsInput(
      objectName = "cohortData", objectClass = "data.table",
      desc = "Total above ground biomass (g/m^2) of each cohorts by pixel groups.",
      columns = c(
        speciesCode    = "Species code used by LandR",
        ecoregionGroup = "The LandR spatial units (i.e., ecoregion).",
        age            = "Age of the cohort.",
        B              = "Total above ground biomass in (g/m^2).",
        pixelGroup     = "Id of the group of pixels sharing the same cohort composition and ecoregion, used in LandR.",
        totalBiomass   = "Total above ground biomass in the pixel group."
      )
    ),
    expectsInput(
      objectName = "pixelGroupMap", objectClass = "SpatRaster",
      desc = "Map of pixel group from LandR. Group of pixels share the same cohort composition.",
    ),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "SpatRaster",
      desc = "Template raster to use for simulations."
    ),
    expectsInput(
      objectName = "standDT", objectClass = "data.table",
      desc = "Table of stand attributes.",
      columns = c(
        pixelIndex   = "Stand ID",
        admin_name   = "Canada province or territory name",
        admin_abbrev = "Canada province or territory 2-character abbreviation",
        eco_id       = "Canada ecozone ID"
      )
    ),
    expectsInput(
      objectName = "yieldTablesCumulative", objectClass = "data.table",
      desc = "Yield Tables intended to supply the requirements for a CBM spinup.",
      columns = c(
        yieldTableIndex = "Id of the group of pixels sharing yield tables.",
        age             = "Age of species going from 0 to their longevity.",
        speciesCode     = "Species code used by LandR.",
        biomass         = "Above ground biomass in g/m^2."
      )
    ),
    expectsInput(
      objectName = "yieldTablesId", objectClass = "data.table",
      desc ="Table linking `yieldTablesCumulative` to pixels.",
      columns = c(
        pixelIndex      = "Integer id of the pixel.",
        yieldTableIndex = "Id of the group of pixels sharing yield tables."
      )
    ),
    expectsInput(
      objectName = "treedFirePixelTableSinceLastDisp", objectClass = "data.table",
      desc = "Optional. Table of pixels that have been disturbed by wildfire."
    ),
    expectsInput(
      objectName = "sppEquiv", objectClass = "data.table",
      desc = "Optional. Table of species equivalencies. See `LandR::sppEquivalencies_CA`."
    ),
    expectsInput(
      objectName = "table6tb", objectClass = "data.table",
      desc = paste(
        "Boudewyn et al. (2007) an alternative set of proportion model parameters", 
        "to use when total biomass per hectare in tonnes (tb = stem wood + stem bark + branches + foliage)", 
        "is the independent variable instead of gross merchantable volume per hectare."),
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv"
    ),
    expectsInput(
      objectName = "table7tb", objectClass = "data.table",
      desc = paste(
        "Boudewyn et al. (2007) an alternative set of caps on proportion models",
        "to use in conjunction with the alternative set of proportion models",
        "when total biomass per hectare in tonnes (tb = stem wood + stem bark + branches + foliage)", 
        "is the independent variable instead of gross merchantable volume per hectare."),
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv"
    ),
    expectsInput(
      objectName = "tableMerch", objectClass = "data.table",
      desc = paste(
        "Parameters to estimate the proportion of stemwood biomass that is merchantable", 
        "estimated by approximating the relationship between stemwood biomass and",
        "nonmerchfactor predicted by equation 2 of Boudewyn et al. (2007)."),
      sourceURL = "https://drive.google.com/file/d/1wa2QMd7Eo-bPpfigchdpPPPxo7NVpPiC"
    )
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "cohortDT",
      objectClass = "data.table",
      desc = "Table of cohort attributes used and updated by CBM_core."
    ),
    createsOutput(
      objectName = "gcIncrements",
      objectClass = "data.table",
      desc = "Growth curve carbon increments (t/ha) used by CBM_core."
    ),
    createsOutput(
      objectName = "gcMeta",
      objectClass = "data.table",
      desc = "Growth curve metadata used by CBM_core in the spinup."
    ),
    createsOutput(
      objectName = "disturbanceEvents",
      objectClass = "data.table",
      desc = "Disturbance events used by CBM_core."
    ),
    createsOutput(
      objectName = "summaryAGB",
      objectClass = "data.table",
      desc = paste("Sum of carbon mass for each species and above ground", 
                   "pool at each timestep across the landscape. Columns are `year`,",
                   "`speciesCode`, `merch`, `foliage`, `other`.")
    )
  )
))

doEvent.LandRCBM_split3pools = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # split initial cohortData and yield tables
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "splitInit", eventPriority = 3)
      
      # adjust that the live biomass post-CBM spinup with the biomass in LandR
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "postSpinupAdjustBiomass", eventPriority = 5.5)
      
      # split AGB of cohorts into pools 
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "annualIncrements", eventPriority = 8)
      
      # summarize simulation 
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "summarizeAGBPools", eventPriority = 10)
      
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
    splitInit = {
      
      # Prepare yield tables for CBM spinup
      sim <- SplitYieldTables(sim)
    },
    postSpinupAdjustBiomass = {
      
      # Adjust biomass after CBM spinup
      sim <- PostSpinupAdjustBiomass(sim)
      
    },
    annualIncrements = {
      
      # set disturbances 
      sim <- AnnualDisturbances(sim)
      
      # split AGB of cohorts into pools and prepare for CBM_core annual event
      sim <- AnnualIncrements(sim)
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 9, "LandRCBM_split3pools", "annualIncrements")
    },
    
    summarizeAGBPools = {
      
      AGB <- sim$cohortDT[gcID >= 1, .(
        pixelIndex, gcID,
        merch   = pools.SoftwoodMerch   + pools.HardwoodMerch,
        foliage = pools.SoftwoodFoliage + pools.HardwoodFoliage,
        other   = pools.SoftwoodOther   + pools.HardwoodOther
      )]
      AGB[sim$gcIncrements, speciesCode := speciesCode, on = "gcID"]
      
      sumBySpecies <- AGB[, lapply(.SD, sum, na.rm = TRUE), by = speciesCode, .SDcols = c("merch", "foliage", "other")]
      sumBySpecies$year <- time(sim)[1]
      
      sim$summaryAGB <- rbind(
        sim$summaryAGB,
        sumBySpecies
      )
      
      # do this for each timestep
      sim <- scheduleEvent(sim, time(sim) + 1, eventPriority = 10, "LandRCBM_split3pools", "summarizeAGBPools")
    },
    plotYC = {
      
      # plot the yield tables
      sim <- PlotYieldTables(sim)
      
      # plot the yield tables with pools separated
      sim <- PlotYieldTablesPools(sim)
    },
    plotMaps = {
      
      # get the sum of each pool per pixelGroups
      poolSum <- sim$cohortDT[gcID >= 1, .(
        pixelIndex, gcID,
        merch   = sum(pools.SoftwoodMerch   + pools.HardwoodMerch),
        foliage = sum(pools.SoftwoodFoliage + pools.HardwoodFoliage),
        other   = sum(pools.SoftwoodOther   + pools.HardwoodOther)
      ), by = "pixelIndex"]
      
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
        increments <- sim$cohortDT[sim$gcIncrements, on = c("gcID", "age")]
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

# Prepare yield tables for CBM spinup
SplitYieldTables <- function(sim) {
  
  # Step 1: Spatial Matching and Cohort/Stand Data Preparation -----------------
  # Link yield curve IDs (yieldTableIndex) to CBM spatial units 
  # and generate initial cohort/stand data structures.
  
  # 1.1. Generate the cohort-level attributes (`cohortDT`).
  #      This links individual cohorts (pixelGroup x species combinations)
  #      to their corresponding growth curve IDs (`gcID`).
  cohortDT <- generateCohortDT(sim$cohortData, sim$pixelGroupMap, sim$standDT, sim$yieldTablesId)
  
  # 1.2. Create and store metadata about growth curves (`sim$gcMeta`).
  #      Links gcID to species information.
  sim$gcMeta <- unique(cohortDT[, .(gcID, admin_abbrev, eco_id, yieldTableIndex, speciesCode)])
  sim$gcMeta <- cbind(
    sim$gcMeta,
    CBMutils::sppMatch(
      sim$gcMeta$speciesCode, sppEquiv = sim$sppEquiv,
      match = "LandR", return = c("Broadleaf", "CanfiCode"))[
        , .(sw = !Broadleaf, canfi_species = CanfiCode)]
  )
  setkey(sim$gcMeta, gcID)
  setcolorder(sim$gcMeta)
  
  # 1.3. Store essential cohort information in simList.
  cohortDT <- cohortDT[, .(pixelIndex, age, gcID, cohort_index = 0)]
  sim$cohortDT <- cohortDT
  
  # Step 2: Splitting AGB Curves into CBM Pools --------------------------------
  # Convert the total Above-Ground Biomass (AGB) yield curves into cumulative biomass
  # for the three CBM above ground pools: Merchantable (merch), Foliage, and Other.
  
  # 2.1. Prepare table for CBM pool splitting function.
  #      Rename the primary biomass column to 'B' as expected by CBMutils.
  cumPools <- merge(
    sim$gcMeta, 
    sim$yieldTablesCumulative[, .(yieldTableIndex, speciesCode, age, biomass)], 
    by = c("speciesCode", "yieldTableIndex"),
    allow.cartesian = TRUE)
  cumPools[, speciesCode := NULL]
  setnames(cumPools,
           old = c("admin_abbrev", "eco_id",  "biomass"),
           new = c("juris_id",     "ecozone", "B"))
  
  # Convert biomass units from g/m^2 to tonnes/ha: 1 g/m^2 = 0.01 tonnes/ha
  cumPools[, B := B / 100]
  
  # 2.2. Split AGB ('B') into cumulative CBM pools (merch, foliage, other).
  #      Uses equations from Boudewyn et al. 2007 adjusted to use total above
  #      ground biomass as input, implemented in CBMutils.
  cumPools[age == 0 & B <= 0.01, B := 0]
  CBMutils::cumPoolsCreateAGB(
    cumPools,
    pixGroupCol = "gcID",
    bTable6tb   = sim$table6tb,
    bTable7tb   = sim$table7tb,
    tableMerch  = sim$tableMerch
  )
  
  # Step 3: Calculating Annual Increments --------------------------------------
  # Calculate the year-to-year increment in biomass for each above ground 
  # biomass pool. These increments drive the spinup dynamics.
  
  # 3.1. Define pool and increment column names.
  poolCols <- c("merch", "foliage", "other")
  colAGB  <- c("merch_inc", "foliage_inc", "other_inc")
  
  # 3.2. Calculate increments using `diff`.
  data.table::setkey(cumPools, gcID, age)
  cumPools[, (colAGB) := lapply(.SD, function(x) c(0, diff(x))), .SDcols = poolCols, by = "gcID"]
  
  # 3.3. Final selection and ordering of columns for `sim$gcIncrements`.
  sim$gcIncrements <- cumPools[, .(gcID, age, merch, foliage, other, merch_inc, foliage_inc, other_inc)]
  
  # Return simList
  return(invisible(sim))
}

# Adjust biomass after CBM spinup
PostSpinupAdjustBiomass <- function(sim){
  
  colJoin  <- c("pixelIndex", "speciesCode", "age")
  colAGB   <- do.call(c, lapply(c("Softwood", "Hardwood"), paste0, c("Merch", "Foliage", "Other")))
  colRoots <- do.call(c, lapply(c("Softwood", "Hardwood"), paste0, c("CoarseRoots", "FineRoots")))
  
  # Split initial above ground biomass
  AGB <- splitCohortData(
    cohortData    = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    standDT       = sim$standDT[, .(pixelIndex, juris_id = admin_abbrev, ecozone = eco_id)],
    table6tb      = sim$table6tb,
    table7tb      = sim$table7tb,
    tableMerch    = sim$tableMerch,
    sppEquiv      = sim$sppEquiv
  )
  
  # Split by SW/HW and calculate root C
  AGB[sim$gcMeta, sw := sw, on = "speciesCode"]
  data.table::setnames(AGB, c("merch", "foliage", "other"), colAGB[1:3])
  AGB[sw==FALSE, (colAGB[4:6]) := .SD, .SDcols = colAGB[1:3]]
  AGB[sw==FALSE, (colAGB[1:3]) := 0]
  data.table::setnafill(AGB, type = "const", fill = 0, cols = colAGB)
  AGB <- cbind(AGB, CBMutils::calcRootC(AGB))
  
  AGB <- AGB[, .SD, .SDcols = c(colJoin, colAGB, colRoots)]
  data.table::setnames(AGB, c(colAGB, colRoots), paste0("pools.", c(colAGB, colRoots)))
  sim$cohortDT[, paste0("pools.", c(colAGB, colRoots)) := NULL]
  sim$cohortDT[sim$gcMeta, speciesCode := speciesCode, on = "gcID"]
  sim$cohortDT <- sim$cohortDT[AGB, on = colJoin]
  
  # Return simList
  return(invisible(sim))
}

# Set yearly disturbances
AnnualDisturbances <- function(sim){
  
  distEvents <- data.table::data.table()
  
  # Wildfire
  if (!is.null(sim$treedFirePixelTableSinceLastDisp)){
    
    distEvents <- rbind(
      distEvents, 
      sim$treedFirePixelTableSinceLastDisp[burnTime == time(sim), .(
        year    = time(sim),
        pixelIndex,
        disturbance_type_name = "Wildfire"
      )],
      fill = TRUE)
  }
  
  if (nrow(distEvents) > 0){
    
    # Apply disturbance to all eligible cohorts
    distEvents$proportion  <- 1L
    
    # Disable merging of cohorts after disturbance
    ## This will use cohort_proportion to recalculate pool values
    distEvents$enable_merge <- 0L
    
    sim$disturbanceEvents <- rbind(sim$disturbanceEvents, distEvents, fill = TRUE)
  }
  
  return(invisible(sim))
  
}

# Process yearly vegetation inputs
AnnualIncrements <- function(sim){
  
  # Split current total above ground biomass
  ## Set age to age at beginning of year
  AGB <- splitCohortData(
    cohortData    = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    standDT       = sim$standDT[, .(pixelIndex, juris_id = admin_abbrev, ecozone = eco_id)],
    table6tb      = sim$table6tb,
    table7tb      = sim$table7tb,
    tableMerch    = sim$tableMerch,
    sppEquiv      = sim$sppEquiv
  )
  AGB[, age := age - 1]
  
  # Get biomass for the previous year
  if (!"speciesCode" %in% names(sim$cohortDT)){
    sim$cohortDT[sim$gcIncrements, speciesCode := speciesCode, on = "gcID"]
    on.exit(sim$cohortDT[, speciesCode := NULL])
  }
  
  cohortDT <- sim$cohortDT[, .(
    pixelIndex, gcID, speciesCode, age, 
    merchTminus1   = pools.SoftwoodMerch   + pools.HardwoodMerch, 
    foliageTminus1 = pools.SoftwoodFoliage + pools.HardwoodFoliage, 
    otherTminus1   = pools.SoftwoodOther   + pools.HardwoodOther
  )]
  cohortDT[, BTminus1 := merchTminus1 + foliageTminus1 + otherTminus1]
  
  # Join between current biomass and previous biomass
  cohortDT <- merge(
    AGB,
    cohortDT, 
    by = c("pixelIndex", "speciesCode", "age"),
    all.x = TRUE, # Include new cohorts
    all.y = TRUE  # Include disturbed and DOM cohorts
  )
  data.table::setnafill(cohortDT, fill = 0, cols = c(
    "merch", "foliage", "other", "merchTminus1", "foliageTminus1", "otherTminus1"))
  
  # Calculate increments by subtracting previous from current
  cohortDT[, `:=`(
    merch_inc   = merch   - merchTminus1,
    foliage_inc = foliage - foliageTminus1,
    other_inc   = other   - otherTminus1
  )]
  cohortDT[, c(
    "merch", "foliage", "other", "merchTminus1", "foliageTminus1", "otherTminus1") := NULL]
  
  ## Check that DOM cohorts have 0 biomass
  if (nrow(cohortDT[gcID == 0 & BTminus1 > 0]) > 0) stop("DOM cohorts have remaining biomass")
  
  # Set gcID for DOM cohorts so that increments == 0
  cohortDT[is.na(B) & BTminus1 == 0, gcID := 0]
  
  # Set gcID for disturbed cohorts so that increments == 0
  ## This assumes that all disturbances are stand replacing for the whole pixel
  if (!is.null(sim$disturbanceEvents)){
    cohortDT[
      pixelIndex %in% sim$disturbanceEvents[year == time(sim), pixelIndex],
      gcID := 0
    ]
  }
  
  # Create unique gcID for active cohorts
  cohortDT[!gcID %in% 0, gcID := as.integer(.GRP), by = c(
    "speciesCode", "age", "merch_inc", "foliage_inc", "other_inc")]
  
  # Set cohorts
  sim$cohortDT[, gcID := NULL]
  sim$cohortDT <- merge(
    cohortDT[, .(pixelIndex, speciesCode, age, gcID, BTminus1)],
    sim$cohortDT,
    by = c("pixelIndex", "speciesCode", "age"),
    all.x = TRUE, # Include new cohorts
    all.y = TRUE  # Keep disturbed and DOM cohorts
  ) |> unique() # Prevents duplicated DOM cohorts

  # DOM cohorts: aggregate per pixel
  poolCols <- names(sim$cohortDT)[grepl("^pools\\.", names(sim$cohortDT))]
  
  sim$cohortDT[, DOM := gcID == 0 & BTminus1 == 0]
  sim$cohortDT[DOM==TRUE, c("speciesCode", "age") := list(NA, 0)]
  
  if (anyDuplicated(sim$cohortDT[DOM==TRUE, pixelIndex]) > 0){

    sim$cohortDT <- rbind(
      sim$cohortDT[DOM==FALSE],
      sim$cohortDT[DOM==TRUE, lapply(.SD, sum), .SDcols = poolCols, by = c(
        setdiff(names(sim$cohortDT), c(
          poolCols, "cohort_index",
          "state.time_since_last_disturbance", "state.time_since_land_class_change"
        )))],
      fill = TRUE)
  }
  sim$cohortDT[, BTminus1 := NULL]
  sim$cohortDT[, DOM      := NULL]
  
  # New cohorts: initiate pools
  sim$cohortDT[is.na(pools.SoftwoodMerch), (poolCols) := 0]

  # Set other cohort attributes
  sim$cohortDT[, cohort_index      := .GRP, by = c("speciesCode", "age")]
  sim$cohortDT[, cohort_proportion := 0]
  
  # Set gcIncrements
  sim$gcIncrements <- unique(
    cohortDT[gcID != 0, .(gcID, speciesCode, age, merch_inc, foliage_inc, other_inc)]
  )
  sim$gcIncrements[, sw := !CBMutils::sppMatch(
    speciesCode, sppEquiv = sim$sppEquiv, match = "LandR", return = "Broadleaf")$Broadleaf]
  
  sim$gcIncrements <- rbind(
    data.table::data.table(
      gcID        = 0L,
      speciesCode = NA_character_,
      age         = NA_real_,
      sw          = TRUE,
      merch_inc   = 0,
      foliage_inc = 0,
      other_inc   = 0
    ),
    sim$gcIncrements,
    fill = TRUE)
  
  data.table::setkey(sim$gcIncrements, gcID, age)
  sim$gcMeta <- NULL
  
  # Return simList
  return(invisible(sim))
}

# Plot yield table curves
PlotYieldTables <- function(sim){
  nPixGroups <- length(unique(sim$yieldTablesId$yieldTableIndex))
  nPlots <- P(sim)$.plotNumPixGroup
  if (nPlots <= 0){
    stop("numPlots needs to be a positive integer")
  } else if (nPlots > nPixGroups) {
    message(".plotNumPixGroup is greater than the number of pixel groups, ",
            "plotting all pixelgroups.")
    nPlots <- nPixGroups
  } 
  pixGroupToPlot <- sample(unique(sim$yieldTablesId$yieldTableIndex), nPlots)
  
  mod$yieldTableIndexPlotted <- pixGroupToPlot
  
  # plot
  Plots(sim$yieldTablesCumulative[yieldTableIndex %in% pixGroupToPlot], 
        fn = gg_yieldCurves,
        types = P(sim)$.plots,
        filename = paste("yieldCurves"),
        title = paste("Yield curves for", nPlots, "randomly selected pixel groups")
  )
  
  # Return simList
  return(invisible(sim))
}

# Plot the curves that are directly out of the Boudewyn-translation
PlotYieldTablesPools <- function(sim){
  
  # We want to plot the same cohorts across figures
  pixGroupToPlot <- mod$yieldTableIndexPlotted
  yieldTablePools <- sim$gcIncrements[sim$gcMeta, on = "gcID"][yieldTableIndex %in% pixGroupToPlot]
  
  # plot total yield curves
  plot_dt <- melt(
    yieldTablePools, 
    id.vars = c("yieldTableIndex", "speciesCode", "age"),
    measure.vars = c("merch", "foliage", "other"),
    variable.name = "pool",
    value.name = "B"
  )
  
  Plots(plot_dt, 
        fn = gg_yieldCurvesPools,
        types = P(sim)$.plots,
        filename = "yieldCurvePools",
        title = paste("Yield curves for", length(unique(plot_dt$yieldPixelGroup)), "randomly selected pixel groups")
  )
  
  # plot increments
  plot_dt <- melt(
    yieldTablePools, 
    id.vars = c("yieldTableIndex", "age", "speciesCode", "gcID"),
    measure.vars = c("merch_inc", "foliage_inc", "other_inc"),
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

.inputObjects <- function(sim) {
  
  if (isTRUE(P(sim, "fixedCohorts", "CBM_core"))) stop(
    "CBM_core parameter fixedCohorts must be FALSE to run LandRCBM_split3pools")
  
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  
  # NFI params. Used to split total biomass into biomass of the three CBM
  #                above ground biomass pools.
  if (!suppliedElsewhere("table6tb", sim)) {
    sim$table6tb <- prepInputs(
      url = extractURL("table6tb"),
      fun = data.table::fread(targetFile, verbose = FALSE),
      destinationPath = inputPath(sim),
      targetFile = "appendix2_table6_tb.csv",
      overwrite = TRUE
    ) |> Cache(userTags = "prepInputsTable6tb")
  }
  
  if (!suppliedElsewhere("table7tb", sim)) {
    sim$table7tb <- prepInputs(
      url = extractURL("table7tb"),
      fun = data.table::fread(targetFile, verbose = FALSE),
      destinationPath = inputPath(sim),
      targetFile = "appendix2_table7_tb.csv",
      overwrite = TRUE
    ) |> Cache(userTags = "prepInputsTable7tb")
  }
  
  if (!suppliedElsewhere("tableMerch", sim)) {
    sim$tableMerch <- prepInputs(
      url = extractURL("tableMerch"),
      fun = data.table::fread(targetFile, verbose = FALSE),
      destinationPath = inputPath(sim),
      targetFile = "merchantabilityParams.csv",
      overwrite = TRUE
    ) |> Cache(userTags = "prepInputsTableMerch")
    sim$tableMerch <- cbind(sim$tableMerch, minAge = P(sim)$minMerchantableAge)
  }
  
  # Return simList
  return(invisible(sim))
}
