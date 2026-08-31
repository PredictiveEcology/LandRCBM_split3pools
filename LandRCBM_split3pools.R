defineModule(sim, list(
  name = "LandRCBM_split3pools",
  description = paste("Takes total aboveground biomass provided by LandR and divides",
                      "it into the 3 required CBM pools."),
  keywords = "",
  authors = c(
    person("Celine", "Boisvenue", email = "cboivenue@gmail.com", role = c("aut", "cre")),
    person("Dominique", "Caron", email = "dominique.caron@nrcan-rncan.gc.ca", role = c("aut")),
    person("Susan",   "Murray",    email = "murray.e.susan@gmail.com",           role = c("ctb")),
    person("Camille", "Giuliano",  email = "camsgiu@gmail.com",                  role = c("ctb")),
    person("Alex M", "Chubaty", email = "achubaty@for-cast.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(LandRCBM_split3pools = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandRCBM_split3pools.Rmd"), ## same file
  reqdPkgs = list("PredictiveEcology/SpaDES.core", "reproducible (>= 2.1.2)", "data.table", "ggplot2", "terra",
                  "SpaDES.tools (>= 1.0.0.9001)", "PredictiveEcology/CBMutils@development (>= 2.5.5.9001)"),
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
        speciesCode = "Species code used by LandR",
        ecoregionGroup = "The LandR spatial units (i.e., ecoregion).",
        age = "Age of the cohort.",
        B = "Total above ground biomass in (g/m^2).",
        pixelGroup = "Id of the group of pixels sharing the same cohort composition and ecoregion, used in LandR.",
        totalBiomass = "Total above ground biomass in the pixel group."
      )
    ),
    expectsInput(
      objectName = "pixelGroupMap", objectClass = "SpatRaster",
      desc = paste("Map of pixel group from LandR. Group of pixels that shares the same ",
                   "cohort composition.")
    ),
    expectsInput(
      objectName = "rasterToMatch", objectClass =  "SpatRaster",
      desc = "Template raster to use for simulations; defaults is the RIA study area."
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
    ),
    expectsInput(
      objectName = "sppEquiv", objectClass = "data.table",
      desc = "Optional. Table of species equivalencies. See `LandR::sppEquivalencies_CA`."
    ),
    expectsInput(
      objectName = "treedFirePixelTableSinceLastDisp", objectClass = "data.table",
      desc = "TODO"
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
      objectName = "tableMerchantability", objectClass = "data.table",
      desc = paste("Parameters to estimate the proportion of stemwood that is merchantable,",
                   "Estimated by approximating the relationship between stemwood biomass and",
                   "nonmerchfactor predicted by equation 2 of Boudewyn et al., 2007."),
      sourceURL = "https://drive.google.com/file/d/1wa2QMd7Eo-bPpfigchdpPPPxo7NVpPiC/view?usp=drive_link"
    )
  ),
  outputObjects = bindrows(
    createsOutput(
      objectName = "cohortDT",
      objectClass = "data.table",
      desc = paste("Cohort-level information.",
                   "Columns are `cohortID`, `pixelIndex`, `age`, and `gcID`.")
    ),
    createsOutput(
      objectName = "gcMeta",
      objectClass = "data.table",
      desc = paste("Growth curve-level information.",
                   "Columns are `gcID`, `speciesCode`, and `sw`")
    ),
    createsOutput(
      objectName = "gcIncrements",
      objectClass = "data.table",
      desc = paste("Increments (metric tonnes of carbon/ha) in each pool",
                   "for each pixel and cohort. Gets updated at each timestep.",
                   "Columns are `gcID`, `age`,`merch_inc`, `foliage_inc`, and `other_inc`.")
    ),
    createsOutput(
      objectName = "disturbanceMeta",
      objectClass = "data.table",
      desc = "TODO"
    ),
    createsOutput(
      objectName = "disturbanceEvents",
      objectClass = "data.table",
      desc = "TODO"
    ),
    createsOutput(
      objectName = "aboveGroundBiomass",
      objectClass = "data.table",
      desc = paste("Above ground biomass (metric tonnes of carbon/ha) in each pool",
                   "for each pixel and cohort. Gets updated at each timestep.",
                   "Columns are `pixelIndex`, `speciesCode`, `age`, `merch`, `foliage`, and `other`.")
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
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_split3pools", "postSpinupAdjustBiomass", eventPriority = 6)
      
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
      
      # split AGB of cohorts into pools and prepare for CBM_core annual event
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
    plotYC = {
      
      # plot the yield tables
      sim <- PlotYieldTables(sim)
      
      # plot the yield tables with pools separated
      sim <- PlotYieldTablesPools(sim)
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
  
  # 1.1. Split initial above ground biomass
  sim$aboveGroundBiomass <- splitCohortData(
    cohortData = sim$cohortData,
    pixelGroupMap = sim$pixelGroupMap,
    standDT = sim$standDT[,.(pixelIndex, juris_id = admin_abbrev, ecozone = eco_id)],
    table6 = sim$table6,
    table7 = sim$table7,
    tableMerchantability = sim$tableMerchantability,
    sppEquiv = sim$sppEquiv
  )
  
  # 1.2. Generate the cohort-level attributes (`cohortDT`).
  #      This links individual cohorts (pixelGroup x species combinations)
  #      to their corresponding growth curve IDs (`gcID`).
  cohortDT <- generateCohortDT(sim$cohortData, sim$pixelGroupMap, sim$standDT, sim$yieldTablesId)
  
  # 1.3. Store essential cohort information in simList.
  sim$cohortDT <- cohortDT[, .(cohortID, pixelIndex, age, gcID)]
  
  # 1.4. Create and store metadata about growth curves (`sim$gcMeta`).
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
  
  rm(cohortDT)
  
  # Step 2: Splitting AGB Curves into CBM Pools --------------------------------
  # Convert the total Above-Ground Biomass (AGB) yield curves into cumulative biomass
  # for the three CBM above ground pools: Merchantable (merch), Foliage, and Other.
  
  # 2.1. Prepare table for CBM pool splitting function.
  #      Rename the primary biomass column to 'B' as expected by CBMutils.
  allInfoYieldTables <- merge(
    sim$gcMeta, 
    sim$yieldTablesCumulative[, .(yieldTableIndex, speciesCode, age, biomass)], 
    by = c("speciesCode", "yieldTableIndex"))
  setnames(allInfoYieldTables,
           old = c("admin_abbrev", "eco_id",  "biomass"),
           new = c("juris_id",     "ecozone", "B"))
  
  # Convert biomass units from g/m^2 to tonnes/ha: 1 g/m^2 = 0.01 tonnes/ha
  allInfoYieldTables[, B := B / 100]
  
  # 2.2. Split AGB ('B') into cumulative CBM pools (merch, foliage, other).
  #      Uses equations from Boudewyn et al. 2007 adjusted to use total above
  #      ground biomass as input, implemented in CBMutils.
  cumPools <- CBMutils::cumPoolsCreateAGB(allInfoAGBin = allInfoYieldTables,
                                          table6 = sim$table6,
                                          table7 = sim$table7,
                                          tableMerchantability = sim$tableMerchantability,
                                          pixGroupCol = "gcID")
  cumPools[, speciesCode := NULL]
  
  # 2.3. Ensure annual resolution by filling missing ages (especially age 0).
  minAgeDT <- cumPools[,.(minAge = max(0L, min(age) - 1L)), by = "gcID"]
  # Create sequences from 0 up to (but not including) the minimum age found.
  # Filter out cases where minAge is already 0.
  fillAgesDT <-  minAgeDT[,.(age = seq(from = 0, to = minAge, by = 1)), by = "gcID"]
  # Only proceed if there are ages to fill
  if (nrow(fillAgesDT) > 0) {
    # Create a table with the missing ages and zero biomass for all pools.
    zeroBiomassDT <- fillAgesDT[, .(merch = 0, foliage = 0, other = 0), by = .(gcID, age)] 
    
    # Combine the original curves with the filled zero-biomass ages.
    cumPools <- rbindlist(list(cumPools, zeroBiomassDT), use.names = TRUE)
  }
  
  # Step 3: Calculating Annual Increments --------------------------------------
  # Calculate the year-to-year increment in biomass for each above ground 
  # biomass pool. These increments drive the spinup dynamics.
  
  # 3.1. Define pool and increment column names.
  poolCols <- c("merch", "foliage", "other")
  incCols <- c("merch_inc", "foliage_inc", "other_inc")
  
  # 3.2. Calculate increments using `diff`.
  setkey(cumPools, gcID, age)
  cumPools[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = poolCols, by = "gcID"]
  cumPools[age == 0, c("merch_inc", "foliage_inc", "other_inc") := list(0, 0, 0)]
  
  # 3.3. Final selection and ordering of columns for `sim$gcIncrements`.
  sim$gcIncrements <- cumPools[,.(gcID, age, merch, foliage, other, merch_inc, foliage_inc, other_inc)]
  data.table::setkey(sim$gcIncrements, gcID, age)
  
  # Return simList
  return(invisible(sim))
}

# Adjust biomass after CBM spinup
PostSpinupAdjustBiomass <- function(sim){
  
  colJoin    <- c("pixelIndex", "speciesCode", "age")
  colReplace <- do.call(c, lapply(c("Softwood", "Hardwood"), function(x) paste0(
    x, c("Merch", "Foliage", "Other", "CoarseRoots", "FineRoots"))))
  
  AGB <- data.table::copy(sim$aboveGroundBiomass)
  
  AGB[, sw := !CBMutils::sppMatch(AGB$speciesCode, sppEquiv = sim$sppEquiv, match = "LandR", return = "Broadleaf")$Broadleaf]
  AGB[, SoftwoodMerch   := data.table::fifelse( sw, merch,   0)]
  AGB[, SoftwoodFoliage := data.table::fifelse( sw, foliage, 0)]
  AGB[, SoftwoodOther   := data.table::fifelse( sw, other,   0)]
  AGB[, HardwoodMerch   := data.table::fifelse(!sw, merch,   0)]
  AGB[, HardwoodFoliage := data.table::fifelse(!sw, foliage, 0)]
  AGB[, HardwoodOther   := data.table::fifelse(!sw, other,   0)]
  AGB <- cbind(AGB, CBMutils::calcRootC(AGB))
  
  AGB <- AGB[, .SD, .SDcols = c(colJoin, colReplace)]
  data.table::setnames(AGB, colReplace, paste0("pools.", colReplace))
  
  sim$cohortDT[, paste0("pools.", colReplace) := NULL]
  sim$cohortDT[sim$gcMeta, speciesCode := speciesCode, on = "gcID"]
  sim$cohortDT <- sim$cohortDT[AGB, on = colJoin]
  sim$cohortDT[, speciesCode := NULL]
  
  # Return simList
  return(invisible(sim))
}

# Process yearly vegetation inputs
AnnualIncrements <- function(sim){
  
  # Step 1: Store the above ground biomass of the previous time step.-----------
  biomassTminus1 <- copy(sim$aboveGroundBiomass)
  # Increment age to match the *current* age for joining later
  biomassTminus1[, age := age + 1L]
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
    standDT = sim$standDT[,.(pixelIndex, juris_id = admin_abbrev, ecozone = eco_id)],
    table6 = sim$table6,
    table7 = sim$table7,
    tableMerchantability = sim$tableMerchantability,
    sppEquiv = sim$sppEquiv
  )
  
  # Step 3: Calculate this year's increments.-----------------------------------
  # Full outer join between current biomass and previous biomass (incremented age)
  incrementsDT <- merge(
    sim$aboveGroundBiomass,
    biomassTminus1,
    by = c("pixelIndex", "speciesCode", "age"),
    all.x = TRUE, # Include new cohorts
    all.y = FALSE # Exclude disturbed and DOM cohorts
  )
  data.table::setnafill(incrementsDT, fill = 0, cols = c("merchTminus1", "foliageTminus1", "otherTminus1"))
  
  # Calculate increments by subtracting previous from current
  incrementsDT[, `:=`(
    merch_inc   = merch   - merchTminus1,
    foliage_inc = foliage - foliageTminus1,
    other_inc   = other   - otherTminus1
  )]
  incrementsDT[, c(c("merch", "foliage", "other", "merchTminus1", "foliageTminus1", "otherTminus1")) := NULL]
  
  # Set age to age at beginning of year
  incrementsDT[, age := age - 1]
  
  # Create unique ID for each increment
  groupCols <- c("speciesCode", "age", "merch_inc", "foliage_inc", "other_inc")
  incrementsDT[, gcID := as.integer(.GRP), by = groupCols]
  
  # Step 4: Set cohorts.-----------------------------------
  
  if (!"speciesCode" %in% names(sim$cohortDT)){
    sim$cohortDT[sim$gcMeta, speciesCode := speciesCode, on = "gcID"]
    on.exit(sim$cohortDT[, speciesCode := NULL])
  }
  
  sim$cohortDT[, gcID := NULL]
  sim$cohortDT <- merge(
    incrementsDT[, .(pixelIndex, speciesCode, age, gcID)],
    sim$cohortDT,
    by = c("pixelIndex", "speciesCode", "age"),
    all.x = TRUE, # Include new cohorts
    all.y = TRUE  # Keep disturbed and DOM cohorts
  )
  
  # New cohorts: initiate pools
  poolCols <- names(sim$cohortDT)[grepl("^pools\\.", names(sim$cohortDT))]
  sim$cohortDT[, new := is.na(pools.SoftwoodMerch)]
  sim$cohortDT[new==TRUE, (poolCols) := 0]
  sim$cohortDT[, new := NULL]
  
  if (anyNA(sim$cohortDT$gcID)){
  
    # Set disturbance types
    # NOTE: "Stand–replacing natural succession" may not be the correct disturbance type for cohorts experiencing mortality.
    distMeta <- cbind(
      rbind(
        data.table::data.table(eventID = 2000, disturbance_type_name = "Stand–replacing natural succession", gcID = 0),
        data.table::data.table(eventID = 2001, disturbance_type_name = "Wildfire"),
        fill = TRUE),
      enable_merge = 1L, # Enable merging of cohorts after disturbance
      proportion   = 1L  # Apply to all eligible cohorts
    )
    
    # Set disturbed & DOM cohorts
    sim$cohortDT[is.na(gcID) & state.last_disturbance_event %in% sim$disturbanceMeta$eventID, gcID := -1] # DOM
    sim$cohortDT[is.na(gcID), gcID := 0] # Disturbed this year
    
    # Assign disturbances
    distEvents <- data.table::data.table()
    
    ## Set fire disturbance events
    if (!is.null(sim$treedFirePixelTableSinceLastDisp)){
      
      distEvents <- rbind(
        distEvents, 
        sim$treedFirePixelTableSinceLastDisp[burnTime == time(sim), .(
          eventID = 2001, pixelIndex, year = burnTime)],
        fill = TRUE)
    }
    
    ## Set mortality disturbance events
    distEvents <- rbind(
      distEvents,
      sim$cohortDT[gcID == 0 & !pixelIndex %in% distEvents$pixelIndex, .(
        eventID = 2000, pixelIndex, year = as.integer(time(sim)))] |> unique(),
      fill = TRUE)
    
    if (nrow(distEvents) > 0){
    
      sim$disturbanceMeta   <- rbind(sim$disturbanceMeta,   distMeta,   fill = TRUE) |> unique()
      sim$disturbanceEvents <- rbind(sim$disturbanceEvents, distEvents, fill = TRUE)
      
      # Check that all disturbed cohorts have a disturbance
      ## TODO
    }

    # Aggregate DOM cohorts per pixel
    if (anyDuplicated(sim$cohortDT[gcID == -1, pixelIndex])){
      
      sim$cohortDT <- rbind(
        sim$cohortDT[!gcID == -1],
        cbind(
          sim$cohortDT[gcID == -1, lapply(.SD, sum), .SDcols = poolCols, by = c(
            setdiff(names(sim$cohortDT), c(
              poolCols, "age", "state.time_since_last_disturbance", "state.time_since_land_class_change")
            ))],
          age = 0
        ), 
        fill = TRUE)
    }
  }

  # Set cohort attributes
  sim$cohortDT[, cohort_proportion := NULL]
  sim$cohortDT[, cohort_index := .GRP, by = c("speciesCode", "age")]
  
  # Step 5: Set gcMeta and gcIncrements.----------------------------------- 
  
  incrementsDT$sw <- !CBMutils::sppMatch(
    incrementsDT$speciesCode, sppEquiv = sim$sppEquiv, match = "LandR", return = "Broadleaf")$Broadleaf
  
  ## Add increments for disturbed and DOM cohorts
  incrementsDT <- rbind(
    incrementsDT,
    data.table::data.table(
      gcID        = -1L:0L,
      speciesCode = NA_character_,
      sw          = TRUE,
      merch_inc   = 0,
      foliage_inc = 0,
      other_inc   = 0
    ),
    fill = TRUE)
  
  sim$gcMeta       <- incrementsDT[, .(gcID, speciesCode, sw)]
  sim$gcIncrements <- incrementsDT[, .(gcID, age = "?", merch_inc, foliage_inc, other_inc)]
  data.table::setkey(sim$gcMeta, gcID)
  data.table::setkey(sim$gcIncrements, gcID, age)
  
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
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  
  # NFI params. Used to split total biomass into biomass of the three CBM
  #                above ground biomass pools.
  if (!suppliedElsewhere("table6", sim)) {
    sim$table6 <- prepInputs(url = extractURL("table6"),
                             fun = data.table::fread(targetFile, verbose = FALSE),
                             destinationPath = inputPath(sim),
                             filename2 = "appendix2_table6_tb.csv",
                             overwrite = TRUE) |> Cache(userTags = "prepInputsTable6")
  }
  
  if (!suppliedElsewhere("table7", sim)) {
    sim$table7 <- prepInputs(url = extractURL("table7"),
                             fun = data.table::fread(targetFile, verbose = FALSE),
                             destinationPath = inputPath(sim),
                             filename2 = "appendix2_table7_tb.csv",
                             overwrite = TRUE) |> Cache(userTags = "prepInputsTable7")
  }
  
  if (!suppliedElsewhere("tableMerchantability", sim)) {
    sim$tableMerchantability <- prepInputs(url = extractURL("tableMerchantability"),
                                           fun = data.table::fread(targetFile, verbose = FALSE),
                                           destinationPath = inputPath(sim),
                                           filename2 = "merchantabilityParams.csv",
                                           overwrite = TRUE) |> Cache(userTags = "prepInputsTableMerch")
    sim$tableMerchantability <- cbind(sim$tableMerchantability, minAge = P(sim)$minMerchantableAge)
  }
  
  # Return simList
  return(invisible(sim))
}
