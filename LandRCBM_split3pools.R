## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "LandRCBM_split3pools",
  description = paste("Takes total aboveground biomass provided by LandR and divides",
                      "it into the 3 required CBM pools"),
  keywords = "",
  authors = structure(list(list(given = c("Celine", "Middle"), family = "Boisvenue", role = c("aut", "cre"), email = "cboivenue@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(LandRCBM_split3pools = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandRCBM_split3pools.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=1.1.0)", "ggplot2", "data.table", "CBMutils"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
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
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    # the required parameter tables are not yet hosted on the NFIS (Oct., 2022).
    # The links "in the examples"table6" and "table7" below are for the
    # parameters in the equations using "volume" instead of "total tree biomass"
    # as a dependent variable. For now these two are replaced by a table
    # containing both the parameters and the caps for BC provided in an email to
    # cboivenue by Paul Boudewyn.
    expectsInput(
      objectName = "table6", objectClass = "dataframe",
      desc = "Proportion model parameters similar to Boudewyn et al 2007,
      but recalculated using total biomass (metric tonnes of tree biomass/ha) instead of vol/ha",
      sourceURL = "https://drive.google.com/file/d/1gvtV-LKBNbqD7hmlL4X0i40P3du75oJc/view?usp=sharing"
        #NOTE: current link is to a google drive but parameters will eventually
        #be on the NFIS site as per the volume-based parameters. For now, I put
        #a copy of the biomass-basedones provided by Paul boudewyn on the
        #WBI/Carbon drive (FOR-Cast).
    ),
    expectsInput(
      objectName = "table7", objectClass = "dataframe",
      desc = "Caps on proportion models similar to  Boudewyn et al 2007,
      but recalculated using total biomass (metric tonnes of tree biomass/ha) instead of vol/ha",
      sourceURL = "https://drive.google.com/file/d/16nQgTGW2p_IYF_Oavcc7WbWbgWl5uywt/view?usp=sharing"
      #NOTE: current link is to a google drive but parameters will eventually
      #be on the NFIS site as per the volume-based parameters. For now, I put
      #a copy of the biomass-basedones provided by Paul boudewyn on the
      #WBI/Carbon drive (FOR-Cast).
    ),
      expectsInput(
      objectName = "cbmAdmin", objectClass = "dataframe",
      desc = "Provides equivalent between provincial boundaries, CBM-id for provincial boundaries and CBM-spatial unit ids",
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"
    ),
    expectsInput(
      objectName = "ecozone", objectClass = "raster",
      desc = "Ecozones of Canada",
      sourceURL = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip"
    ),
    expectsInput(
      objectName = "canfi_species",
      objectClass = "dataframe",
      desc = "File containing the possible species in the Boudewyn table",
      sourceURL = "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo"
    ),
    expectsInput(##TODO Yield module will be modified to provide required format
      objectName = "CBM_AGB", objectClass = "dataframe",
      desc = "Table created in the Yield module, will eventually get it from the module,
              for now, sitting in the WBI/Carbon/LandrCBM folder",
      sourceURL = "https://drive.google.com/file/d/1xxfG-8ZJKPlO5HguHpVtqio5TXdcRGy7"
    ),
    expectsInput(
      objectName = "CBM_speciesCodes", objectClass = "dataframe",
      desc = "Table created in the Yield module, will eventually get it from the module,
              for now, sitting in the WBI/Carbon/LandrCBM folder",
      sourceURL = "https://drive.google.com/file/d/1sVsDoT1E-CDgo2hnCU2pgqV6PpVic2Fe"
    ),
    expectsInput(objectName = "pixelGroupMap",
                 objectClass = "raster",
                 desc = "PixelGroup map from LandR",
                 sourceURL = "https://drive.google.com/file/d/1Pso52N9DFVJ46OFxtvtqrVX9VzOVhcf3"
    )
  )

    outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "CBM_AGBplots",
                  objectClass = "plot",
                  desc = "Plot of the AGB values per cohort provided by the Yield module"),
    createsOutput(objectName = "cumPools",
                  objectClass = "dataframe",
                  desc = "Cumulative carbon in three pools, totMerch, fol, and other per cohort"),
    createsOutput(objectName = "growth_incForSpinup",
                  objectClass = "matrix",
                  desc = "Matrix of the 1/2 increment for every age provided per cohort")
    )
))

## event types
#   - type `init` is required for initialization

doEvent.LandRCBM_split3pools = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LandRCBM_split3pools", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LandRCBM_split3pools", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "LandRCBM_split3pools", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "LandRCBM_split3pools", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "LandRCBM_split3pools", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "LandRCBM_split3pools", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  ###1. Data clean-up and creation until we update the Yield module
  ## extra column was probably created when I saved the file - no need for this
  ## unce Yield is updated
  CBM_AGB <- sim$CBM_AGB[,2:6]
  CBM_speciesCodes <- sim$CBM_speciesCodes[,2:4]

  ### need to match the pixel groups with the ecozones and juris_id
  ## Are pixelGroupMap and ecozone the same RTM?
  ##checking
  if(length(pixelGroupMap[]) != length(ecozone[])){
    stop("There is a problem: the ecozone raster and the pixelGroupMap are not equal")
  }
  sim$pixelGroupEco <- as.data.table(cbind(pixelIndex = 1:ncell(pixelGroupMap),
                                       pixelGroup = pixelGroupMap[],
                                       ecozone = ecozone[]))

  ##TODO
  ##Limiting the pixelGroups to three to get all this working. Will need to
  ##update this section to use all our pixelGroups
  ### limiting this to our pixelGroups
  pixelGroupEco <- sim$pixelGroupEco[, "pixelIndex" := NULL][pixelGroup %in% 1:3,]
  pixelGroupEco <- unique(pixelGroupEco)

  ### matching the ecozone to the admin, for now this is done by hand...
  ##TODO make the matching to the admin abreviation automatic or make it a user
  ##defined parameter in the global.R
  juris_id <- sim$cbmAdmin[(EcoBoundaryID %in% pixelGroupEco$ecozone) & abreviation == "BC",]$abreviation

  pixelGroupEco[, juris_id := juris_id]


  ##TODO
  ###Make up my data from from Yield module: doing this b/c what comes out of
  ###Yield is currently not the correct format
  setnames(CBM_AGB,"id","pixelGroup")

  ##TODO
  ##Limiting the pixelGroups to three to get all this working. Will need to
  ##update this section to use all our pixelGroups
  CBM_AGB <- CBM_AGB[pixelGroup %in% 1:3,]

  cbm1 <- melt.data.table(CBM_AGB, id.vars = c("pixelGroup", "age"), variable.name = "Sp", value.name = "B")
  keycols1 = c("pixelGroup", "Sp")
  setkeyv(cbm1, keycols1)
  setkeyv(CBM_speciesCodes, keycols1)
  CBM_yieldOut <- merge(cbm1, CBM_speciesCodes, by = c("pixelGroup", "Sp"))
  CBM_yieldOut <- CBM_yieldOut[,-"Sp"]





  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  # 1. NFIparams
  if (!suppliedElsewhere("table6", sim)) {

    # in this module data folder
    sim$table6<- prepInputs(targetFile = extractURL("table6"),
                             fun = "data.table::fread",
                             destinationPath = Paths$inputPath,
                             #purge = 7,
                             filename2 = "appendix2_table6_tb.csv")
  }

  if (!suppliedElsewhere("table7", sim)) {

    # in this module data folder
    sim$table7<- prepInputs(targetFile = extractURL("table7"),
                            fun = "data.table::fread",
                            destinationPath = Paths$inputPath,
                            #purge = 7,
                            filename2 = "appendix2_table7_tb.csv")
  }

###HERE
  # 2. CBM and NFI admin
    if (!suppliedElsewhere("cbmAdmin", sim)) {
      sim$cbmAdmin <- prepInputs(url = extractURL("cbmAdmin"),
                                 fun = "data.table::fread",
                                 destinationPath = Paths$inputPath,
                                 #purge = 7,
                                 filename2 = "cbmAdmin.csv")
    }

  if (!suppliedElsewhere("canfi_species", sim)) {
    sim$canfi_species <- prepInputs(url = extractURL("canfi_species"),
                               fun = "data.table::fread",
                               destinationPath = Paths$inputPath,
                               #purge = 7,
                               filename2 = "canfi_species.csv")
  }
  if (!suppliedElsewhere("ecozone", sim)) {
    sim$ecozone <- CBMutils::prepInputsEcozones(url = extractURL("ecozone"),
                               destinationPath = Paths$inputPath,
                               rasterToMatch = RIArtm)
  }

  # 3. Information from LandR
  ## these two next tables will be coming from the Yield module
  ## this one is the actual yields that are needed for the CBM spinup

  if (!suppliedElsewhere("CBM_AGB", sim)){
    sim$CBM_AGB <- prepInputs(url = extractURL("CBM_AGB"),
                              fun = "data.table::fread",
                              destinationPath = Paths$inputPath,
                              filename2 = "CBM_AGB.csv")
  }

  if (!suppliedElsewhere("CBM_speciesCodes", sim)){
    sim$CBM_speciesCodes <- prepInputs(url = extractURL("CBM_speciesCodes"),
                              fun = "data.table::fread",
                              destinationPath = Paths$inputPath,
                              filename2 = "CBM_speciesCodes.csv")
  }
  ## pixel to pixelGroup map from LandR
  if (!suppliedElsewhere("pixelGroupMap", sim))
  sim$pixelGroupMap <- prepInputs(url = extractURL("pixelGroupMap"),
                         destinationPath = Paths$inputPath,
                         rasterToMatch = RIArtm,
                         useCache = TRUE)


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot(data, aes(TheSample)) +
    geom_histogram(...)
}

### add additional events as needed by copy/pasting from above
