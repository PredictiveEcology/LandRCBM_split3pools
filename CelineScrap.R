##Celine's scrapt to make it work before putting it in a module

## Need packages?
###running up to line 39 in global

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
# AdminBoundaryID and EcoBoundary and us used for parameter matching

cbmAdmin <-  prepInputs(url = "https://drive.google.com/file/d/1NwVPqMho8fOcf9mVVzm5SlfDbbyQmQlh/view?usp=sharing",
                            fun = "data.table::fread",
                            destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                            #purge = 7,
                            filename2 = "cbmAdmin.csv")

## we need the studyArea
RIArtm <- prepInputs(url = "https://drive.google.com/file/d/1h7gK44g64dwcoqhij24F2K54hs5e35Ci/view?usp=sharing",
                     destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs")


### ecozones
###Line below failed
#installGithubPackage("PredictiveEcology/CBMutils@development")

Require("devtools")
load_all("C:/Celine/github/CBMutils")

ecozone <- CBMutils::prepInputsEcozones(url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                 dPath =  "C:/Celine/github/LandRCBM_split3pools/inputs",
                 rasterToMatch = RIArtm)


## not sure if I need this yet...
# cbmAdminThisSA <- cbmAdmin[adminName == "British Columbia", ]
#
# rows <- match(ecozone[], cbmAdminThisSA$EcoBoundaryID)
# ####not sure if it is the spus I want
# spatialUnitID <- cbmAdminThisSA[rows,"SpatialUnitID"]

### subsetting table6 and table7 to have "BC" only and the right ecozones
eco <- unique(ecozone[])
eco <- eco[!is.na(eco)]


## canfi_species list
canfi_species <- prepInputs(url = "https://drive.google.com/file/d/1l9b9V7czTZdiCIFX3dsvAsKpQxmN-Epo",
                            fun = "data.table::fread",
                            destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                            filename2 = "canfi_species.csv")

## these two next tables will be coming from the Yield module
## this one is the actual yields
CBM_AGB <- prepInputs(url = "https://drive.google.com/file/d/1xxfG-8ZJKPlO5HguHpVtqio5TXdcRGy7",
                            fun = "data.table::fread",
                            destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                            filename2 = "CBM_AGB.csv")

## not sure why that extra column is added
CBM_AGB <- CBM_AGB[,2:6]

## this is the species matching the Sp1 Sp2 and Sp3 of the previous table
CBM_speciesCodes <- prepInputs(url = "https://drive.google.com/file/d/1sVsDoT1E-CDgo2hnCU2pgqV6PpVic2Fe",
                      fun = "data.table::fread",
                      destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs",
                      filename2 = "CBM_speciesCodes.csv"
                      # , useCache = FALSE
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
                     destinationPath = "C:/Celine/github/LandRCBM_split3pools/inputs")

### need to match the pixel groups with the ecozones and juris_id
## lines of code to help
  # dtRasters <- as.data.table(cbind(growth_curve_component_id = gcIDRaster[], ages = ageRaster[], ecozones = ecozone[], spatialUnitID))
  # sim$allPixDT <- as.data.table(cbind(dtRasters, pixelIndex = 1:ncell(gcIDRaster), growth_curve_id = gcIDRaster[]))
  # setnames(sim$allPixDT,"SpatialUnitID", "spatial_unit_id")
