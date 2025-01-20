matchCanfi <- function(LandR_species, canfi_species){
  speciesCode <- LandR_species
  NFI <- LandR::sppEquivalencies_CA$NFI[match(speciesCode, LandR::sppEquivalencies_CA$LandR)]
  canfi_species[, NFI := .(paste(genus, species, sep = "_"))]
  canfi_code <- canfi_species$canfi_species[match(NFI, canfi_species$NFI)]
  return(data.table(speciesCode = speciesCode,
                    canfi_species = canfi_code))
}

#Test
# LandR_species = c("Abie_las", "Betu_pap", "Pice_gla", "Pice_mar", "Pinu_con", "Popu_tre")
# canfi_species = c(304, 1303, 105, 101, 204, 1201)