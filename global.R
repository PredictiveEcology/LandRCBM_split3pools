## install/load required packages
pkgDir <- file.path(tools::R_user_dir(basename(getwd()), "data"), "packages",
                    version$platform, getRversion()[, 1:2])
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)

## this is a work-around for working from PFC...R cannot connect to URL

#options("download.file.method" = "wininet")


### In this section, only load the minimum of packages (Require, SpaDES.install)
#so all packages can be installed with correct version numbering. If we load a
#package too early and it is an older version that what may be required by a
#module, then we get an inconsistency

if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("PredictiveEcology/Require@development")
library(Require)

## load necessary packages

Require(c("PredictiveEcology/SpaDES.project@transition", "SpaDES", "reproducible",
          "SpaDES.core (>=1.1.0)", "SpaDES.tools (>= 1.0.0)",
          "googledrive", 'RCurl', 'XML', "PredictiveEcology/CBMutils@development",
          "data.table", "devtools"), # comes up with an error so added "devtools'
        require = c("SpaDES.core", "devtools"), # call `require` only on this package (same as `library`)
        verbose = 1)
#there is a problem with CBMutils
#Require("devtools")
#load_all("C:/Celine/github/CBMutils")



##TODO messing up the module path
setPaths(cachePath = "cache",
         inputPath = "inputs",
         #modulePath = "C:/Celine/github/",
         outputPath = "outputs")


## Study area RTM

RIArtm <- Cache(prepInputs, url = "https://drive.google.com/file/d/1h7gK44g64dwcoqhij24F2K54hs5e35Ci/view?usp=sharing",
                destinationPath = Paths$inputPath)
##TODO
### I think there needs to be this as a parameter for this module?
#idCols <- c(“pixelGroup”, “cohort_id”)
# this would need to trickly through the module

# this is not working for me
# options(spades.moduleCodeChecks = FALSE,
#         spades.recoveryMode = FALSE)
# if (any(Sys.info()[["user"]] %in% c("cboisven", "cboisvenue"))) {
#   Require("googledrive")
#   options(
#     gargle_oauth_cache = ".secrets",
#     gargle_oauth_email = "cboisvenue@gmail.com"
#   )
# }


objects <- list(RTM = RIArtm)
times <- list(start = 0, end = 350)

parameters <- list(
  LandRCBM_split3pools = list(.useCache = ".inputObjects")
  )

modules <- list("LandRCBM_split3pools")




# All the action is here
split3poolsInit <- simInit(
  times = times,
  params = parameters,
  modules = "modules",
  objects = objects
)


# # Extras not used, for plotting some things
# if (FALSE) {
#   cd1 <- cds[pixelGroup %in% sample(cds$pixelGroup, size = 49)]
#   cd1 <- rbindlist(list(cd1, cd1[, list(speciesCode = "Total", B = sum(B)), by = c("age", "pixelGroup")]), use.names = TRUE)
#   ggplot(cd1, aes(age, B, colour = speciesCode)) + facet_wrap(~pixelGroup) + geom_line() + theme_bw()
#
#   domAt100 <- cds[age == 100, speciesCode[which.max(B)], by = "pixelGroup"]
#   domAt200 <- cds[age == 200, speciesCode[which.max(B)], by = "pixelGroup"]
#   domAt300 <- cds[age == 300, speciesCode[which.max(B)], by = "pixelGroup"]
#
#   ageWhoIsDom <- cds[, B[which.max(B)]/sum(B), by = c("age", "pixelGroup")]
#   sam <- sample(seq(NROW(ageWhoIsDom)), size = NROW(ageWhoIsDom)/100)
#   ageWhoIsDom2 <- ageWhoIsDom[sam]
#   ggplot(ageWhoIsDom2, aes(age, V1)) + geom_line() + theme_bw() + geom_smooth()
#
#   ageWhoIsDom2 <- ageWhoIsDom[sam, mean(V1), by = "age"]
#   ggplot(ageWhoIsDom2, aes(age, V1)) + geom_line() + theme_bw() + geom_smooth()
#
#   giForCBM <- data.table::fread("growth_increments.csv")
#
# }
#
