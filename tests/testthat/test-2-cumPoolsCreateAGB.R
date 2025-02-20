# tables 
table6 <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1gvtV-LKBNbqD7hmlL4X0i40P3du75oJc",
                                   fun = "data.table::fread",
                                   destinationPath = "inputs",
                                   filename2 = "appendix2_table6_tb.csv")
table7 <- reproducible::prepInputs(url = "https://drive.google.com/file/d/16nQgTGW2p_IYF_Oavcc7WbWbgWl5uywt",
                                   fun = "data.table::fread",
                                   destinationPath = "inputs",
                                   filename2 = "appendix2_table7_tb.csv")

# test convertAGB2pools
dt <- data.table(
  expand.grid(canfi_species = c(204), # PINU_CON
              age = c(3,15, 35),
              ecozone = 4,
              juris_id = "AB"
              ) 
)
dt$B <- round(runif(nrow(dt), 1, 100))
out <- convertAGB2pools(dt, table6 = table6, table7 = table7)

#sum of the pools equal total AGB
expect_equal(rowSums(out), dt$B)
expect_true(out[dt$age < 15, "totMerch"] ==  0)
expect_true(all(colnames(out) == c("totMerch", "fol", "other")))
expect_true(all(!is.na(out)))
expect_equal(dim(out), c(3,3))

# test cumPoolsCreateAGB
dt <- data.table(
  expand.grid(canfi_species = c(204, 1201), # PINU_CON, POPU_TRE
              age = c(3,15, 35),
              ecozone = 4,
              juris_id = "AB",
              pixelGroup = c(1,2)
  ) 
)
dt$B <- round(runif(nrow(dt), 1, 100))
dt$speciesCode[dt$canfi_species == 204] <- "PINU_CON"
dt$speciesCode[dt$canfi_species == 1201] <- "POPU_TRE"
setorder(dt, speciesCode, age, pixelGroup)

out2 <- cumPoolsCreateAGB(dt, table6 = table6, table7 = table7)

expect_equal(rowSums(out2[,c("totMerch", "fol", "other")]), dt$B/2)
expect_true(all(out2[dt$age < 15, "totMerch"] ==  0))
expect_equal(nrow(out2), nrow(dt))
expect_true(all(colnames(out2) == c("species", "age", "pixelGroup", "totMerch", "fol", "other")))

dt$yieldPixelGroup <- dt$pixelGroup
dt$pixelGroup <- NULL
dt$cohort_id <- c(1:nrow(dt))
out3 <- cumPoolsCreateAGB(dt, table6 = table6, table7 = table7, pixGroupCol = "yieldPixelGroup")
expect_equal(out3$gcids, dt$cohort_id)
expect_equivalent(out2, out3[,-c("gcids")])
