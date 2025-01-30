source("R/cumPoolsCreateAGB.R")
library(data.table)
table6 <- reproducible::prepInputs(url = "https://drive.google.com/file/d/1gvtV-LKBNbqD7hmlL4X0i40P3du75oJc",
                         fun = "data.table::fread",
                         destinationPath = "inputs",
                     filename2 = "appendix2_table6_tb.csv")
table7 <- reproducible::prepInputs(url = "https://drive.google.com/file/d/16nQgTGW2p_IYF_Oavcc7WbWbgWl5uywt",
                         fun = "data.table::fread",
                         destinationPath = "inputs",
                         filename2 = "appendix2_table7_tb.csv")
CBM_AGB <- reproducible::prepInputs(
  "tests/testthat/fixtures/yield_module_sample/CBM_AGB.csv", 
  fun = "data.table::fread"
)
allInfoYieldTables <- fread("tests/testthat/fixtures/allInfoYieldTables.csv")
allInfoYieldTables <- merge(CBM_AGB, allInfoYieldTables, allow.cartesian = TRUE)
setnames(allInfoYieldTables, c("abreviation", "EcoBoundaryID"), c("juris_id", "ecozone"))

out <- cumPoolsCreateAGB(allInfoYieldTables, table6, table7)

ggplot(out) +
  geom_line(aes(x = age, y = totMerch, color = as.factor(gcids)), linewidth = 1)

ggplot(out) +
  geom_line(aes(x = age, y = fol, color = as.factor(gcids)), linewidth = 1)

ggplot(out) +
  geom_line(aes(x = age, y = other, color = as.factor(gcids)), linewidth = 1)
