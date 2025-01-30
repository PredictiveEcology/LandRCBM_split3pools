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
