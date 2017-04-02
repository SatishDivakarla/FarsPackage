Sys.setenv("R_TESTS" = "")

library(testthat)
library(FarsPackage)

test_check("FarsPackage")
