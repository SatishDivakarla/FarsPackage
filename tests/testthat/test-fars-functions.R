require(testthat)
require("FarsPackage")

test_that("Make file name working",  {
  expect_equal(make_filename("2013"), "accident_2013.csv.bz2")
})

