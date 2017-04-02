require(testthat)
require("FarsPackage")

test_that("Make file name working with valid number",  {
  expect_equal(make_filename("2013"), "accident_2013.csv.bz2")
})

test_that("Make file name failed with non integer",  {
  expect_warning(make_filename("test"))
})
