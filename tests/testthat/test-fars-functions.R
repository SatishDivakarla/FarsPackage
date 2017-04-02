require(testthat)
require("FarsPackage")

test_that("fars_read reads valid file", {
  valid_filename <- "data/accident_2013.csv.bz2"
  result_table <- fars_read(valid_filename)
  expect_that(result_table, is_a("data.frame"), label = "result_table should be an object of data.frame")
  expect_that(result_table, is_a("tbl"), label = "result_table should be an object of tbl")
  expect_that(result_table, is_a("tbl_df"), label = "result_table should be an object of tbl_df")
  expect_equal(length(result_table), 50, label = "length of result_table should be 50")
})

test_that("fars_read fails when file does not exist", {
  invalid_filename <- "data/accident_20143.csv.bz2"
  expect_error(fars_read(invalid_filename), label= "Fars_read should fail when file does not exist")
})
