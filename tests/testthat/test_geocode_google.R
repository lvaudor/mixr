library(testthat)
library(mixr)

context("Test geocode_google() for simple requests")

test_that("Test geocode_google() for simple location, no info specified", {
  result=geocode_google("Place Bellecour, Lyon")
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result.")
  expect_true(ncol(result)==3, "Expected number of columns to be 3 (for default info lat-lng)")
})

test_that("Test geocode_google() for missing location, no info specified", {
  result=geocode_google("")
  expect_true(dplyr::is.tbl(result), "Expected a tibble as a result")
  expect_true(nrow(result)==1, "Expected 1 line of (empty) result")
  expect_true(ncol(result)==3, "Expected number of columns to be 3 (for default info lat-lng)")
})


context("Test geocode_google() for simple requests, some specific info required.")

test_that("Test geocode_google() for simple location, required info administrative_area_level_1",{
  result=geocode_google("Gare SNCF Part-Dieu, Lyon",
                        info=c("administrative_area_level_1"))
  expect_true(dplyr::is.tbl(result), "Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result")
  expect_true(ncol(result)==2, "Expected number of columns to be 2 (for only one info required)")
})

test_that("Test geocode_google() for simple location, required 4 specific items of info",{
  result=geocode_google("Gare SNCF Part-Dieu, Lyon",
                        info=c("administrative_area_level_1","country_code","locality","lat"))
  expect_true(dplyr::is.tbl(result), "Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result")
  expect_true(ncol(result)==5, "Expected number of columns to be 5 (for 5 specific items of info required)")
})
