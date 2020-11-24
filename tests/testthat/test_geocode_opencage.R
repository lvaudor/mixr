library(testthat)
library(mixr)

context("Test geocode_opencage() for simple requests")

test_that("Test geocode_opencage() for simple location, no info specified", {
  result=geocode_opencage("Place Bellecour, Lyon")
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result.")
  expect_true(ncol(result)==3, "Expected number of columns to be 3 (for default info lat-lng)")
})

test_that("Test geocode_opencage() for missing location, no info specified", {
  result=geocode_opencage("")
  expect_true(dplyr::is.tbl(result), "Expected a tibble as a result")
  expect_true(nrow(result)==1, "Expected 1 line of (empty) result")
  expect_true(ncol(result)==3, "Expected number of columns to be 3 (for default info lat-lng)")
})


context("Test geocode_opencage() for simple requests, some specific info required.")

test_that("Test geocode_opencage() for simple location, required info annotations.wikidata",{
  result=geocode_opencage("La Guillotière, Lyon", info=c("annotations.wikidata"))
  expect_true(dplyr::is.tbl(result), "Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result")
  expect_true(ncol(result)==2, "Expected number of columns to be 2 (for only one info required)")
})

test_that("Test geocode_opencage() for simple location, required info annotations.wikidata",{
  result=geocode_opencage("La Guillotière, Lyon", info=c("annotations.wikidata"))
  expect_true(dplyr::is.tbl(result), "Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result")
  expect_true(ncol(result)==2, "Expected number of columns to be 2 (for only one info required)")
})

test_that("Test geocode_opencage() for simple location, required non-existent info",{
  result=geocode_opencage("Gare SNCF Part-Dieu, Lyon", info=c("lat","pouetpouet"))
  expect_true(dplyr::is.tbl(result), "Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result")
  expect_true(ncol(result)==3, "Expected number of columns to be 5 (for 5 specific items of info required)")
})
