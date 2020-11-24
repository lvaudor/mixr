library(testthat)
library(mixr)

context("get_expressions")


test_that("get_expressions('fr')", {
  result=get_expressions("fr") %>% dplyr::slice(100)
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result.")
  expect_true(ncol(result)==2, "Expected number of columns to be 2")
  expect_true(all(colnames(result)==c("word","word_cor")))
})


context("get_lexicon")

test_that("get_lexicon('it')", {
  result=get_lexicon("it") %>% dplyr::slice(100)
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(ncol(result)==3, "Expected number of columns to be 3")
  expect_true(all(colnames(result)==c("word","lemma","type")))
})
