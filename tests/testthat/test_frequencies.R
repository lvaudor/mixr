library(testthat)
library(mixr)

context("tidy_frequencies()")

mydf=tibble::tibble(txt=janeaustenr::prideprejudice) %>%
  tidytext::unnest_tokens(word,txt)

test_that("tidy_frequencies()", {
  result=tidy_frequencies(mydf, word)
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result.")
  expect_true(ncol(result)==ncol(mydf)+1, "Expected number of columns to be ncol(df)+1")
})

test_that("tidy_frequencies(min_freq)", {
  result=tidy_frequencies(mydf,
                          word,
                          min_freq=100)
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(min(result$freq)>=100)
})

test_that("tidy_frequencies(top_freq)", {
  result=tidy_frequencies(mydf,
                          word,
                          top_freq=10)
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(nrow(result)==10)
})

context("plot_frequencies()")
test_that("plot_frequencies() returns ggplot",{
  df_freq <- tidy_frequencies(mydf, word,min_freq=500)
  p=plot_frequencies(df_freq, word,n)
  expect_true("gg" %in% class(p),"Expected ggplot output")
})
