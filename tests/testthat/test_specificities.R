library(testthat)
library(mixr)

context("tidy_specificities()")

mydf=dplyr::bind_rows(tibble::tibble(txt=janeaustenr::prideprejudice,novel="Pride and Prejudice"),
                      tibble::tibble(txt=janeaustenr::sensesensibility,novel="Sense and Sensibility")) %>%
  tidytext::unnest_tokens(word,txt)

test_that("tidy_specificities()", {
  result=tidy_specificities(mydf,
                            cat1=word,
                            cat2=novel)
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(nrow(result)>0, "Expected at least one line of result.")
  expect_true(ncol(result)==ncol(mydf)+2, "Expected number of columns to be ncol(df)+1")
})

test_that("tidy_specificities(min_spec)", {
  result=tidy_specificities(mydf,
                            cat1=word,
                            cat2=novel,
                            min_spec=2)
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(all(result$spec>=2))
})

test_that("tidy_specificities(top_spec)", {
  result=tidy_specificities(mydf,
                            cat1=word,
                            cat2=novel,
                            top_spec=10)
  expect_true(dplyr::is.tbl(result),"Expected a tibble as a result")
  expect_true(nrow(result)==20)
})

context("plot_specificities()")
test_that("plot_specificities() returns ggplot",{
  df_spec <- tidy_specificities(mydf, word, novel,min_spec=5)
  p=plot_specificities(df_spec, word,novel)
  expect_true("gg" %in% class(p),"Expected ggplot output")
})
