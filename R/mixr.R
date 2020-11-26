#' @title \code{mixr} package
#' @details Functions to simplify text analysis with multiple languages.
#' Uses Lexique382 dictionnaries (French lexicon and expressions) and Iramuteq's dictionnaries for other languages.
#'
#' See the README on
#' \href{https://github.com/lvaudor/mixr#readme}{GitHub}
#'
#' @docType package
#' @name mixr
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
