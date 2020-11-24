#' \code{mixr} package
#'
#' A package that mixes a few tools to analyse texts
#'
#' See the README on
#' \href{https://cran.r-project.org/package=mixr/README.html}{CRAN}
#' or \href{https://github.com/lvaudor/mixr#readme}{GitHub}
#'
#' @docType package
#' @name mixr
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
