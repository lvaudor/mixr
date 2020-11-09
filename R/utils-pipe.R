
#' Make empty line
#' @param info the column names
make_empty_result=function(info=c("lat","lng")){
  result=rep(NA,length(info)+1) %>% t() %>%
    tibble::as_tibble(.name_repair="minimal")
  names(result)=c("stringlocation",info)
  return(result)
}


#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
