
#' Make empty line
#' @param info the column names
#' @example make_empty_result(info=c("lat","lng"))
make_empty_result=function(info){
  result=rep(NA,length(info))
  names(result)=info
  result=result %>%
    dplyr::bind_rows()
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
