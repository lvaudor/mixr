#' Returns a tibble with specificities according to two crossed categories.
#' @param mydf a tibble
#' @param cat1 a factor corresponding to words or lemmas
#' @param cat2 a category
#' @param top_spec how many items by category (filter based on specificity) should be kept. If not provided (the default) everything is kept.
#' @param min_spec which is the minimum specificity for an item to be kept. If not provided (the default) everything is kept.
#' @return tibble with additional columns cat1, cat2, spec
#' @export
#' @examples
#'  mydf=dplyr::bind_rows(
#'          tibble::tibble(txt=janeaustenr::prideprejudice,
#'          novel="Pride and Prejudice"),
#'          tibble::tibble(txt=janeaustenr::sensesensibility,
#'          novel="Sense and Sensibility")) %>%
#'       tidytext::unnest_tokens(word,txt)
#'  tidy_specificities(mydf,
#'                     cat1=word,
#'                     cat2=novel)

tidy_specificities=function(mydf,
                            cat1,
                            cat2,
                            top_spec=NA,
                            min_spec=NA){
  qcat1 <- rlang::enquo(cat1)
  qcat2 <- rlang::enquo(cat2)
  vcat1=mydf %>%
    dplyr::select(!!qcat1) %>%
    dplyr::pull(1)
  vcat2=mydf %>%
    dplyr::select(!!qcat2) %>%
    dplyr::pull(1)
  freqs=mydf %>%
    dplyr::group_by(!!qcat1,!!qcat2) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::select(cat1=!!qcat1,
                  cat2=!!qcat2,
                  n)
  spe=textometry::specificities(table(vcat1,vcat2))
  spe=dplyr::bind_cols(cat1=row.names(spe),
                       tibble::as_tibble(spe,
                                         .name_repair="minimal")) %>%
      tidyr::gather("cat2","spec",
                    -cat1) %>%
      dplyr::left_join(freqs, by=c("cat1","cat2"))
  print(spe)
  if(!is.na(top_spec)){
    spe <- spe %>%
      dplyr::group_by(cat2) %>%
      dplyr::top_n(top_spec,.data$spec) %>%
      dplyr::ungroup()
  }
  if(!is.na(min_spec)){
    spe <- spe %>%
      dplyr::filter(.data$spec>min_spec)
  }
  spe <- spe %>%
    dplyr::arrange(dplyr::desc(.data$spec)) %>%
    purrr::set_names(c(colnames(dplyr::select(mydf,!!qcat1,!!qcat2)),
                       "spec","n"))
  return(spe)
}
