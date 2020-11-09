#' Returns a tibble with specificities according to two crossed categories.
#' @param data a tibble
#' @param cat1 a factor corresponding to words or lemmas
#' @param cat2 a category
#' @param criterion one of "all" (default), "top_n" or "min_spec": should the information displayed be filtered by top specificities (top_n) or according to a minimum value of specificity (min_spec).
#' @param top_n in case criterion=='top_n', how many items by category should be kept (defaults to 50)
#' @param min_spec in case criterion=='min_spec', which is the minimum specificity for an item to be kept (defaults to 2)
#' @return tibble with additional columns cat1, cat2, spec
#' @export
#' @examples
#' library(janeaustenr)
#' df1<- tibble(txt=prideprejudice) %>% unnest_tokens(word,txt)
#' df2<- tibble(txt=sensesensibility) %>% unnest_tokens(word,txt)
#' df <- bind_rows(mutate(df1,novel="prideprejudice"),
#'                 mutate(df2,novel="sensesensibility"))
#' data_spec=tidy_specificities(df, word, novel)

tidy_specificities=function(data,cat1,cat2, criterion="all", top_n=50, min_spec=2){
  qcat1 <- rlang::enquo(cat1)
  qcat2 <- rlang::enquo(cat2)
  vcat1=data %>%
    dplyr::select(!!qcat1) %>%
    dplyr::pull(1)
  vcat2=data %>%
    dplyr::select(!!qcat2) %>%
    dplyr::pull(1)
  freqs=data %>%
    dplyr::group_by(!!qcat1,!!qcat2) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::select(cat1=!!qcat1,
           cat2=!!qcat2,
           n)
  spe=textometry::specificities(table(vcat1,vcat2))
  spe=dplyr::bind_cols(cat1=row.names(spe),
                       tibble::as_tibble(spe,.name_repair="minimal"))
  spe=tidyr::gather(spe,
                    "cat2","spec",
                    -cat1)
  mode(spe$cat1)=mode(vcat1)
  mode(spe$cat2)=mode(vcat2)
  spe <- spe %>%
    dplyr::left_join(freqs, by=c("cat1","cat2"))
  colnames(spe)=c(colnames(dplyr::select(data,!!qcat1,!!qcat2)),"spec","n")
  cat1 <- rlang::enquo(cat1)
  cat2 <- rlang::enquo(cat2)

  if(criterion=="top_n"){
    spe <- spe %>%
      dplyr::group_by(!!cat2) %>%
      dplyr::top_n(top_n,spec) %>%
      dplyr::ungroup()
  }
  if(criterion=="min_spec"){
    spe <- spe %>%
      dplyr::filter(spec>min_spec)
  }
  spe <- spe %>%
    dplyr::arrange(desc(spec))
  return(spe)
}
