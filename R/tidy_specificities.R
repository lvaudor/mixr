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
  qcat1 <- enquo(cat1)
  qcat2 <- enquo(cat2)
  vcat1=select(data,
               !!qcat1) %>% pull(1)
  vcat2=select(data,
               !!qcat2) %>% pull(1)
  spe=specificities(table(vcat1,vcat2))
  spe=bind_cols(cat1=row.names(spe),as_tibble(spe))
  spe=tidyr::gather(spe,
                    "cat2","spec",
                    -cat1)
  mode(spe$cat1)=mode(vcat1)
  mode(spe$cat2)=mode(vcat2)
  colnames(spe)=c(colnames(select(data,!!qcat1,!!qcat2)),"spec")
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)

  if(criterion=="top_n"){
    spe <- spe %>%
      group_by(!!cat2) %>%
      top_n(top_n,spec) %>%
      ungroup()
  }
  if(criterion=="min_spec"){
    spe <- spe %>%
      filter(spec>min_spec)
  }
  spe <- spe %>%
    arrange(desc(spec))
  return(spe)
}
