#' Returns a tibble with specificities according to two crossed categories.
#' @param data a tibble
#' @param cat1 a category
#' @param cat2 a factor corresponding to words or lemmas
#' @return tibble with additional columns cat1, cat2, spec
#' @export
#' @examples
#' "pouet"

tidy_specificities=function(data,cat1,cat2){
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
  return(spe)
}
