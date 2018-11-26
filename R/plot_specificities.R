#' Plot specificities of cat1 (lemma or word) according to cat2 (categories)
#' @param data a tibble
#' @param cat1 words or lemmas
#' @param cat2 categories
#' @param criterion should the information displayed be filtered by top specificities by category ('top_n') or according to a minimum value of specificity ("spec_min").
#' @param top_n in case criterion=='top_n', how many items by category should be kept (defaults to 50)
#' @param spec_min in case criterion=='min_spec', which is the minimum specificity for an item to be kept (defaults to 2)
#' @return a plot
#' @export
#' @examples
#' "pouet"
plot_specificities=function(spec_data,cat1, cat2, criterion="top_n", top_n=50, min_spec=2){
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  spec_data <- spec_data %>%
    group_by(!!cat2)

  if(criterion=="top_n"){
    spec_data <- spec_data %>%
      top_n(top_n,spec)
  }
  if(criterion=="min_spec"){
    spec_data <- spec_data %>%
      filter(spec>min_spec)
  }
  spec_data <- spec_data %>%
    arrange(!!cat2,spec)%>%
    mutate(id=1:length(spec))
  p=ggplot(spec_data,
           aes(x=id, y=spec, fill=!!cat2))+
    geom_bar(stat="identity", alpha=0.5)+
    geom_text(aes(label=!!cat1, y=0), hjust=0)+
    coord_flip()+
    facet_wrap(vars(!!cat2), scales="free")+
    scale_x_discrete(breaks=NULL)
  return(p)
}
