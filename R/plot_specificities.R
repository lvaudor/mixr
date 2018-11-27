#' Plot specificities of cat1 (lemma or word) according to cat2 (categories)
#' @param data a tibble
#' @param cat1 words or lemmas
#' @param cat2 categories
#' @return a plot
#' @export
#' @examples
#' library(janeaustenr)
#' df1<- tibble(txt=prideprejudice) %>% unnest_tokens(word,txt)
#' df2<- tibble(txt=sensesensibility) %>% unnest_tokens(word,txt)
#' df <- bind_rows(mutate(df1,novel="prideprejudice"),
#'                 mutate(df2,novel="sensesensibility"))
#' df_spec=tidy_specificities(df, word, novel, criterion="top_n",30)
#' plot_specificities(df_spec, word, novel)
plot_specificities=function(spec_data, cat1, cat2){
  cat1 <- enquo(cat1)
  cat2 <- enquo(cat2)
  spec_data <- spec_data %>%
    group_by(!!cat2)
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
