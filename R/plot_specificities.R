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
#' data_spec=tidy_specificities(df, word, novel, criterion="top_n",top_n=30)
#' plot_specificities(data_spec, word, novel)
plot_specificities=function(spec_data, cat1, cat2){
  cat1 <- rlang::enquo(cat1)
  cat2 <- rlang::enquo(cat2)
  spec_data <- spec_data %>%
    dplyr::group_by(!!cat2)
  spec_data <- spec_data %>%
    dplyr::arrange(!!cat2,spec)%>%
    dplyr::mutate(id=1:length(spec))
  p=ggplot2::ggplot(spec_data,
                    ggplot2::aes(x=id, y=spec, fill=factor(!!cat2)))+
    ggplot2::geom_bar(stat="identity", alpha=0.5)+
    ggplot2::geom_text(aes(label=!!cat1, y=0), hjust=0)+
    ggplot2::coord_flip()+
    ggplot2::facet_wrap(vars(!!cat2), scales="free")+
    ggplot2::scale_x_discrete(breaks=NULL)+
    ggplot2::theme(legend.position="none")+
    ggplot2::labs(x=cat1,y="specificity score")
  return(p)
}
