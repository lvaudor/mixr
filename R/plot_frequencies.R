#' Returns a plot showing frequencies of cat (lemma or word for instance) in data
#' @param data a tibble with frequencies of cat
#' @param cat words or lemmas, for instance
#' @param frequency frequency of cat
#' @return a plot
#' @export
#' @examples
#' library(janeaustenr)
#' df <- tibble(txt=prideprejudice) %>% unnest_tokens(word,txt)
#' df_freq <- tidy_frequencies(df, word, criterion="n_min",n_min=500)
#' plot_frequencies(df_freq, word,n)
plot_frequencies=function(data,cat, frequency, scale=NA){
  qcat=enquo(cat)
  data <- data %>%
    arrange(n) %>%
    mutate(id=1:length(n))
  p=ggplot(data,
           aes(x=id,y=n)) +
    geom_bar(stat="identity", alpha=0.5, fill="turquoise")+
    geom_text(aes(label=!!qcat, y=0), hjust=0)+
    coord_flip()+
    scale_x_discrete(breaks=NULL)+
    labs(x=qcat,y="frequency")
  if(!is.na(scale)){
    p=p+scale_y_continuous(trans=scale)
  }
  return(p)
}
