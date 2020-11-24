#' Returns a plot showing frequencies of cat (lemma or word for instance) in data
#' @param df_freq a tibble with frequencies of cat
#' @param cat words or lemmas, for instance
#' @param frequency frequency of cat
#' @param scale type of transformation (if any) to apply to the x-axis (e.g. "log","sqrt",etc.)
#' @return a plot
#' @export
#' @examples
#' mydf <- tibble::tibble(txt=janeaustenr::prideprejudice) %>%
#'     tidytext::unnest_tokens(word,txt)
#' df_freq <- tidy_frequencies(mydf, word, min_freq=500)
#' plot_frequencies(df_freq,
#'                     cat=word,
#'                     frequency=freq)
plot_frequencies=function(df_freq,cat, frequency,scale=NA){
  qcat=rlang::enquo(cat)
  df_freq <- df_freq %>%
    dplyr::arrange(.data$freq) %>%
    dplyr::mutate(id=1:length(.data$freq))
  p=ggplot2::ggplot(df_freq,
           ggplot2::aes(x=.data$id,y=.data$freq)) +
    ggplot2::geom_bar(stat="identity", alpha=0.5, fill="turquoise")+
    ggplot2::geom_text(ggplot2::aes(label=!!qcat, y=0), hjust=0)+
    ggplot2::coord_flip()+
    ggplot2::scale_x_discrete(breaks=NULL)+
    ggplot2::labs(x=qcat,y="frequency")
  if(!is.na(scale)){
    p=p+ggplot2::scale_y_continuous(trans=scale)
  }
  return(p)
}
