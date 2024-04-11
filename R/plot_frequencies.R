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
#'                  cat=word,
#'                  frequency=freq)
plot_frequencies=function(df_freq, cat, frequency, scale=NA, fill=NULL, fill_fixed="grey50"){
  fill=rlang::enquo(fill)
  cat=rlang::enquo(cat)
  frequency=rlang::enquo(frequency)
  p = ggplot2::ggplot(df_freq,
                      ggplot2::aes(x = forcats::fct_reorder({{cat}},{{frequency}}),
                                   y = {{frequency}}))
  if(!rlang::quo_is_null(fill)){
    p=p + ggplot2::geom_col(ggplot2::aes(fill={{fill}}),
                            alpha=0.5)
  }else{
    p=p + ggplot2::geom_col(fill=fill_fixed,
                            alpha=0.5)
  }
  p = p +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(x={{cat}}, y = 0,
                                    label = {{cat}}),
                       hjust = 0) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::xlab(rlang::enquo(cat))
  return(p)
}
