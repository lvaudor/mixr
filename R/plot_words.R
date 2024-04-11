#' Plot a metric according to word (for instance, frequencies)
#' @param df a tibble with words (or any string to be displayed) and metric (frequency for instance)
#' @param words the name of the column with words
#' @param metric the name of the column with the considered metric
#' @return a plot
#' @export
#' @examples
#' mydf=dplyr::bind_rows(tibble::tibble(txt=janeaustenr::prideprejudice,
#'                                      novel="Pride and Prejudice"),
#'                       tibble::tibble(txt=janeaustenr::sensesensibility,
#'                                      novel="Sense and Sensibility")) %>%
#'   tidytext::unnest_tokens(word,txt)
#' df_freq <- tidy_frequencies(mydf,
#'                             cat=word,
#'                             top_freq=20)
#' plot_words(df_freq,word,freq)
plot_words <- function(df, words, metric, fill=NULL,fill_fixed="grey50") {
  fill=rlang::enquo(fill)
  p = ggplot2::ggplot(df,
                      ggplot2::aes(x = forcats::fct_reorder({{words}},{{metric}}),
                                   y = {{metric}}))
  if(!rlang::quo_is_null(fill)){
    p=p + ggplot2::geom_col(ggplot2::aes(fill={{fill}}),
                            alpha=0.5)
  }else{
    p=p + ggplot2::geom_col(fill=fill_fixed,
                            alpha=0.5)
  }
  p = p +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(x={{words}}, y = 0,
                                    label = {{words}}),
                       hjust = 0) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()) +
    ggplot2::xlab(rlang::enquo(words))
  return(p)
}
