#' Returns tibble with frequencies of cat (lemma or word for instance) in data
#' @param mydf a tibble
#' @param cat words or lemmas, for instance
#' @param top_freq how many items by category (filter based on frequency) should be kept. If not provided (the default) everything is kept.
#' @param min_freq which is the minimum specificity for an item to be kept. If not provided (the default) everything is kept.
#' @return a tibble of cat frequencies
#' @export
#' @examples
#' mydf <- tibble::tibble(txt=janeaustenr::prideprejudice) %>%
#'   tidytext::unnest_tokens(word,txt)
#' tidy_frequencies(mydf, word, min_freq=200)
tidy_frequencies <- function(mydf,
                             cat,
                             top_freq=NA,
                             min_freq=NA){
    qcat=rlang::enquo(cat)
    freq_data <- mydf %>%
      dplyr::group_by(!!qcat) %>%
      dplyr::summarise(freq=dplyr::n()) %>%
      dplyr::arrange(dplyr::desc(.data$freq))
    if(!is.na(top_freq)){
      freq_data <- freq_data %>%
        dplyr::top_n(top_freq)
    }
    if(!is.na(min_freq)){
      freq_data <- freq_data %>%
        dplyr::filter(.data$freq>=min_freq)
    }
    return(freq_data)
}
