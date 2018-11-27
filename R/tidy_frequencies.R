#' Returns tibble with frequencies of cat (lemma or word for instance) in data
#' @param data a tibble
#' @param cat words or lemmas, for instance
#' @param criterion should the output be filtered by top specificities by category ('top_n') or according to a minimum value of specificity ("spec_min").
#' @param top_n in case criterion=='top_n', how many items by category should be kept (defaults to 50)
#' @param spec_min in case criterion=='min_spec', which is the minimum specificity for an item to be kept (defaults to 2)
#' @return a tibble of cat frequencies
#' @export
#' @examples
#' library(janeaustenr)
#' df <- tibble(txt=prideprejudice) %>% unnest_tokens(word,txt)
#' tidy_frequencies(df, word, criterion="n_min",n_min=200)
tidy_frequencies <- function(data, cat, criterion="top_n", top_n=30, n_min=10){
    qcat=enquo(cat)
    freq_data <- data %>%
      group_by(!!qcat) %>%
      summarise(n=n()) %>%
      arrange(desc(n))
    if(criterion=="top_n"){
      freq_data <- freq_data %>%
        top_n(top_n,n)
    }
    if(criterion=="n_min"){
      freq_data <- freq_data %>%
        filter(n>=n_min)
    }
    return(freq_data)
}
