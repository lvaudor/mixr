#' Plot specificities of cat1 (lemma or word) according to cat2 (categories)
#' @param df_spec a tibble with specifictities of cat1 according to cat2
#' @param cat1 words or lemmas
#' @param cat2 categories
#' @return a plot
#' @export
#' @examples
#' mydf=dplyr::bind_rows(tibble::tibble(txt=janeaustenr::prideprejudice,
#'                                      novel="Pride and Prejudice"),
#'                       tibble::tibble(txt=janeaustenr::sensesensibility,
#'                                      novel="Sense and Sensibility")) %>%
#'  tidytext::unnest_tokens(word,txt)
#' df_spec <- tidy_specificities(mydf,
#'                               cat1=word,
#'                               cat2=novel,
#'                               min_spec=5)
#' plot_specificities(df_spec,
#'                    cat1=word,
#'                    cat2=novel)
plot_specificities=function(df_spec, cat1, cat2){
  cat1 <- rlang::enquo(cat1)
  cat2 <- rlang::enquo(cat2)
  df_spec <- df_spec %>%
    dplyr::group_by(!!cat2)
  df_spec <- df_spec %>%
    dplyr::arrange(!!cat2,.data$spec)%>%
    dplyr::mutate(id=1:length(.data$spec))
  p=ggplot2::ggplot(df_spec,
                    ggplot2::aes(x=df_spec$id,
                                 y=df_spec$spec,
                                 fill=factor(!!cat2)))+
    ggplot2::geom_bar(stat="identity", alpha=0.5)+
    ggplot2::geom_text(ggplot2::aes(label=!!cat1, y=0), hjust=0)+
    ggplot2::coord_flip()+
    ggplot2::facet_wrap(ggplot2::vars(!!cat2), scales="free")+
    ggplot2::scale_x_discrete(breaks=NULL)+
    ggplot2::theme(legend.position="none")+
    ggplot2::labs(x=cat1,y="specificity score")
  return(p)
}
