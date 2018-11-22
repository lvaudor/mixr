#' Returns the table of expressions in a given language.
#' This function uses the IRaMuTeQ package's dictionnaries.
#' @param language. Can be one of "de" (German),"en" (English),"fr" (French),"gl" (Galician),"gr" (Greek),"it" (Italian),"pt" (Portuguese),"sp" (Spanish),"sw" (Swedish)
#' @return tibble with word= word or expression, word_cor= corrected word or expression. For instance, in French, "aujourd'hui" is corrected as "ajourd_hui", and "curriculum vitae" is corrected as "curriculum_vitae".
#' @examples
#' fr_expr=get_expressions("fr")
get_expressions=function(language="fr"){
  root=find.package("linkR")
  expressions <- suppressMessages(
    suppressWarnings(
      readr::read_delim(str_c(root,"/dictionnaires/expression_",language,".txt"),
                                   "\t", escape_double = FALSE, col_names = FALSE,
                                   trim_ws = TRUE)
      ))
  expressions <- dplyr::select(expressions, 1:2)
  colnames(expressions)=c("word","word_cor")
  return(expressions)
}
