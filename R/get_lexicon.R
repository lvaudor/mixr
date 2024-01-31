#' Returns a lexicon in a given language.
#' This function uses the IRaMuTeQ package's dictionaries.
#' @param language Can be one of "
#' * "de" (German),
#' * "en" (English),
#' * "fr" (French),
#' * "gl" (Galician),
#' * "gr" (Greek),
#' * "it" (Italian),
#' * "pt" (Portuguese),
#' * "sp" (Spanish),
#' * "sw" (Swedish)
#' @return tibble with word, lemma and type (.
#' @details Type: a word (in French lexicon) can be of type:
#' * adj: adjective
#'    + adj_dem: demonstrative adjective
#'    + adj_ind: indefinite adjective
#'    + adj_int : interrogative adjective
#'    + adj_num: numerical adjective
#'    + adj_pos: possessive adjective
#'    + adj_sup:
#' * adv: adverb
#'    + adv_sup
#'* art_def: definite article
#' * aux: auxiliary
#' * con: conjunction
#' * nom: nominal
#' * ono: onomatopoeia
#' * pre: preposition
#' * pro: pronoun
#'    + pro_dem: demonstrative pronoun
#'    + pro_ind: indefinite pronoun
#'    + pro_per: personnal pronoun
#'    + pro_pos: possessive pronoun
#'    + pro_rel: relative pronoun
#' * ver: verb
#'    + ver_sup:
#' @export
#' @examples
#' fr_expr=get_lexicon("fr")
get_lexicon=function(language="fr"){
  lexicon=paste0("lexicon_",language)
  data(list=lexicon)
  lexicon=get(lexicon)
  return(lexicon)
}
