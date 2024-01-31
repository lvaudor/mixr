library(tidyverse)
languages=c("sw","sp","pt","it","gr","gl","fr","en","de")
for(i in 1:length(languages)){
  language=languages[i]
  lexicon=readr::read_delim(stringr::str_c("data-raw/dictionnaires/lexique_",
                                           language,".txt"),
                    "\t", escape_double = FALSE, col_names = FALSE,
                    trim_ws = TRUE) %>%
    dplyr::select(1:3)
  colnames(lexicon)=c("word","lemma","type")
  assign(paste0("lexicon_",language),lexicon)
}
usethis::use_data(lexicon_sw)
usethis::use_data(lexicon_sp)
usethis::use_data(lexicon_pt)
usethis::use_data(lexicon_it)
usethis::use_data(lexicon_gr)
usethis::use_data(lexicon_gl)
usethis::use_data(lexicon_fr)
usethis::use_data(lexicon_en)
usethis::use_data(lexicon_de)
