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
usethis::use_data(lexicon_de)


lexicon_en_missing=readr::read_csv("data-raw/dictionnaires/missing_words_lexicon_en.csv") %>%
  mutate(type="unspecified")
lexicon_en=lexicon_en %>%
  bind_rows(lexicon_en_missing) %>%
  arrange(word,lemma)
usethis::use_data(lexicon_en, overwrite=TRUE)
