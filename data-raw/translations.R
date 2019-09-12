## Script for creating the translations
library(tidyverse)
tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,

  # main tab titles
  'actual_tab_title', "Tururu", "Tururu", "Tururu"

  ## TODO continue translations thesaurus
) %>%
  {.} -> app_translations



usethis::use_data(
  app_translations,

  internal = TRUE, overwrite = TRUE
)
