## Translations ####
library(tidyverse)
tibble::tribble(
  ~text_id, ~translation_cat, ~translation_eng, ~translation_spa,

  # main tab titles
  'actual_tab_title', "Tururu", "Tururu", "Tururu"

  ## TODO continue translations thesaurus
) %>%
  {.} -> app_translations

## QA needed data ####
qa_years <- 1976:2016
qa_sum <- vector('list', length(qa_years))
qa_list <- vector('list', length(qa_years))

for (i in 1:length(qa_years)) {
  qa_list[[i]] <- readRDS(
    file.path(
      '/Datasets', 'Climate', 'Products', 'MeteorologyInterpolationData',
      'CrossValidations', paste0('CV_', qa_years[[i]], '.rds')
    )
  )
  qa_sum[[i]] <- summary(qa_list[[i]])
}

qa_vars <- row.names(qa_sum[[1]])
qa_statistics <- names(qa_sum[[1]])

# Calibrations data
load('data-raw/calibrations.RData')

# Stations data
load('data-raw/stations_data.RData')

# grid as point topography
load('data-raw/grid_as_points_topography.RData')

usethis::use_data(
  # translations
  app_translations,
  # QA
  qa_sum, qa_vars, qa_statistics,
  # calibrations
  prec_cal, tdew_cal, tmax_cal, tmin_cal,
  # stations
  stations_data,
  # grid as point topography
  grid_as_points_topography,

  internal = TRUE, overwrite = TRUE
)
