# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang) {

  app_translations

  id %>%
    purrr::map_chr(
      ~ app_translations %>%
        dplyr::filter(text_id == .x) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            .x
          } else {
            dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
          }
        }
    )
}
