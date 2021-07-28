usei18n <- function(translator, ns = NULL) {
  if (is.null(ns)) {
    ns <- function(id) id
  shiny::addResourcePath("shiny_i18n", system.file("www", 
                                                   package = "shiny.i18n"))
  js_file <- file.path("shiny_i18n", "shiny-i18n.js")
  translator$use_js()
  translations <- translator$get_translations()
  key_translation <- translator$get_key_translation()
  translations[[key_translation]] <- rownames(translations)
  shiny::tagList(shiny::tags$head(shiny::tags$script(glue::glue("var i18n_translations = {toJSON(translations, auto_unbox = TRUE)}")), 
                                  shiny::tags$script(src = js_file)), i18n_state(translator$key_translation))
}
