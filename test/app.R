jscode <- "var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('mydata', language);
console.log(language);"
library(shiny); library(shinyjs)
library(shiny.i18n)

i18n <- Translator$new(translation_json_path = "C:/Users/hamid/Documents/R/2020/translation.json")

shinyApp(
    ui = fluidPage(
        useShinyjs(),
        titlePanel(i18n$at("Update highcharter dynamically")),
        "This is your browser language",
        titlePanel(translate_with_google_cloud("translate", "de")),
        textOutput("jj"),
        column(4,textOutput(i18n$at("Hallo")))
        
    ),
    server = function(input, output,session) {
        runjs(jscode)
        output$your_lang <- renderPrint(input$mydata)
        reactive(i18n$set_translation_language(input$mydata))
    }
)