#' This script demonstrates how to use shiny.i18n Translator object
#' for live language change on the UI side. Two key steps are:
#' (a) add `usei18n(i18n)` to UI
#' (b) use `update_lang` function to change the language in session

library(shiny)
library(shiny.i18n)
library(shinyjs)

# File with translations
i18n <- Translator$new(automatic = T)
i18n$set_translation_language("en") # here you select the default translation to display

ui <- fluidPage(
    #
    div(style = "float: right;",
        selectInput('selected_language',
                    i18n$at("Change language"),
                    choices = c("de","en"),
                    selected = i18n$get_key_translation())
        
    ),
    titlePanel(i18n$at("Hello Shiny!"), windowTitle = NULL),
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("bins",
                        i18n$at("Number of bins:"), # you use i18n object as always
                        min = 1,
                        max = 50,
                        value = 30)
            
        ),
        mainPanel(tags$head(tags$style(HTML("#OnTime{height:20vh !important;} "))),
                  title = "On Time", status = "primary", solidHeader = TRUE, width = 6,
                  highchartOutput("test"),
            plotOutput("distPlot"),
            p(i18n$at("This is description of the plot.")),
            radioButtons("test",i18n$at("Sociodemographic"), choices = list(i18n$at("Migration") = "Hallo", "Alter" = "JD", "Geschlecht" =" SJ"), selected = "Migration")
            
        )
    )
)

server <- function(input, output, session) {
    
    observeEvent(input$selected_language, {
        # This print is just for demonstration
        print(paste("Language change!", input$selected_language))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selected_language)
    })
    
    output$distPlot <- renderPlot({
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins,
             col = "darkgray", border = "white",
             main = i18n$at("Histogram of x"), ylab = i18n$at("Frequency"))
    })
    
    output$test <- renderHighchart({
        ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.name);}")
        
        dfx <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3),y2= c( 3,4,5,2,1) )
        
        
        highchart() %>% 
            hc_chart(type = "column")%>%
            hc_plotOptions(events = list(click = ClickFunction)) %>% 
            hc_xAxis(categories = dfx$name) %>% 
            hc_add_series(name= i18n$at("No migration background"), data =dfx$y )%>%
            hc_add_series(name= "Parents imigrated",data =dfx$y1 ) %>%
            hc_add_series(name= "Self-Immigrated", data =dfx$y2) 
    })
    makeReactiveBinding("outputText")
    
    observeEvent(input$Clicked, {
        print(paste0(input$Clicked))
        outputText <<- paste0(input$Clicked)
    })
    
    output$text <- renderText({
        outputText
    })
   
}

shinyApp(ui, server)