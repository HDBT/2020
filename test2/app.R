library(shiny)
library(highcharter)


hc_base <- highchart() %>% 
  hc_xAxis(categories = citytemp$month) %>% 
  hc_add_series(name = "Tokyo", data = citytemp$tokyo) 

ui <- fluidPage(                                        prettyToggle(inputId = "switch1",label_on ="e", label_off = "k", icon_on = icon("thumbs-up"),icon_off = icon("thumbs-down") ),

  h2("Viewer"),
  fluidRow(
    h3(""), highchartOutput("hc_1", width = "100%", height = "800px"),
    h3("Click"), verbatimTextOutput("hc_1_input2")
  )
)
server = function(input, output) {
  output$hc_1 <- renderHighchart({
    hc_base %>% 
      hc_add_theme(hc_theme_ffx())%>%
      hc_tooltip(backgroundColor="skyblue",crosshairs = TRUE, borderWidth = 5, valueDecimals=2)%>%
      hc_add_event_point(event = "click")
  })
  
  observeEvent(input$hc_1_click,{
    output$hc_1 <- renderHighchart({
      hc_base %>% 
        hc_add_theme(hc_theme_ffx())%>%
        hc_tooltip(backgroundColor="skyblue",crosshairs = TRUE, borderWidth = 5, valueDecimals=2)%>%
        hc_add_series_scatter(cars$speed, cars$dist)
    })
    
  })
  
  output$hc_1_input2 <- renderPrint({input$hc_1_click })
}
shinyApp(ui = ui, server = server)