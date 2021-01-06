library(highcharter)
library(shiny)
library(shinyjs)

df <- data.frame(
    a = floor(runif(10, min = 1, max = 10)),
    b = floor(runif(10, min = 1, max = 10))
)

updaterfunction <- function(sendid, df, session) {
    message = jsonlite::toJSON(df)
    session$sendCustomMessage(sendid, message)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Update highcharter dynamically"),
    #includeScript("www/script.js"),
    useShinyjs(),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            actionButton("data", "Generate Data")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            highchartOutput("plot")
        )
    )
)


server <- function(input, output, session) {
    
    sendid <- "handler"
    chartid <- "#plot"
    
    jscode <- paste0('Shiny.addCustomMessageHandler("', sendid, '", function(message) {
        var chart1 = $("', chartid, '").highcharts()

        var newArray1 = new Array(message.length)
        var newArray2 = new Array(message.length)

        for(var i in message) {
            newArray1[i] = message[i].a
            newArray2[i] = message[i].b
        }

        chart1.series[0].update({
            // type: "line",
            data: newArray1
        }, false)

        chart1.series[1].update({
        //   type: "line",
          data: newArray2
      }, false)

      console.log("code was run")

      chart1.redraw();
    })')
    
    runjs(jscode)
    
    
    observeEvent(input$data, {
        
        df1 <- data.frame(
            a = floor(runif(10, min = 1, max = 10)),
            b = floor(runif(10, min = 1, max = 10))
        )
        
        updaterfunction(sendid = sendid, df = df1, session = session)
        
    })
    
    
    output$plot <- renderHighchart({
        
        highchart() %>%
            
            hc_add_series(type = "bar", data = df$a) %>%
            hc_add_series(type = "bar", data = df$b)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)