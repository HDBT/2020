library (shiny)
library (shinydashboard)
library (shinydashboardPlus)
library (dplyr)
library (tibble)
library (highcharter)
#install.packages("DT")
library(shinyjs)
library (DT)

rm(list=ls())
header <- dashboardHeader()
sidebar <- dashboardSidebar()

body <- dashboardBody(
    fluidRow(
        flipBox(id=1, back_content = NULL,
            tags$head(tags$style(HTML("#OnTime{height:20vh !important;} "))),
            title = "On Time", status = "primary", solidHeader = TRUE, width = 6,
            highchartOutput("OnTime")
        )
    ),
    fluidRow(
        box(tags$head(tags$style(HTML("test{height:20vh !important;} "))),
            title = "On Time", status = "primary", solidHeader = TRUE, width = 6,
            highchartOutput("test")
        )
    ),
    fluidRow(
        box(
            textOutput("text")
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
    
    Customer <- c("England", "France", "US", "Canada", "England", "France", "US", "Canada", "England")
    OnTime <- c("On Time", "On Time", "Overdue", "On Time", "Overdue", "On Time", "Overdue","On Time", "On Time")
    Gate <- c(1,2,3,2,3,2,1,2,3)
    Quantity <- c(1,1,1,1,1,1,1,1,1)
    
    data <- data.frame(Customer,OnTime,Gate, Quantity)
    
    output$OnTime <- renderHighchart({
        
        Lvl1GroupingStatus <- aggregate(data$Quantity, by = list(data$OnTime),FUN=sum)
        Lvl1dfStatus <- data_frame(name = Lvl1GroupingStatus$Group.1,y = Lvl1GroupingStatus$x,drilldown = tolower(name))
        
        Lvl2WIPOverDue <- data[data$OnTime == "Overdue",]
        Lvl2WIPOverDueb <- aggregate(Lvl2WIPOverDue$Quantity, by = list(Lvl2WIPOverDue$Customer),FUN=sum)
        Lvl2dfWIPOverDue <- arrange(data_frame(name = Lvl2WIPOverDueb$Group.1,value = Lvl2WIPOverDueb$x),desc(value))
        
        Lvl2WIPOnTime <- data[data$OnTime == "On Time",]
        Lvl2WIPOnTimeb <- aggregate(Lvl2WIPOnTime$Quantity, by = list(Lvl2WIPOnTime$Customer),FUN=sum)
        Lvl2dfWIPOnTime <- arrange(data_frame(name = Lvl2WIPOnTimeb$Group.1,value = Lvl2WIPOnTimeb$x),desc(value))
        
        ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.category);}")
        dfx <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3),y2= c( 3,4,5,2,1) )
        dfc <- dfx[c(-4,-2)]
        
        highchart() %>%
            hc_chart(type = "column") %>%
            hc_xAxis(categories = dfx$name) %>% 
            hc_legend(enabled = T) %>%
            hc_yAxis(gridLineWidth = 0) %>%
            hc_plotOptions(series = list(#column = list(stacking = "normal"), 
                                         borderWidth=0,
                                         dataLabels = list(enabled = TRUE),
                                         events = list(click = ClickFunction)
            )
            ) %>%
            hc_add_series(name= i18n$at("No migration background"), data =dfx[c("name","y")]) %>%
            hc_add_series(name= i18n$at("Parents imigrated"),data= dfx$y1) %>% 
            hc_add_series(name= "Self-Immigrated", data =dfx$y2) 
        
        
           # hc_add_series(data=,name="Status", colorByPoint = TRUE,colors = c("#003395","#D20000")) %>%
            # 
            # hc_drilldown(
            #     allowPointDrilldown = TRUE,
            #     series = list(
            #         list(id = "overdue", data = list_parse2(Lvl2dfWIPOverDue), name="Customer"),
            #         list(id = "on time", data = list_parse2(Lvl2dfWIPOnTime), name="Customer")
            #         
            #     )
            # )
    })
    
    output$test <- renderHighchart({
        ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.category);}")
        
        dfx <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3),y2= c( 3,4,5,2,1) )
        
        
        highchart() %>% 
            hc_chart(type = "column")%>%
            hc_legend(enabled = T) %>%
            hc_xAxis(categories = dfx$name) %>% 
            hc_plotOptions(series = list(#column = list(stacking = "normal"), 
                borderWidth=0,
                dataLabels = list(enabled = TRUE),
                events = list(click = ClickFunction))) %>% 
        
            hc_add_series(name= i18n$at("No migration background"), data =dfx[c("name","y")] )%>%
            hc_add_series(name= "Parents imigrated",data =dfx$y1 ) %>%
            hc_add_series(name= "Self-Immigrated", data =dfx$y2) 
    })
    
    makeReactiveBinding("outputText")
    
    observeEvent(input$Clicked, {
        outputText <<- paste0(input$Clicked)
        print(paste0(input$Clicked))
        
    })
    
    output$text <- renderText({
        outputText
    })
    
    output$Table <- DT::renderDataTable({
        
        temp <- data
        rowcheck <- temp[temp$OnTime == input$Clicked,]
        
        if (nrow(rowcheck)!=0) {
            temp <- temp[temp$OnTime == input$Clicked,]
            Lvl1Click <<- input$Clicked
        }
        else {
            temp <- temp[temp$OnTime == Lvl1Click,]
            temp <- temp[temp$Customer == input$Clicked,]
        }
        
        return (temp)
        
    })
    
    #maps https://www.datanovia.com/en/lessons/highchart-interactive-world-map-in-r/
    hcmap(map= "countries/lu/lu-all", data =data.frame(name= c("Diekirch","Grevenmacher","Luxembourg"), value = c(1,3,4)), value = "value", joinBy = "name") %>%
        hc_add_series(name ="fijfo",data = data.frame(name= c("Diekirch","Grevenmacher","Luxembourg"), value = c(5,3,4)), type= "mapbubble")
   

    
    
}

#Combines Dasboard and Data together
shinyApp(ui, server)