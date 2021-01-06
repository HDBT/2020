#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

#loading the packages
library(shinydashboard)
require(shiny)
require(highcharter)

#layout of the dashboard
#defining character vectors for select inputs
country<-c("India","United States","Mexico","Canada","China, People's Republic of","Japan","Russian Federation","Germany","United Kingdom","European Union",
           "ASEAN-5","New Zealand","Australia","Netherlands","Luxembourg",
           "France","Qatar","United Arab Emirates","Saudi Arabia")

unions<-c("Major advanced economies (G7)","European Union","Emerging and Developing Europe","ASEAN-5","Commonwealth of Independent States",
          "Emerging and Developing Asia","Latin America and the Caribbean",
          "Middle East, North Africa, Afghanistan, and Pakistan")
#function used to define the dashboard 
dashboardPage(
    #defines header
    skin = "red",
    
    #header of the dashboard
    dashboardHeader(
        title="Inflation Rates" ,
        dropdownMenu()
    ),
    #defines sidebar of the dashboard
    dashboardSidebar(
        sidebarMenu(
            #the sidebar menu items
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("About", tabName = "about", icon = icon("th")),
            menuItem("Trade Unions",tabName="unions",icon=icon("signal")),
            menuItem("World",tabName="world",icon=icon("globe"))      
        )),
    #defines the body of the dashboard
    dashboardBody(
        #to add external CSS
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            #First TAB Menu-dashboard- first argument should be the 'tabName' value of the menuItem function  
            tabItem(tabName = "dashboard",
                    fluidRow(
                        column(12,
                               #box() is similar to a 'div' element in HTML
                               box(
                                   selectInput("country",label="Select Country",choices=country), width = 12)# end box 
                        ),#end column
                        #box for plotting the time series plot
                        column(12,
                               box(
                                   #below function is used to define a highcharter output plot which will be made in the server side
                                   highchartOutput("hcontainer"),
                                   width="12") #end box2
                        ), #end column
                        br(), #line break
                        h4("Relative inflation rates time series plot",align="center"),
                        br(),
                        column(12,
                               box(highchartOutput("hc2"),width=12))
                    ),#end row
                    h4("Made with love from", strong("Anish Singh Walia")),
                    a("R code for this project",target="_blank",href="https://github.com/anishsingh20/Analzying-Inflation-Rates-Worldwide")
            ),
            #second tab menu- ABOUT
            tabItem(tabName="about",
                    h2("What is Inflation ?",style="text-align:center"),
                    br(),
                    br(),
                    box(width=12,height="400px",
                        p(style="font-size:20px",strong("Inflation"),"rates are the general rate at which price of the goods and services within a particular economy are rising and the purchasing power of the currency is declining due to the higher priced goods. High inflation is definitely not good for an economy because it will always reduce the value for money.In general central banks of an economy tries to and work towards reducing the inflation rate and avoiding deflation."),
                        
                        p(style="font-size:20px",strong("Deflation"), "is opposite of inflation. Deflation occurs when the inflation rates become negative or are below 0. Deflation is more harmful and dangerous for an economy because it means that the prices of goods and services are going to decrease. Now, this sounds amazing for consumers like us. But what actually happens is that the demand for goods and services have declined over a long term of time. 
This directly indicates that a recession is on its way. This brings job losses, declining wages and a big hit to the stock portfolio. Deflation slows economy's growth. As prices fall, people defer(postpone) purchases in hope of a better lower price deal. Due to this companies and firms have to cut down the cost of their goods and products which directly affects the wages of the employees which have to be lowered.")
                    )
            ),
            tabItem(tabName = "unions", h3("Time series of Inflation rates of Economic trade unions",align="center") ,
                    fluidRow(
                        column(12,
                               box(selectInput("region",label="Select Economic Region",choices=unions),width = 12) 
                        ),
                        box(highchartOutput("hc3"), width=12)
                    )# end row 
            ),
            tabItem(tabName = "world",
                    h3("World's Inflation Rates",align="center") ,
                    box(highchartOutput("hc4"), width=12)
            )
        )#end tabitems
    )#end body
    
)#end dashboard


# Define server logic required to draw a histogram
require(shinydashboard)
require(ggplot2)
require(dplyr)
#install.packages("readxl")
require(highcharter) #to plot amazing time series plots
library(readxl)
require(tidyr)
#loading the dataset
inflation <- read_excel("inflation.xls")
year<-c(1980:2022) #making a vector consisting of all years
year<-as.character(year)#converting to character type to use in gather()

#converting dataset to a long format
inf <- gather(year,key = "Year",value="InflationRate")
inf <- na.omit(inf) #omitting NA values
names(inf) <- c("region","year","inflation")
inf$year<- as.integer(inf$year)

#Making data frame for specific countries and economic unions to be used in plotting

India <- filter(inf,region=="India")
India$inflation<-as.numeric(India$inflation)
India$year<-as.numeric(India$year)
China<-filter(inf,region=="China, People's Republic of")
Ger<-filter(inf,region=="Germany")
Japan<-filter(inf,region=="Japan")
US<-filter(inf,region=="United States")
EU<-filter(inf,region=="European Union")
UK<-filter(inf,region=="United Kingdom")
Fr<-filter(inf,region=="France")
uae<-filter(inf,region=="United Arab Emirates")

#server function and logic
server <- function(input, output) { 
    output$hcontainer <- renderHighchart ({
        
        #write all R-code inside this
        
        df  <- inf %>% filter(region==input$country) #making the dataframe of the country
        #above input$country is used to extract the select input value from the UI and then make 
        #a dataframe based on the selected input
        df$inflation <- as.numeric(df$inflation)
        df$year <- as.numeric(df$year)
        
        #plotting the data
        hchart(df, "line",color="#DC270C",hcaes(x=year,y=inflation))  %>%
            
            hc_exporting(enabled = TRUE) %>% 
            hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                       shared = TRUE, borderWidth = 2) %>%
            hc_title(text="Time series plot of Inflation Rates",align="center") %>%
            hc_subtitle(text="Data Source: IMF",align="center") %>%
            hc_add_theme(hc_theme_elementary()) 
        
    }) # end hcontainer
    
    
    output$hc2 <- renderHighchart({
        
        highchart() %>% 
            hc_xAxis(categories=inf$year) %>% 
            hc_add_series(name = "India", data = India$inflation) %>% 
            hc_add_series(name = "USA", data = US$inflation) %>%
            hc_add_series(name = "UK", data = UK$inflation) %>%
            hc_add_series(name = "China", data = China$inflation) %>%
            hc_add_series(name = "Germany", data = Ger$inflation) %>%
            hc_add_series(name="Japan",data=Japan$inflation) %>%
            #to add colors
            hc_colors(c("red","blue","green","purple","darkpink","orange")) %>%
            hc_add_theme(hc_theme_elementary())
    }) # end hc2
    
    output$hc3<-renderHighchart({
        union <- inf %>% filter(region==input$region)
        union$year <- as.numeric(union$year)
        union$inflation <- as.numeric(union$inflation)
        
        hchart(union,hcaes(x=year,y=inflation),type="area",color="#2B1F97") %>%
            hc_exporting(enabled = TRUE) %>% 
            hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                       shared = TRUE, borderWidth = 2) %>%
            hc_title(text="Time series plot of Inflation Rates for Economic Unions",align="center") %>%
            hc_subtitle(text="Data Source: IMF",align="center") %>%
            hc_add_theme(hc_theme_elementary())
    }) #end hc3
    
    output$hc4<-renderHighchart({
        
        world <- inf %>% filter(region=="World")
        world$year<-as.numeric(world$year)
        world$inflation <-as.numeric(world$inflation)
        
        hchart(world,hcaes(x=year,y=inflation),type="area",color="#B915A3") %>%
            hc_exporting(enabled = TRUE) %>% 
            hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                       shared = TRUE, borderWidth = 2) %>%
            hc_title(text="Time series plot of Inflation Rates for World",align="center") %>%
            hc_subtitle(text="Data Source: IMF",align="center") %>%
            hc_add_theme(hc_theme_elementary())
        
    }) #end hc4
}
# Run the application 
shinyApp(ui = ui, server = server)


