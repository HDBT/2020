#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggvis)
#options(shiny.error = browser)

# For dropdown menu #useless?
actionLink <- function(inputId, ...) {
    tags$a(href='javascript:void',
           id=inputId,
           class='action-button',
           ...)
}
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Titel"),
    
    fluidRow(
        column(3,
            wellPanel(
                h4("Filter"),
                sliderInput("einkommen", "Einkommen", 0, 100, c(0,100), step = 1),
                selectInput("sex", "Geschlecht", c("Beide","Weiblich","Maennlich")),
                textInput("stimmung", "Stimmung eingeben z.b. hoffnungsvoll")
            ),
            wellPanel(
                selectInput("xvar", "X-Achse-Variable bestimmen", axis_vars, selected = "einkommen"),
                selectInput("yvar", "Y-Achse var bestimmen", axis_vars, selected = "Zufriedenheit")

                
            )

               ),
        column(9,
               ggvisOutput(("plot1")
                           ))
    )
)
    # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
#

# Define server logic required to draw a histogram
library(ggvis) 
library(dplyr)
source("global.r")
#write.csv(df,"df.csv")
options(shiny.reactlog = T)
server <- function(input, output) {
    
    # filter the obs, returning a subset dataframe
    dfs <- reactive({ 
        tempMinEinkommen <- input$einkommen[1]   #first creating temp var, because of issues with dplyr, maybe solved.
        tempMaxEinkommen <- input$einkommen[2] 
        #apply filters
        tempD <-  df %>% 
            filter(
                einkommen >= tempMinEinkommen,
                einkommen <= tempMaxEinkommen
                ) 
        #%>% arrange(Zufriedenheit) 
        
       # Optional: filter by genre
        if (input$sex != c("Beide")) {
            tempSex <- if_else(input$sex == "Weiblich",1,0)
            tempD <- df %>% filter(sex ==tempSex)
        }

        tempD <- as.data.frame(tempD)
        })
    
    # Function for generating tooltip text
    # movie_tooltip <- function(x) {
    #     if (is.null(x)) return(NULL)
    #     if (is.null(x$ID)) return(NULL)
    #     
    #     # Pick out the movie with this ID
    #     all_movies <- isolate(movies())
    #     movie <- all_movies[all_movies$ID == x$ID, ]
    #     
    #     paste0("<b>", movie$Title, "</b><br>",
    #            movie$Year, "<br>",
    #            "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
    #     )
    # }

    # A reactive expression with the ggvis plot
    vis <- reactive({
        #lables for axes 
        # xvar_name <- names(axis_vars)[axis_vars == input$xvar]
        # yvar_name <- names(axis_vars)[axis_vars == input$yvar]
        
        # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
        # but since the inputs are strings, we need to do a little more work.
          xvar <- prop("x", as.symbol(input$xvar))
          yvar <- prop("y", as.symbol(input$yvar))
        # xvar <- 1
        # yvar <- d$Zufriedenheit
        # 
        dfs %>% 
            ggvis(x = xvar, y = yvar) #%<% 
            #layer_points(size:=50:)
        
    })
    
    vis %>% bind_shiny("plot1")
}
    
    #output$n_movies <- renderText({ nrow(movies()) })
    
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }

# Run the application 
shinyApp(ui = ui, server = server)
