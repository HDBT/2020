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
options(shiny.erro = browser)

# For dropdown menu
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
                sliderInput("sex", "Geschlecht", 0, 1, 0, step = 1)
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

# Define server logic required to draw a histogram
library(ggvis) 
library(dplyr)
source("global.r")
#write.csv(df,"df.csv")
options(shiny.reactlog = T)
server <- function(input, output) {
    
    # filter the obs, returning a subset dataframe
    dfs <- reactive({ 
        Sex <- input$sex   #first creating temp var, because of issues with dplyr, maybe solved.
        
        #apply filters
        tempD <-  df %>% 
            filter(sex == Sex) 
        #%>% arrange(Zufriedenheit) 
        
        # Optional: filter by genre
        # if (input$genre != "All") {
        #     genre <- paste0("%", input$genre, "%")
        #     m <- m %>% filter(Genre %like% genre)
        # }
        # 
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
        # xvar <- prop("x", as.symbol(input$xvar))
        # yvar <- prop("y", as.symbol(input$yvar))
        # xvar <- 1
        # yvar <- d$Zufriedenheit
        # 
        dfs %>% 
            ggvis(x = ~sex, y = ~Zufriedenheit) #%<% 
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
