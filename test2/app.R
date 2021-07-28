if (interactive()) {
  library(devtools)
  #devtools::install_github("hdbt/shiny.i18n")
  library(shiny)
  library(shinyjs)
  library(shinyWidgets)
  library(shinyjs)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shiny.i18n) #dev version wegen google probs
  setwd('~/R/2020/module')
  i18n <- Translator$new(translation_json_path = "translation.json")
  #i18n <- Translator$new(automatic = TRUE)
  i18n$set_translation_language('de')
  
  translations_file <- jsonlite::fromJSON("translation.json")
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(usei18n(i18n),
        actionButton("toggle", "Toggle flip box"),
        uiOutput("active_side"), 
        useShinyjs(),
        
        tags$span(style='float: right; bottom: 200;',
                  selectInput(width = 105,
                              inputId='selected_language',
                              label= NULL,
                              choices = c("English" = "en", "Deutsch" = "de", "Français" = "fr"),
                              selected = i18n$get_key_translation()
                   ) ),
        flipBox(
          id = "myflipbox",
          front =  tags$div(titlePanel(
            tags$span(class = "i18n", `data-key` ="<h4 style='text-align: center;'><a href='https://www.jugend-in-luxemburg.lu/youth-survey/'>Youth Survey Luxembourg</a></h4> <p style='text-align: justify;'>The Youth Survey Luxembourg is a representative, large-scale survey of Luxembourg residents.</p> <p style='text-align: justify;'>The target population of the Youth Survey Luxembourg 2019 is comprised of residents of Luxembourg who are 16&ndash;29 years old, regardless of their nationality or country of birth. Sampling frame and sources of information Data provided by the Institut National de la Statistique et des Etudes Economiques du Grand-Duch&eacute; de Luxembourg (STATEC) was used for sampling and weighting calculations for the Youth Survey Luxembourg.</p> <h4 class='LC20lb DKV0Md' style='text-align: center;'><a href='https://www.jugend-in-luxemburg.lu/yac-plus/'> Young People and COVID-19 (YAC+)</a></h4> <p style='text-align: justify;'>To assess the situation during and after the pandemic, two surveys will be conducted in 2020 and 2021 based on the Youth Survey Luxembourg 2019 and in close collaboration with the research group of the Child and Adolescent Health Study '<a href='https://www.jugend-in-luxemburg.lu/hbsc-kooperation/'>Health Behavior in School-Aged Children</a>'.</p> <div class='elementor-element elementor-element-289e4d2 elementor-widget elementor-widget-text-editor' data-id='289e4d2' data-element_type='widget' data-widget_type='text-editor.default'> <div class='elementor-widget-container'> <div class='elementor-text-editor elementor-clearfix'> <p style='text-align: justify;'>For YAC+, this group will be supplemented with children and adolescents aged 12 to 16. Thus, the age group of 12 to 29 years old can be surveyed.</p> <p style='text-align: justify;'>These standardized surveys will be supplemented by qualitative interviews to gain a deeper understanding of the situation and subjective evaluations of adolescents and young adults.</p> </div> </div> </div>", HTML("text"))),
                      radioGroupButtons("thema",i18n$t('Year'), choiceNames = c("2019","2020","Differences"),choiceValues = c("2019","2020","Differences"), size = "normal",direction = "horizontal"),
                      fluidRow(tags$span(class = "i18n", `data-key` = '&lt;p style="text-align: justify;"&gt;&lt;strong&gt;Differences &lt;/strong&gt; &lt;/p&gt;',HTML("<b>Hallo</b>")),
                               column(2,
                                      fluidRow(
                                               column(1),
                                               column(11,
                                                      br(),
                                                      radioGroupButtons("test",i18n$t("Sociodemographic"), choices = c("None", "Age", "Gender","Status"),size = "normal",direction = "vertical", selected = "None")
                                                      #,highchartOutput("hcchart2")
                                               ) 
                                      )     
                              )
                      )
                    ),
          back = NULL
          
        )
      )
    ),
    
    server = function(input, output, session) {
      output$active_side <- renderUI({
        side <- if (input$myflipbox) "front" else "back"
        dashboardBadge(side, color = "blue")
      })
   
      shinyjs::runjs('document.getElementsByName("thema").innerHTML = "Halloo";')
      
      observeEvent(input$toggle, {
        updateFlipBox("myflipbox")
      })
      observeEvent(input$selected_language, {
        shiny.i18n::update_lang(session, input$selected_language)
      })
    }
  )
}