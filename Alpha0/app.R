#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#library(geojsonio)
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(ggvis)
library(highcharter)
library(shinyjs)
library(shinyhelper)
library(shinyBS)
#install.packages("shiny.i18n")
#install.packages("fusionchartsR")
#require(fusionchartsR)
#library(shinycustomloader)
source("global.r")
source("flipBox.R")
#options(shiny.error = browser)
#install.packages("shinycustomloader")
# For dropdown menu #useless?
actionLink <- function(inputId, ...) {
    tags$a(href='javascript:void',
           id=inputId,
           class='action-button',
           ...)
}
CSS <- "
@media (max-width: 1000px) { 
  .bootstrap-select > .dropdown-toggle[title='Choose ...'],
  .bootstrap-select > .dropdown-toggle[title='Choose ...']:hover,
  .bootstrap-select > .dropdown-toggle[title='Choose ...']:focus,
  .bootstrap-select > .dropdown-toggle[title='Choose ...']:active,
  .pClass {
    font-size: 12; 
    color: green;
  }
}
@media (min-width: 1001px) { 
  .bootstrap-select > .dropdown-toggle[title='Choose ...'],
  .bootstrap-select > .dropdown-toggle[title='Choose ...']:hover,
  .bootstrap-select > .dropdown-toggle[title='Choose ...']:focus,
  .bootstrap-select > .dropdown-toggle[title='Choose ...']:active,
  .pClass {
    font-size: 18; 
    color: blue;
  }
}"

# sprache -----------------------------------------------------------------
library(shiny.i18n) #dev version wegen google probs

i18n <- Translator$new(translation_json_path = "translation.json")
#i18n <- Translator$new(automatic = TRUE)
i18n$set_translation_language('de')

# UI
ui <- fluidPage(#theme = "bootstrap.css",
  useShinyjs(),
  tags$script(src = "js.js"),
  tags$head(tags$style(HTML('* {font-family: "Helvetica" !important};'))), # * um jedes Element zu selektieren. !important um  optionen in den Kasaden zu überschreiben
  tags$head(tags$style(HTML(".shiny-input-container { font-size: 18px; }"))), #funzt
  tags$head(tags$style(HTML(".highcharts-input-container { font-size: 60px; }"))), #funzt nicht
              fluidRow(id ="first",shiny.i18n::usei18n(i18n),
                       
                        column(12,                    
                               flipBoxN(front_btn_text = "Meta-Information",
                                      id = 1,
                                      main_img = NULL,
                                      header_img = NULL  ,
                                      back_content  = "The target population of the Youth Survey Luxembourg is comprised of residents of Luxembourg who are 16–29 years old, regardless of their nationality or country of birth. Sampling frame and sources of information Data provided by the Institut National de la Statistique et des Etudes Economiques du Grand-Duché  de  Luxembourg  (STATEC)4  was  used  for  sampling  and  weighting calculations  for  the  Youth  Survey  Luxembourg.  
                                      ",
                                      radioGroupButtons("thema",i18n$t("Thema"), choiceNames = c("Identität","Politisches Interesse","Politische Aktion"),choiceValues = c("Identität","Politisches Interesse","Politische Aktion"), size = "normal",direction = "horizontal"),
                                      fluidRow(
                                         column(2,
                                                fluidRow(
                                                   column(1),
                                                   column(11,
                                                          br(),
                                                    radioGroupButtons("test",i18n$t("Sociodemographic"), choices = c("<i class='fa fa-bar-chart'></i>" = "None","Migration", "Alter", "Geschlecht","Status"),size = "normal",direction = "vertical", selected = "None")
                                                    ,highchartOutput("hcchart2")
                                                   ) 
                                                )    
                                         ),
                                        
                                         bsTooltip("test", "Weiterführende Infos","right", options = list(container = "body")),
                                         column(10,
                                                div(highchartOutput("hcchart1"), style = "font-size:15%"),
                                                
                                           
                               
                                                 #actionButton("mybutton", "action"),
                                                tags$style(HTML("#lang_div .shiny-input-container  {font-size: 16px;}")),  #individuelles style setzen indem man eine eigens erstellte id anspricht
                                                 div(id ="lang_div",prettySwitch(inputId = "switch","Spaltendiagramm",slim = T, value = TRUE),#div() um eigene ID zu setzen fürs ansprechen (individuelle style tags z.b.)
                                                 tags$div(  
                                                   style='float: right;width: 70px;',
                                                   selectInput(
                                                     inputId='selected_language',
                                                     label=i18n$t('Change language'),
                                                     choices = i18n$get_languages(),
                                                     selected = i18n$get_key_translation()
                                                   )
                                                 )
                                                 )
                                        )      
                                 
                                       ),
                               )       
                 )     
                       )
             ) 
  

library(ggvis) 
library(plyr)
#library(shinyjs)
library(dplyr)
library(data.table)
library(highcharter)
#write.csv(df,"df.csv")
options(shiny.reactlog = T)
server <- function(input, output,session) {
  source("global.r")
  
    # 
    # # filter the obs, returning a subset dataframe
    # dfs <- reactive({ 
    #     tempMinEinkommen <- input$einkommen[1]   #first creating temp var, because of issues with dplyr, maybe solved.
    #     tempMaxEinkommen <- input$einkommen[2] 
    #     #apply filters
    #     tempD <-  df %>% 
    #         filter(
    #             einkommen >= tempMinEinkommen,
    #             einkommen <= tempMaxEinkommen
    #             ) 
    #     #%>% arrange(Zufriedenheit) 
    #     
    #    # Optional: filter by geschlecht dropdown
    #     if (input$sex != c("Beide")) {
    #         tempSex <- if_else(input$sex == "Weiblich",1,0)
    #         tempD <- df %>% filter(sex ==tempSex)
    #     }
    # 
    #   
    #     
    # 
    #     #filter bei emotion 
    #     # if (!is.null(input$emotion) && input$emotion != ""){
    #     #     tempEmotion <- paste0("%", input$emotion, "%")
    #     #     tempD <- tempD$emotion[tempD$emotion %like%  tempEmotion]
    #     # }
    #     # 
    #     
    #   tempD <- as.data.frame(tempD)
    # 
    # })
    # # Function for generating tooltip text
    # genTooltip <- function(x) {
    #     if (is.null(x)) return(NULL)
    #     if (is.null(x$id)) return(NULL)
    #     
    #     isolDfs <- isolate(dfs())
    #     info <- isolDfs[isolDfs$id == x$id,]
    #     
    #     paste0("<b>", info$sex, "</b><br>",
    #            "$",info$einkommen, "<br>", format(info$Zufriedenheit, big.mark = ",", scientific = FALSE)
    #     )
    #     
    # }


# sprache obs -------------------------------------------------------------

    observeEvent(input$selected_language, {
      # This print is just for demonstration
      print(paste("Language change!", input$selected_language))
      # Here is where we update language in session
      shiny.i18n::update_lang(session, input$selected_language)
    })
    
   #  # i18n <- reactive({
   #  #   selected <- input$selected_language
   #  #   if (length(selected) > 0 && selected %in% translator$get_languages()) {
   #  #     translator$set_translation_language(selected)
   #  #   }
   #  #   translator
   #  # })
   #  # 
   #  #leaflet Data prep
   #  #install.packages("geojsonio")
   #  
   #  #install.packages("leaflet")
   #  library(leaflet)
   #  states <- geojsonio::geojson_read("luxembourg.geojson", what = "sp")
   #  class(states)
   #  states$density <- rnorm(12,50,20)
   #  # Daten agreggieren
   #  
   #  # Reactive expression for the data subsetted to what the user selected
   #  
   #  #agg <- reactive({aggregate(dfs,by = list(dfs$region),FUN = mean,na.rm=TRUE)})
   #  agg <-  reactive({aggregate(dfs(),by = list(dfs()$region),FUN = mean,na.rm=TRUE) })
   #  aggs <- reactive({states})
   #  #aggs$einkommen <- reactive({agg$})
   #  #states$einkommen <- agg$einkommen
   # 
   # 
   # 
   #  bins <- c(0, 30, 40, 50, 60 ,70, Inf)
   #  pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
   #  
   #  labels <- sprintf(
   #    "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
   #    states$name, states$density
   #  ) %>% lapply(htmltools::HTML)
   #  
   # 
   #  output$mymap <- renderLeaflet({
   #    leaflet(aggs(),options = leafletOptions(zoomControl = FALSE,minZoom = 8.7, maxZoom = 8.7,dragging = FALSE)) %>%
   #      addProviderTiles("MapBox", options = providerTileOptions(
   #        id = "mapbox.light",
   #        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>% 
   #      addPolygons(
   #    fillColor = pal(aggs()$density),
   #    weight = 2,
   #    opacity = 1,
   #    color = "white",
   #    dashArray = "3",
   #    fillOpacity = 0.7,
   #    highlight = highlightOptions(
   #      weight = 5,
   #      color = "#666",
   #      dashArray = "",
   #      fillOpacity = 0.7,
   #      bringToFront = TRUE),
   #    label = labels,
   #    labelOptions = labelOptions(
   #      style = list("font-weight" = "normal", padding = "3px 8px"),
   #      textsize = "15px",
   #      direction = "auto")) %>%
   #    addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
   #              position = "bottomright")
   #  
   #  })
   #  
   #  #proxy fuer interaktiionsaenderungen
   #  observe({
   #    #pal <- colorpal()  brauche ich noch nicht
   #    
   #    leafletProxy("mymap", data = agg) 
   #  })
   #  
   #  
   #  #toggle between ggvis and highchartR
   # # whichplot <- reactiveVal(TRUE)  #start of as True 
   #  
   #  
   #  # A reactive expression with the ggvis plot
   #  vis <- reactive({
   #      #lables for axes 
   #      # xvar_name <- names(axis_vars)[axis_vars == input$xvar]
   #      # yvar_name <- names(axis_vars)[axis_vars == input$yvar]
   #      
   #      # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
   #      # but since the inputs are strings, we need to do a little more work.
   #        xvar <- prop("x", as.symbol(input$xvar))
   #        yvar <- prop("y", as.symbol(input$yvar))
   #      # xvar <- 1
   #      # yvar <- d$Zufriedenheit
   #      # 
   #      dfs %>% 
   #          ggvis(x = xvar, y = yvar) %>% 
   #          layer_points(size := 50, size.hover := 200, fillOpacity := 0.2, fillOpacity.hover := 0.5, stroke = ~covid, key := ~id) %>%
   #          add_tooltip(genTooltip,"hover") %>%
   #          add_legend("stroke",title = "Hatte Corona Erfahrung in soz. Umkreis", values = c("Ja","Nein")) %>%
   #          scale_nominal("stroke", domain =  c("Ja","Nein"), range = c("orange","lightblue")) %>%
   #          set_options(width = 800, height =  600)
   #          
   #  })
   #  
   #  vis %>% bind_shiny("plot1")
   #  output$N <- renderText({ nrow((dfs())) })
   #  
    # 
    # 
    # #boxplot + highchart
    # dat <- data_to_boxplot(df, Zufriedenheit, sex,name = "Unterschiede in Zufriedenheit") #fuer highcharter box
    # output$hcontainer <- renderHighchart ({
    #     
    #     #write all R-code inside this
    #     
    #     # df  <- inf %>% filter(region==input$country) #making the dataframe of the country
    #     # #above input$country is used to extract the select input value from the UI and then make 
    #     # #a dataframe based on the selected input
    #     # df$inflation <- as.numeric(df$inflation)
    #     # df$year <- as.numeric(df$year)
    #     
    #     #plotting the data
    #   hchart(df%>% filter(sex == 0), type = "point", hcaes(x = Zufriedenheit, y = einkommen), name = "Männer") %>%
    #     hc_add_series(df %>% filter(sex == 1), type = "point", mapping = hcaes(x = Zufriedenheit, y = einkommen), name = "Frauen", fast = FALSE) 
    #   
    #    #highchart() %>%hc_xAxis(type = "category") %>%hc_add_series_list(dat) 
    #    ## Not run:## End(Not run)data_to_hierarchicalHelper to transform data frame for treemap/sunburst highcharts for-matDescriptionHelper to transform data frame for treemap/sunburst highcharts format
    #    #  hchart(df%>% filter(sex == 0), type = "point", hcaes(x = Zufriedenheit, y = einkommen), name = "Männer") %>%
    #     #     hc_add_series(df %>% filter(sex == 1), type = "point", mapping = hcaes(x = Zufriedenheit, y = einkommen), name = "Frauen", fast = FALSE)
    #     #) 
    #         # hc_exporting(enabled = TRUE) %>% 
    #         # hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
    #         #            shared = TRUE, borderWidth = 2) %>%
    #         # hc_title(text="Time series plot of Inflation Rates",align="center") %>%
    #         # hc_subtitle(text="Data Source: IMF",align="center") %>%
    #         # hc_add_theme(hc_theme_elementary()) 
    #     
    # }) # end hcontainer
    # output$chart2 <- renderHighchart ({
    #               highchart() %>% hc_xAxis(type = "category") %>% hc_add_series_list(dat) 
    # })
    # 

# Tab2 Vis ----------------------------------------------------------------
   output$hcchart1 <- renderHighchart({
    #switch proxy für charttype

    if (input$switch == T)
      {switch <-"column"
    } else { switch <- "bar"
    }  
     dfn <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,26,91,90,89) )
     dfx <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,35,90,82,82), y1 = c (51,24,82,81,81),y2= c(37,36,76,80,82) )
     df_l  <- lst(dfn, dfx)
     l2<-lapply(df_l, function(df) 
       cbind(df, b = df$y *1.1, c = df$y *1.2, d = df$y *0.7))
     
     colors <- c("#e41618","#52bde7","#4d4d52","#90b36d","#f5951f","#6f4b89","#3fb54e","#eea4d8")
     ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.index);}")
     addPopover(session, "hcchart1", "Infos", content = paste0("weiterführende Infos"), trigger = "click")
    hc <-   highchart() %>%
      hc_xAxis(labels = list(style = list(fontSize = "16px"))) %>% 
       hc_yAxis(labels= list(format = "{value} %", style = list(fontSize = "16px"))) %>%
      hc_chart(type = switch)%>%
      hc_colors(colors) %>% 
      hc_title(style = list(fontSize = "18px")) %>%
      hc_subtitle(text = "Luxembourg, 2019") %>%
       hc_plotOptions(series = list(#column = list(stacking = "normal"), 
        borderWidth=0,
        dataLabels = list(style = list(fontSize = "14px"),enabled = TRUE),
        events = list(click = ClickFunction)))%>%
       hc_tooltip(headerFormat = '<span style="font-size:16px">{point.key}</span><table>', pointFormat = '<tr><td style="color:{series.color};font-size:16px;padding:0">{series.name}: </td><td style="padding:0;font-size:16px;"><b>{point.y:.1f} %</b></td></tr>', footerFormat = '</table>', shared = T, useHTML =T) %>%
       hc_exporting(enabled = T, buttons = list(contextButton = list( symbol = "menu"  )), filename = "custom-file-name_Luxembourg_Data") 
      #hc_exporting(enabled = T, buttons = list(contextButton = list( symbol = "menu",text = "Download", menuItems = "null", onclick = JS("function () { this.renderer.label('efwfe',100,100).attr({fill:'#a4edba',r:5,padding: 10, zIndex: 10}) .css({ fontSize: '1.5em'}) .add();}") )), filename = "custom-file-name_Luxembourg_Data") 
      #switch <- switch(input$switch, TRUEE = "column", "FALSE" = "column", "column")
    
     if (input$test == "None" & input$thema == "Identität") {
       #dfn <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,26,91,90,89) )

       hc %>% 
         hc_title(text = "Percentage of answers “Very Important” and “Important” according to the dimensions of National Identity.")%>%
         hc_xAxis(categories = dfn$name ,additonialInfo = 1:4 ) %>% 
         hc_add_series(name= " ",data =df_l$dfn[c("name","y")] ,showInLegend = F)

     }
    
     else if (input$test == "Migration" & input$thema == "Identität") {
     
  
      hc %>% 
      hc_title(text = "Percentage of answers “Very Important” and “Important” according to the dimensions of National Identity by migration.")%>%
      hc_xAxis(categories = dfx$name) %>% 
      hc_add_series(name= i18n$t("No migration background"), data =dfx[c("name","y")] )%>%
      hc_add_series(name= "Parents imigrated",data =dfx$y1 ) %>%
      hc_add_series(name= "Self-Immigrated", data =dfx$y2) 
    }
    else if (input$test == "Alter" & input$thema == "Identität") {
      df2 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(50,30,80,76,75), y1 = c (48,26,81,80,81),y2= c(48,30,79,82,82) )

          hc %>%
            hc_title(text = "Percentage of answers “Very Important” and “Important” according to the dimensions of National Identity by  age.")%>%
          #hc_plotOptions(bar = list(stacking = "percent")) %>%
          hc_xAxis(categories = df2$name) %>%
          hc_add_series(name= "16-20", data =df2$y )%>%
          hc_add_series(name= "21-25",data =df2$y1 ) %>%
          hc_add_series(name= "26-29", data =df2$y2)
    }
   
    else if (input$test == "Geschlecht" & input$thema == "Identität") {

      
      
    df3 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(48,27,82,82,81), y1 = c(47,27,79,78,79))
      hc %>% 
        hc_title(text = "Percentage of answers “Very Important” and “Important” according to the dimensions of National Identity by living gender.")%>%
        
      #hc_plotOptions(bar = list(stacking = "percent")) %>% 
      hc_xAxis(categories = df3$name) %>% 
      hc_add_series(name= "Female", data =df3$y )%>%
      hc_add_series(name= "Male",data =df3$y1 )
    }
    else if (input$test == "Status" & input$thema == "Identität") {
      
      
      
      df4 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(47,26,92,92,85), y1 = c (48,34,89,93,94), y2 = c (49,42,84,76,75))
      hc %>% 
        hc_title(text = "Percentage of answers “Very Important” and “Important” according to the dimensions of National Identity by living status.")%>%
        
        #hc_plotOptions(bar = list(stacking = "percent")) %>% 
        hc_xAxis(categories = df4$name) %>% 
        hc_add_series(name= "Students", data =df4$y )%>%
        hc_add_series(name= "Employed",data =df4$y1 ) %>%
        hc_add_series(name= "NEET",data =df4$y2 )
        
    }
    
    
    else if (input$test == "None" & input$thema == "Politisches Interesse") {
      
      df5 <- tibble(name = c("Extremely","Very","Medium","Not Very","Not at all"),y = c(5,15,39,27,14))
      hc %>% 
        hc_title(text = "How young individuals are interested in politics.")%>%
        #hc_plotOptions(bar = list(stacking = "percent")) %>% 
        hc_xAxis(categories = df5$name) %>% 
        hc_add_series(name= " ", data =df5$y )

      
    }
    
    
    else if (input$test == "Migration" & input$thema == "Politisches Interesse") {
      
      df6 <- tibble(name = c("Very Interested","Moderately Interested","Not interested"),y = c(22,39,39), y1 = c(13,40,44), y2 = c(20,35,55))
      hc %>% 
        hc_title(text = "How young individuals are interested in politics by migration background.")%>%
        #hc_plotOptions(bar = list(stacking = "percent")) %>% 
        hc_xAxis(categories = df6$name) %>% 
        hc_add_series(name= "No Migration Background", data =df6$y )%>%
        hc_add_series(name= "Parents Immigrated",data =df6$y1 ) %>%
        hc_add_series(name= "Self Immigrated",data =df6$y2 )
      
      
    }
    else if (input$test == "Alter" & input$thema == "Politisches Interesse") {
      
      df6 <- tibble(name = c("Very Interested","Moderately Interested","Not interested"),y = c(15,37,49), y1 = c(21,38,41), y2 = c(22,39,40))
      hc %>% 
        hc_title(text = "How young individuals are interested in politics by age.")%>%
        #hc_plotOptions(bar = list(stacking = "percent")) %>% 
        hc_xAxis(categories = df6$name) %>% 
        hc_add_series(name= "16-20 y.o.", data =df6$y )%>%
        hc_add_series(name= "21-25 y.o.",data =df6$y1 ) %>%
        hc_add_series(name= "26-29 y.o.",data =df6$y2 )
      
      
    }
    else if (input$test == "Geschlecht" & input$thema == "Politisches Interesse") {
      
      df7 <- tibble(name = c("Very Interested","Moderately Interested","Not interested"),y = c(12,49,49), y1 = c(25,48,39))
      hc %>% 
        hc_title(text = "How young individuals are interested in politics by gender.")%>%
        #hc_plotOptions(bar = list(stacking = "percent")) %>% 
        hc_xAxis(categories = df7$name) %>% 
        hc_add_series(name= "Female", data =df7$y )%>%
        hc_add_series(name= "Male",data =df7$y1 )
      
      
    }
    else if (input$test == "Status" & input$thema == "Politisches Interesse") {
      
      df8 <- tibble(name = c("Very Interested","Moderately Interested","Not interested"),y = c(19,40,41), y1 = c(21,37,42), y2 = c(15,38,47))
      hc %>% 
        hc_title(text = "How young individuals are interested in politics by living status.")%>%
        #hc_plotOptions(bar = list(stacking = "percent")) %>% 
        hc_xAxis(categories = df8$name) %>% 
        hc_add_series(name= "Students", data =df8$y )%>%
        hc_add_series(name= "Employed",data =df8$y1 )%>%
        hc_add_series(name= "NEET",data =df8$y2 )
    }
    
    else if (input$test == "None" & input$thema == "Politische Aktion") {
      
      df9 <- tibble(name = c("Taking part in public discussion","Getting involved in citizens initiative","Getting involved in a political party","Taking part in unauthorised demonstration", "Taking part in authorised demonstration", "Taking part in a signature collection campaign", "Boycotting or purchasing goods for political reasons", "Taking part in an online protest", "Posting or sharing something about politics online"),y = c(24,23,23,24,31,33,37,32,33))
      hc %>% 
        hc_title(text = "Political actions done before.")%>%
        #hc_plotOptions(bar = list(stacking = "percent")) %>% 
        hc_xAxis(categories = df9$name) %>% 
        hc_add_series(name= "", data =df9$y )
    }
    
    else if (input$test == "Migration" & input$thema == "Politische Aktion") {
      
      df10 <- tibble(name = c("Taking part in public discussion","Getting involved in citizens initiative","Getting involved in a political party","Taking part in unauthorised demonstration", "Taking part in authorised demonstration", "Taking part in a signature collection campaign", "Boycotting or purchasing goods for political reasons", "Taking part in an online protest", "Posting or sharing something about politics online"),y = c(25,18,32,31,28,31,39,34,37), y1 = c(24,23,19,20,35,33,40,33,36), y2 = c(19,21,10,11,30,31,31,30,31)) #, y3 = c(24), y4 = c(33), y5 = c(37), y6 = c(33), y7 = c(32), y8 = c(33))
      hc %>% 
        hc_title(text = "Political actions done before by migration background.")%>%
        #hc_plotOptions(bar = list(stacking = "percent")) %>% 
        hc_xAxis(categories = df10$name) %>% 
        hc_add_series(name= "No migration background", data =df10$y )%>%
        hc_add_series(name= "Parents Immigrated", data =df10$y1 )%>%
        hc_add_series(name= "Self Immigrated", data =df10$y2 )
      
    }
    
    })

# observe -----------------------------------------------------------------

    
    observeEvent(input$mybutton, output$hcchart1 <- renderHighchart({
      
     # ClickFunction <- JS("function(event){Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
      ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.category);}")
      
      df3 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3))
      highchart() %>% 
        hc_chart(type = "sunburst")%>%
        #hc_plotOptions(bar = list(stacking = "percent")) %>% 
        hc_add_series(name= "Female",cursor= "pointer" ,data =tibble (id = c("0.0","1.3"), parent = c("", "0.0"), name = c("tt", "feof" )))        
      })
    )
    #map render observe event
    observeEvent(input$Clicked, 
      if (req(input$Clicked == "1")) {
        click("btn-1-front",F)
      output$hcchart2 <-  renderHighchart({
        
        hcmap(map= "countries/lu/lu-all", data =data.frame(name= c("Diekirch","Grevenmacher","Luxembourg"), value =as.vector(unlist(l2[[input$Clicked]][2,3:5]))), value = "value", joinBy = "name") %>%   #unlist oder flatten aus purrr
          hc_plotOptions(series = list(#column = list(stacking = "normal"), 
            borderWidth=0,
            dataLabels = list(style = list(fontSize = "14px"),enabled = TRUE),
            events = list(click = ClickFunction)))  %>%
           hc_credits(enabled = F) %>%
           hc_legend(enabled = F)
      
        })
      }else {NULL}
      
    )
    
    
    
    JS("setInterval(function(){ $('#reactiveButton').click(); }, 1000*4);")
    
 
   
    makeReactiveBinding("outputText")

    observeEvent(input$Clicked, {
      print(paste0(input$Clicked))
      outputText <<- paste0(input$Clicked)
    })
    observeEvent(input$switch, {
      switch <- switch(input$switch, "bar", "column")
      print(paste0(switch))
      print(paste0(input$switch))
      })

    output$text <- renderText({
      outputText
    })
    
    # Observe for third theme update
    observeEvent(input$thema, {
      if (input$thema == "Politische Aktion") {
      updateRadioGroupButtons(session,"test",size = "normal",choices = c("None","Migration"))
      } 
      else {
      updateRadioGroupButtons(session,"test",size = "normal",choices = c("None","Migration", "Alter", "Geschlecht","Status"))
        
      }
      
    })

} #/server function 
    
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

#comments
# conditonalPanel Funktion auf Server verschieben. Sinnvoller, um Ressourcen zu sparen.
