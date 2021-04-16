#https://rstudio-pubs-static.s3.amazonaws.com/304105_70f2ad540827454e934117e3d90f6c1a.html
# unbedingt reinschauen!

#
#library(geojsonio)
library(shiny)
#library(leaflet)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(highcharter)
library(shinyjs)
library(shinyhelper)
library(shinyBS)
library(dplyr)
#install.packages("shiny.i18n")
#install.packages("fusionchartsR")
#require(fusionchartsR)
#library(shinycustomloader)
#source("global.r")
source("flipBox.R")
source("map.r")
#library(ggvis) 
#library(plyr)
library(shiny.i18n) #dev version wegen google probs

i18n <- Translator$new(translation_json_path = "../Module/translation.json")
#i18n <- Translator$new(automatic = TRUE)
i18n$set_translation_language('en')

#library(shinyjs)
library(dplyr)
#library(data.table)
library(highcharter)
#write.csv(df,"df.csv")
options(shiny.reactlog = T)
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
script <- '
    Shiny.addCustomMessageHandler("jsCode", function(message) { 
        eval(message.value);
    });
    function hello() {
        console.log("hello from function hello!");
    };
'



# sprache -----------------------------------------------------------------

# UI
ui <- fluidPage(#theme = "bootstrap.css",
    useShinyjs(),
   
    tags$head(tags$style(HTML(".thema.hidden{visibility: hidden !important;}"))),
    tags$head(tags$script(' document.getElementById("Clicked").onclick = function() {
 Shiny.onInputChange("Clicked", NULL); }; ')),
    tags$head(tags$script('function printChart() { hcchart1.print() ;};')),
    tags$head(tags$style(HTML('* {font-family: "Helvetica" !important};'))), # * um jedes Element zu selektieren. !important um  optionen in den Kasaden zu überschreiben
    tags$head(tags$style(HTML(".shiny-input-container { font-size: 18px; }"))), #funzt
    tags$head(tags$style(HTML(".highcharts-input-container { font-size: 60px; }"))), #funzt nicht
    fluidRow(id ="first",shiny.i18n::usei18n(i18n),
             extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.Clicked', 'null'); }", functions = c()),
             
             column(12,          
                    flipBoxN(front_btn_text = "Meta-Information",
                             id = 1,
                             main_img = NULL,
                             header_img = NULL  ,
                             back_content  = tagList(column(12,highchartOutput("hcchart2"))) #"The target population of the Youth Survey Luxembourg is comprised of residents of Luxembourg who are 16–29 years old, regardless of their nationality or country of birth. Sampling frame and sources of information Data provided by the Institut National de la Statistique et des Etudes Economiques du Grand-Duché  de  Luxembourg  (STATEC)4  was  used  for  sampling  and  weighting calculations  for  the  Youth  Survey  Luxembourg."
                             ,
                             radioGroupButtons("thema",i18n$t("Theme"), choiceNames = c("2020","2019","Diff "),choiceValues = c("2020","2019","Diff "), size = "normal",direction = "horizontal"),
                             fluidRow(
                                 column(2,
                                        fluidRow(
                                            column(1),
                                            column(11,
                                                   br(),
                                                   radioGroupButtons("test",i18n$t("Sociodemographic"), choices = c("None", "Age", "Gender","Status"),size = "normal",direction = "vertical", selected = "None")
                                                   #,highchartOutput("hcchart2")
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
                                                style='float: right;width: 100px;',
                                                selectInput(
                                                    inputId='selected_language',
                                                    label=i18n$t('Change language'),
                                                    choices = c("English" = "en", "Deutsch" = "de", "Français" = "fr"),
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



server <- function(input, output,session) {

  get(load("data.RData",envir = .GlobalEnv))
  get(load("data19.RData",envir = .GlobalEnv))
    #source("global.r")
    
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
    # Set highcharter options
    #options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))
    ClickFunction <-  JS("function(event) {var rr = event.point.index; var rr = {rr, '.nonce': Math.random()};Shiny.onInputChange('Clicked',rr);}")
    #ClickFunction <-  JS("function(event) {Shiny.onInputChange('Clicked',event.point.index);}")
    colors <- c("#e41618","#52bde7","#4d4d52","#90b36d","#f5951f","#6f4b89","#3fb54e","#eea4d8")
    #source("module_global.R")
    # Tab2 Vis ----------------------------------------------------------------
    output$hcchart1 <- renderHighchart({
        #switch proxy für charttype
        
        dfn <- tibble(name = i18n$t(c("Alcohol","Tobacco","Cannabis")),y = asc )
        dfx <- tibble(name = i18n$t(c("Alcohol","Tobacco","Cannabis")),y = ma[1,], y1 = ma[2,],y2= ma[3,], n = c("9","8","1") )
        df3 <- tibble(name = i18n$t(c("Alcohol","Tobacco","Cannabis")),y = mg[1,], y1 = mg[2,])
        df2 <- tibble(name = i18n$t(c("Alcohol","Tobacco","Cannabis")),y = ms[1,], y1 = ms[2,],y2= ms[3,])
        
        #uebersetzung hier noetig, da das Dataframe format nicht wie vorher zerlegt ist
        df_gender <- df_gender %>% mutate(Var2 = i18n$t(as.character(Var2)))
        df_status <- df_status %>% mutate(Var2 = i18n$t(as.character(Var2)))
          

        df_l  <- lst(dfn,dfx)
        print(head(df_l))
        l2<-lapply(df_l, function(df) 
            cbind(df, b = df$y *1.1, c = df$y *1.2, d = df$y *0.7))
        if (input$switch == T)
        {switch <-"column"
        } else { switch <- "bar"
        }  
        if (input$thema == i18n_r()$t("2019") ){
          subtitle <- "Source: Youth Survey Luxembourg 2019, n = 2593"
        }else{
          
          subtitle <- "Source: Youth Survey Luxembourg 2020, n = 4189"
        }
        addPopover(session, "hcchart1", "Infos", content = paste0("weiterführende Infos"), trigger = "click")
        hc <-   highchart() %>%
            hc_xAxis(labels = list(style = list(fontSize = "16px"))) %>% 
            hc_yAxis(labels= list(format = "{value} %", style = list(fontSize = "16px"))) %>%
            hc_chart(type = switch)%>%
            hc_colors(colors) %>% 
            hc_title(style = list(fontSize = "18px")) %>%
            hc_subtitle(text = i18n$t(subtitle)) %>%
            hc_plotOptions(series = list(#column = list(stacking = "normal"), 
                borderWidth=0,
                dataLabels = list(style = list(fontSize = "14px"),enabled = TRUE),
                events = list(click = ClickFunction)))%>%
            hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>',pointFormatter= JS("function () { return  '<tr><td style = color:'+ this.color +';font-size:16px;padding:0;>'+  this.series.name + ':' +'</td>'+ '<td style =font-size:16px;padding:0;>' +'<b>' + this.y.toFixed(1) +'%' +'</td>' + '<td>'+ '<b/>' + ' \u00B1' + (Math.sqrt(((this.y/100)*(1-(this.y/100)))  /1000)*2*100).toFixed(1) + '%' + '</b>'+ '</td>'+'</tr>';  }"), shared= TRUE,footerFormat = '{series.n}{this.n}</table> ',useHTML =T) %>%
            #hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>', pointFormat = '<tr><td style="color:{series.color};font-size:16px;padding:0">{series.name}{point.n}: </td><td style="padding:0;font-size:16px;"><b>{point.y:.1f} % {point.y}</b></td></tr>', footerFormat = '{series.n}{this.n}</table> ', shared = T, useHTML =T) %>%
            hc_exporting(enabled = T, buttons = list(contextButton = list( symbol = "menu"  )), filename = "custom-file-name_Luxembourg_Data") 
        #hc_exporting(enabled = T, buttons = list(contextButton = list( symbol = "menu",text = "Download", menuItems = "null", onclick = JS("function () { this.renderer.label('efwfe',100,100).attr({fill:'#a4edba',r:5,padding: 10, zIndex: 10}) .css({ fontSize: '1.5em'}) .add();}") )), filename = "custom-file-name_Luxembourg_Data") 
        #switch <- switch(input$switch, TRUEE = "column", "FALSE" = "column", "column")
        
        if (input$test == i18n_r()$t("None") & input$thema == i18n_r()$t("2019")) {
          #dfn <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,26,91,90,89) )
          
          hc %>%
            hc_title(text = i18n$t("Percentage of answers “One or more days” according to substance consumption in the previous 30 days."))%>%
            hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>',pointFormatter= JS("function () { return  '<tr><td style = color:'+ this.color +';font-size:16px;padding:0;>'  +'</td>'+ '<td style =font-size:16px;padding:0;>' +'<b>' + this.y.toFixed(1) +'%' +'</td>' + '<td>'+ '<b/>' + ' \u00B1' + (Math.sqrt(((this.y/100)*(1-(this.y/100)))  /1000)*2*100).toFixed(1) + '%' + '</b>'+ '</td>'+'</tr>';  }"), shared= TRUE,footerFormat = '{series.n}{this.n}</table> ',useHTML =T) %>%
            
            hc_add_series(df0, "column",hcaes(x = Var1, y = Freq*100),showInLegend = FALSE,
                          tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
            #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
            #             centerInCategory = TRUE, groupPadding = .68,
            #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
            hc_xAxis(categories = df0$Var1, title = list(text = "Konsum"))
          
        }
        else if (input$test == i18n_r()$t("Age") & input$thema == i18n_r()$t("2019")) { #vorher MIgration
          
          
          hc %>%
            hc_title(text = i18n$t("Percentage of answers “One or more days” according to substance consumption in the previous 30 days by age."))%>%
            hc_add_series(df_age, "column",hcaes(x = Var1, y = Freq*100, group = Var2),
                          tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
            #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
            #             centerInCategory = TRUE, groupPadding = .68,
            #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
            hc_xAxis(categories = i18n$t(as.character(df_age$Var1[order(df_age$Var2)])), title = list(text = "Konsum"))
          
        }
        else if (input$test == i18n_r()$t("Gender") & input$thema == i18n_r()$t("2019")) {
          
          hc %>%
            hc_title(text = i18n$t("Percentage of answers “One or more days” according to substance consumption in the previous 30 days by gender."))%>%
            hc_add_series(df_gender, "column",hcaes(x = Var1, y = Freq*100, group = Var2),
                          tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
            #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
            #             centerInCategory = TRUE, groupPadding = .68,
            #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
            hc_xAxis(categories = i18n$t(as.character(df_gender$Var1[order(df_gender$Var2)])), title = list(text = "Konsum"))
          
        }
        
        else if (input$test == i18n_r()$t("Status") & input$thema == i18n_r()$t("2019")) {
          
          hc %>%
            hc_title(text = i18n$t("Percentage of answers “One or more days” according to substance consumption in the previous 30 days by ZZZstatus."))%>%
            hc_add_series(df_status, "column",hcaes(x = Var1, y = Freq*100, group = Var2),
                          tooltip = list(enabled = TRUE,pointFormat = '${point.y}')) %>%
            #hc_add_series(df, "errorbar", stemWidth = 1,  whiskerLength = 10, grouping = FALSE,
            #             centerInCategory = TRUE, groupPadding = .68,
            #            hcaes(x = ShareType, low = lower, high = upper, group = Gender)) %>%
            hc_xAxis(categories = i18n$t(as.character(df_status$Var1[order(df_status$Var2)])), title = list(text = "Konsum")) 
          
          
        }
        
        
        
        
        else if (input$test == i18n_r()$t("None") & input$thema == i18n_r()$t("2020")) {
            #dfn <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,26,91,90,89) )
            
            hc %>% 
            hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>',pointFormatter= JS("function () { return  '<tr><td style = color:'+ this.color +';font-size:16px;padding:0;>'  +'</td>'+ '<td style =font-size:16px;padding:0;>' +'<b>' + this.y.toFixed(1) +'%' +'</td>' + '<td>'+ '<b/>' + ' \u00B1' + (Math.sqrt(((this.y/100)*(1-(this.y/100)))  /1000)*2*100).toFixed(1) + '%' + '</b>'+ '</td>'+'</tr>';  }"), shared= TRUE,footerFormat = '{series.n}{this.n}</table> ',useHTML =T) %>%
            
                hc_title(text = i18n$t("Percentage of answers “One or more days” according to substance consumption in the previous 30 days."))%>%
                hc_xAxis(categories = dfn$name ,additonialInfo = 1:4 ) %>% 
                hc_add_series(name= " ",data =l2$dfn[c("name","y")] ,showInLegend = F)
            
        }
        
        else if (input$test == i18n_r()$t("Status") & input$thema == i18n_r()$t("2020")) { #vorher MIgration
            
            
            hc %>% 
                hc_title(text = i18n$t("Percentage of answers “One or more days” according to substance consumption in the previous 30 days by status"))%>%
                hc_xAxis(categories = df2$name) %>% 
                hc_add_series(name= i18n$t("Student"), data =df2[c("name","y")] )%>% # unnecessary "name?
                hc_add_series(name= i18n$t("Employed"),data =df2$y1 ) %>%
                hc_add_series(name= i18n$t("NEET"), data =df2$y2) 
        }
        else if (input$test == i18n_r()$t("Age") & input$thema == i18n_r()$t("2020")) {
            
            hc %>%
                hc_title(text = i18n$t("Percentage of answers “One or more days” according to substance consumption in the previous 30 days by age."))%>%
                hc_plotOptions(bar = list(stacking = "percent")) %>%
                hc_xAxis(categories = dfx$name) %>%
                hc_add_series(name= i18n$t("12-16"), data =dfx[c("n","y")]) %>%
                hc_add_series(name= i18n$t("17-23"),data = dfx$y1 ) %>%
                hc_add_series(name= i18n$t("24-29"), data =dfx$y2)#%>%
                #hc_add_series(type= "errorbar",linkedTo = i18n$t("12-16"), data= list(c(20,60),c(20,30),c(20,40)))
               # hc_add_series(name= i18n$t("24-29"),type= "errorbar", data= map(ma[3,],.f = function(x) x+ c(-1.96,1.96)*sqrt((x/100*(1-x/100)/1000))*100))
            
          
        }
        
        else if (input$test == i18n_r()$t("Gender") & input$thema == i18n_r()$t("2020")) {
            
            hc %>% 
                hc_title(text = "Percentage of answers “One or more days” according to substance consumption in the previous 30 days by gender.")%>%
                
                #hc_plotOptions(bar = list(stacking = "percent")) %>% 
                hc_xAxis(categories = df3$name) %>% 
                hc_add_series(name= i18n$t("Female"), data =df3$y )%>%
                hc_add_series(name= i18n$t("Male"),data =df3$y1 )
        }

       
        
    })
    
    # observe -----------------------------------------------------------------
    #ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.category);}") # sollte man global regeln
    
    observeEvent(input$mybutton, output$hcchart1 <- renderHighchart({ #experimental
        
        # ClickFunction <- JS("function(event){Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
        
        df3 <- tibble(name = c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux."),y = c(5,3,4,3,2), y1 = c (4,4,2,4,3))
        highchart() %>% 
            hc_chart(type = "sunburst")%>%
            #hc_plotOptions(bar = list(stacking = "percent")) %>% 
            hc_add_series(name= "Female",cursor= "pointer" ,data =tibble (id = c("0.0","1.3"), parent = c("", "0.0"), name = c("tt", "feof" )))        
    })
    )
    unNonce <- function(f) {
        x <-  as.integer(input[[f]][1])  #auf doppel [[]] achten, weil single object??? # ist eine Liste; deshalb as.integer
        print(x)
        print("^")
        return(x)
    } 
    
    
    #map render observe event
    worldgeojson<-  convertMap("https://code.highcharts.com/mapdata/countries/lu/lu-all.js")
    observeEvent(input$Clicked, 
                 if (req(unNonce("Clicked") == "1" | unNonce("Clicked") == "2")) {
                     Clicked <- unNonce("Clicked")
                     click("btn-1-front",F)
                     delay(500,
                           output$hcchart2 <-  renderHighchart({
                               print(typeof(Clicked))
                               dfn <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,26,91,90,89) )
                               dfx <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,35,90,82,82), y1 = c (51,24,82,81,81),y2= c(37,36,76,80,82) )
                               df_l  <- lst(dfn,dfx)
                               print(head(df_l))
                               l2<-lapply(df_l, function(df) 
                                   cbind(df, b = df$y *1.1, c = df$y *1.2, d = df$y *0.7))
                               highchart(type = "map") %>% 
                                   
                                   hc_add_series_map(map =worldgeojson, df= data.frame(name= c("Diekirch","Grevenmacher","Luxembourg"),  value =as.vector(unlist(l2[[Clicked]][2,3:5]))), value = "value", joinBy = "name", name = "test") %>%
                                   
                                   #hcmap(map= "countries/lu/lu-all", data =data.frame(name= c("Diekirch","Grevenmacher","Luxembourg"), value =as.vector(unlist(l2[[input$Clicked]][2,3:5]))), value = "value", joinBy = "name") %>%   #unlist oder flatten aus purrr
                                   hc_plotOptions(series = list(#column = list(stacking = "normal"), 
                                       borderWidth=0,
                                       dataLabels = list(style = list(fontSize = "14px"),enabled = TRUE),
                                       events = list(click = ClickFunction)))  %>%
                                   hc_credits(enabled = F) %>%
                                   hc_title(text = list(l2[[Clicked]][2,1])) %>%
                                   hc_legend(enabled = T)
                               
                           })
                     )
                     
                     #session$sendCustomMessage('Clicked', "Shiny.setInputValue('Clicked', '0');")
                     print(paste("jidw",input$Clicked))
                     print(paste("-->", unNonce("Clicked"),"<<-"))
                     print(paste("m",input$Clicked[1]))}
    )
    
    
    # custom session message for rotation ---------------------------------------------
    
    
    delay(10000, print(paste0(input$Clicked)))
    fxn <- "click"
    fxn <- paste0("shinyjs-", fxn)
    params <- list(id = "btn-1-front", asis = TRUE)
    
    params[["id"]] <- session$ns(params[["id"]])
    # session$sendCustomMessage(type = fxn , message = params) # Works quite well!
    
    #register handler for back button to null hc? from js to r?
    
    
    #observe fuer  back button
    # observe(input$btn-1-front, print("fj"))
    
    
    # highchart(type = "map") %>% 
    #   hc_plotOptions(map = list(mapData = worldgeojson)) %>%
    #   hc_add_series( data= data.frame(name= c("Diekirch","Grevenmacher","Luxembourg"),  value =as.vector(unlist(l2[[1]][2,3:5]))), value = "value", joinBy = "name", name = "test") %>%
    # hc_add_series_map(map =worldgeojson, df= data.frame(name= c("Diekirch","Grevenmacher","Luxembourg"),  value =as.vector(unlist(l2[[1]][2,3:5]))), value = "value", joinBy = "name", name = "test")
    #   
    
    JS("setInterval(function(){ $('#reactiveButton').click(); }, 1000*4);")  #muss in script eingebunden werden
    
    
    makeReactiveBinding("outputText")  #unnoetig
    
    observeEvent(input$Clicked, {  #monitor um eingabe in console zu prüfen
        print(paste0(input$Clicked))
        print(paste0(input$event.point.index, "fj"))
        outputText <<- paste0(input$Clicked)
    })
    observeEvent(input$switch, {   #gehört zu hcchaarts 
        switch <- switch(input$switch, "bar", "column")
        print(paste0(switch))
        print(paste0(input$switch))
    })
    
    output$text <- renderText({  #unnötig
        outputText
    })
   
    
    # Observe for third topic update of inputselections
    observeEvent(input$thema, {
        if (input$thema == i18n_r()$t("Diff ")) {
            updateRadioGroupButtons(session,"test",label = i18n_r()$t("Sociodemographic"),size = "normal",choices = i18n_r()$t(c("None","Migration")))
        } 
        else {
            updateRadioGroupButtons(session,"test",size = "normal",choices = i18n_r()$t(c("None", "Age", "Gender","Status")))
        }
        
        
    })
    # rename #github examp.
    i18n_r <- reactive({
        i18n
    })
    observe({  #reactive update for labels
      #Achtung. translate rbaucht einer übersetzung in den radiobuttons, ansonsten spinnt der abru der hcs.
        updateRadioGroupButtons(session,"thema",label = i18n_r()$t("Theme"),size = "normal",choices = i18n_r()$t(c("2020","2019","Diff ")))
    })
    
    
    # sprache obs -------------------------------------------------------------
    
    observeEvent(input$selected_language,ignoreInit = T, {
        # This print is just for demonstration
        print(paste("Language change!", input$selected_language))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selected_language)
    })
    
    
    
    
    
    #reactivce translations for ui buttons
    # i18n_r <- reactive({
    #   i18n
    # })
    # 
    # 
    # observe({
    #   updateRadioGroupButtons(session, "thema", label = i18n_r()$t("Thema"),
    #                           choiceNames = i18n_r()(c("Identitaet","Politisches Interesse","Politische Aktion")) )
    #   
    # })
    
} 

shinyApp(ui = ui, server = server)

# conditonalPanel Funktion auf Server verschieben. Sinnvoller, um Ressourcen zu sparen.
