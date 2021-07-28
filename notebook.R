Age >=16 & Age < 20 ~ 1, #Änderung um es vergleichbar zu machen mit '19
Age >=20 & Age < 25  ~ 2,
Age >=25 & Age <= 29  ~  3)

df020
df0
Age_Cat = case_when(Age >=12 & Age < 16 ~ 1, #Änderung um es vergleichbar zu machen mit '19
                    Age >=16 & Age < 20  ~ 2,
                    Age >=20 & Age < 25  ~ 3,
                    Age >=25 & Age <= 29  ~  4),
df_gender20F, df_status20F, df_age20F, df020F
df_gender19,
df_status19, df_age19,df019

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


#  hover <- JS("function () {
#   var chart = this,
#   legend = chart.legend;
#   
#   for (var i = 0, len = legend.allItems.length; i < len; i++) {
#                     (function(i) {
#                         var item = legend.allItems[i].legendItem,;
#                         item.on('mouseover', function (e) {
#                             '//show custom tooltip here'
#                             console.log('mouseover' + i);
#                         }).on('mouseout', function (e) {
#                             //hide tooltip
#                             console.log('mouseout' + i);
#                         });
#                     })(i);
#   }
#                 
#             
# }")




#First Batch Analysis

library(pacman)
p_load(tidyverse, readxl, Amelia)
batch <- read_csv(file.choose(),)
batch <- read_excel(file.choose())

375 376 365
names(batch[375])

batch <-  batch %>% mutate(gender = case_when(M...375 == "M" ~ 1, 
                                              M...375 == "F" ~ 0))
map(list(batch$gender, batch$`16`), glimpse)

batch %>% ggplot(aes(x =`16`)) +
  geom_density(aes(color = M...375 , fill=M...375, alpha = 0.5) )+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  facet_wrap(~Troisvierges)
     
missmap(batch)
EDA::
  
var inner_text = document.getElementsByClassName("i18n")[0].innerText;


var inner_text = document.getElementsByClassName("i18n");
var parser = new DOMParser();
for(var i=0; i<inner_text.length; i++) {
  var i_t = inner_text[i].innerText;
  var p_t = parser.parseFromString(i_t, 'text/html');
  console.log(inner_text[i].innerHTML);
  console.log(i_t);
  console.log(p_t);
  document.getElementsByClassName("i18n")[i].innerHTML = i_t;
}


var decodeEntities = (function() {
  // this prevents any overhead from creating the object each time
  var element = document.createElement('div');
  
  function decodeHTMLEntities (str) {
    if(str && typeof str === 'string') {
      // strip script/html tags
      str = str.replace(/<script[^>]*>([\S\s]*?)<\/script>/gmi, '');
      str = str.replace(/<\/?\w(?:[^"'>]|"[^"]*"|'[^']*')*>/gmi, '');
      element.innerHTML = str;
      str = element.textContent;
      element.textContent = '';
    }

    return str;
  }

  return decodeHTMLEntities;
})();


for(var i=0; i<i18n_translations.length; i++){
  var obj = {a:"de",b:"fr",c:"en",d:"_row"};
  for(var	j in obj){
    console.log(decodeEntities(i18n_translations[i][obj[j]]));
    i18n_translations[i][obj[j]] = decodeEntities(i18n_translations[i][obj[j]]);
    
  }
}




