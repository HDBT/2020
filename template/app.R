#https://rstudio-pubs-static.s3.amazonaws.com/304105_70f2ad540827454e934117e3d90f6c1a.html
# unbedingt reinschauen!

#
#library(geojsonio)
library(shiny.i18n) #dev version wegen google probs
i18n <- shiny.i18n::Translator$new(translation_json_path = "translation.json")
i18n$set_translation_language('de')
library(shiny)
#i18n <- Translator$new(translation_json_path = "translation.json")
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
#install.packages("shiny.i18n")

#i18n <- Translator$new(automatic = TRUE)

#library(shinyjs)

#library(data.table)

#write.csv(df,"df.csv")
options(shiny.reactlog = T)
#options(shiny.error = browser)
#install.packages("shinycustomloader")
# For dropdown menu #useless?
# load("data.RData",envir = .GlobalEnv)
# load("data19.RData",envir = .GlobalEnv)
# load("data19_Diff.RData",envir = .GlobalEnv)
# load("data20_Diff.RData",envir = .GlobalEnv)
# load("data20_full.RData",envir = .GlobalEnv)

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
i18n_html <- JS("var inner_text = document.getElementsByClassName('i18n');
                      var parser = new DOMParser();
                      for(var i=0; i<inner_text.length; i++) {
                        var i_t = inner_text[i].innerText;
                        var p_t = parser.parseFromString(i_t, 'text/html');
                        console.log(inner_text[i].innerHTML);
                        console.log(i_t);
                        console.log(p_t); 
                        document.getElementsByClassName('i18n')[i].innerHTML = i_t;
                      }")


css_style <- tagList(  
                
                tags$style(".fa-bars, .fa-chart-bar  { color: #666666 !important; border: 0.9px solid #cccccc !important;padding: 6px 12px;
                                                      display: inline-block!important;
                                                      width: 100%!important;
                                                      overflow: hidden!important;
                                                      position: relative!important;
                                                      z-index: 1!important;
                                                      -webkit-box-sizing: border-box!important;
                                                      -moz-box-sizing: border-box!important;
                                                      box-sizing: border-box!important;
                                                      -webkit-box-shadow: none!important;
                                                      box-shadow: none!important;
                                                      -webkit-border-radius: 4px!important;
                                                      -moz-border-radius: 4px!important;
                                                      border-radius: 4px!important;
                                                      transform: scale(1.2) !important;
                           }"), #funz  !!! <- nicht durch ; trennen 

                tags$style(HTML(".state {font-size: 28px !important}")), #funz
                tags$style(HTML("i { display: inline-block;
                  color: white;
                  border-radius: 4px;
                  padding: 0.3em; /* adjust padding */
                  line-height: initial !important; /* reset line-height */
                  height: 1em;
                  width: 1em;
                  text-align:center;
                  }")), #funz nicht
                #tags$style("#shadow_row_1:focus {box-shadow: inset 0 0 0 2em var(--hover);}"), #funz nicht
                tags$head(tags$script(' document.getElementById("Clicked").onclick = function() { Shiny.onInputChange("Clicked", NULL); }; ')), #?
                tags$script('     $(document).on("keypress", function (e) { Shiny.onInputChange("mydata", e.which);     });   '),
                tags$head(tags$script(HTML('for(var i=0; i<i18n_translations.length; i++){  var obj = {a:"de",b:"fr",c:"en",d:"_row"}; for(var	j in obj){  console.log(decodeEntities(i18n_translations[i][obj[j]]));i18n_translations[i][obj[j]] = decodeEntities(i18n_translations[i][obj[j]]);}}'))),
                tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css.css")),
                tags$head(tags$style(HTML('* {font-family: "helvetica" };'))), # * um jedes Element zu selektieren. !important um  optionen in den Kasaden zu überschreiben
                tags$head(tags$style(HTML(".shiny-input-container { font-size: 18px; }"))), #funzt
                tags$head(tags$style(HTML(".selected {color:#16A085;}"))), #funzt nicht
                tags$head(tags$style(HTML(".tooltip-inner {  text-align: center;    font-size: 14px;};"))), #funzt 
                
                tags$head(tags$style(HTML(".highcharts-input-container { font-size: 60px; }"))) #funzt nicht
                ,tags$body( onload = HTML(i18n_html)) #just to initiate custom js once, and then language triggered
)

  
back_content <- tagList(column(12,tags$body(HTML(i18n$t('<h4 style="text-align: center;"><a href="https://www.jugend-in-luxemburg.lu/youth-survey/">Youth Survey Luxembourg</a></h4>
<p style="text-align: justify;">The Youth Survey Luxembourg is a representative, large-scale survey of Luxembourg residents.</p>
<p style="text-align: justify;">The target population of the Youth Survey Luxembourg 2019 is comprised of residents of Luxembourg who are 16&ndash;29 years old, regardless of their nationality or country of birth. Sampling frame and sources of information Data provided by the Institut National de la Statistique et des Etudes Economiques du Grand-Duch&eacute; de Luxembourg (STATEC) was used for sampling and weighting calculations for the Youth Survey Luxembourg.</p>
<h4 class="LC20lb DKV0Md" style="text-align: center;"><a href="https://www.jugend-in-luxemburg.lu/yac-plus/"> Young People and COVID-19 (YAC+)</a></h4>
<p style="text-align: justify;">To assess the situation during and after the pandemic, two surveys will be conducted in 2020 and 2021 based on the Youth Survey Luxembourg 2019 and in close collaboration with the research group of the Child and Adolescent Health Study "<a href="https://www.jugend-in-luxemburg.lu/hbsc-kooperation/">Health Behavior in School-Aged Children</a>".</p>
<div class="elementor-element elementor-element-289e4d2 elementor-widget elementor-widget-text-editor" data-id="289e4d2" data-element_type="widget" data-widget_type="text-editor.default">
<div class="elementor-widget-container">
<div class="elementor-text-editor elementor-clearfix">
<p style="text-align: justify;">For YAC+, this group will be supplemented with children and adolescents aged 12 to 16. Thus, the age group of 12 to 29 years old can be surveyed.</p>
<p style="text-align: justify;">These standardized surveys will be supplemented by qualitative interviews to gain a deeper understanding of the situation and subjective evaluations of adolescents and young adults.</p>
</div>
</div>
</div>'))))) #"The target population of the Youth Survey Luxembourg is comprised of residents of Luxembourg who are 16–29 years old, regardless of their nationality or country of birth. Sampling frame and sources of information Data provided by the Institut National de la Statistique et des Etudes Economiques du Grand-Duché  de  Luxembourg  (STATEC)4  was  used  for  sampling  and  weighting calculations  for  the  Youth  Survey  Luxembourg."


Download <- JS("
                  Highcharts.SVGRenderer.prototype.symbols.download = function (x, y, w, h) {
    var path = [
        // Arrow stem
        'M', x + w * 0.5, y,
        'L', x + w * 0.5, y + h * 0.7,
        // Arrow head
        'M', x + w * 0.3, y + h * 0.5,
        'L', x + w * 0.5, y + h * 0.7,
        'L', x + w * 0.7, y + h * 0.5,
        // Box
        'M', x, y + h * 0.9,
        'L', x, y + h,
        'L', x + w, y + h,
        'L', x + w, y + h * 0.9
    ];
    return path;
};   ")




# UI--------------------------
ui <- fluidPage(#theme = "bootstrap.css",
  shiny.i18n::usei18n(i18n),#notwendig für  rendering on UI side
  singleton(tags$head(tags$script(src = "pop_patch.js"))),
  singleton(tags$head(tags$script(src = "decode_entities.js"))),
  
    useShinyjs(),
    css_style,
    bsTooltip("switch", i18n$t("Switch between horizontal and vertical orientation"),"left", options = list(container = "body")), #notwendig, um BS serveritig laufen zu lassen. Noch redundant, was machen?
  
    fluidRow(id ="first",
             #extendShinyjs(text = "shinyjs.resetClick = function() { Shiny.onInputChange('.Clicked', 'null'); }", functions = c()), #why?
             column(12,      
        
                    flipBoxN(front_btn_text = NULL, #created custom on next lines
                             id = 1,
                             main_img = NULL,
                             header_img = NULL  ,
                             back_content  = tagList(column(12,tags$body(i18n$t("<h4 style='text-align: center;'><a href='https://www.jugend-in-luxemburg.lu/youth-survey/'>Youth Survey Luxembourg</a></h4> <p style='text-align: justify;'>The Youth Survey Luxembourg is a representative, large-scale survey of Luxembourg residents.</p> <p style='text-align: justify;'>The target population of the Youth Survey Luxembourg 2019 is comprised of residents of Luxembourg who are 16&ndash;29 years old, regardless of their nationality or country of birth. Sampling frame and sources of information Data provided by the Institut National de la Statistique et des Etudes Economiques du Grand-Duch&eacute; de Luxembourg (STATEC) was used for sampling and weighting calculations for the Youth Survey Luxembourg.</p> <h4 class='LC20lb DKV0Md' style='text-align: center;'><a href='https://www.jugend-in-luxemburg.lu/yac-plus/'> Young People and COVID-19 (YAC+)</a></h4> <p style='text-align: justify;'>To assess the situation during and after the pandemic, two surveys will be conducted in 2020 and 2021 based on the Youth Survey Luxembourg 2019 and in close collaboration with the research group of the Child and Adolescent Health Study '<a href='https://www.jugend-in-luxemburg.lu/hbsc-kooperation/'>Health Behavior in School-Aged Children</a>'.</p> <div class='elementor-element elementor-element-289e4d2 elementor-widget elementor-widget-text-editor' data-id='289e4d2' data-element_type='widget' data-widget_type='text-editor.default'> <div class='elementor-widget-container'> <div class='elementor-text-editor elementor-clearfix'> <p style='text-align: justify;'>For YAC+, this group will be supplemented with children and adolescents aged 12 to 16. Thus, the age group of 12 to 29 years old can be surveyed.</p> <p style='text-align: justify;'>These standardized surveys will be supplemented by qualitative interviews to gain a deeper understanding of the situation and subjective evaluations of adolescents and young adults.</p> </div> </div> </div>")))) 
                             ,
                             radioGroupButtons("thema",i18n$t('Year'), choiceNames = c("2019","2020","Differences"),choiceValues = c("2019","2020","Differences"), size = "normal",direction = "horizontal"),
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
                                 
                                 #setShadow(id = "shadow_row_0"), #switch
                                 #setShadow(id = "shadow_row_1"), #switch
                                 column(10,
                                        div(highchartOutput("hcchart1"), style = "font-size:15%"),
                                        #actionButton("mybutton", "action"),
                                          tags$style(HTML("#lang_div .shiny-input-container  {font-size: 16px;}")),  #individuelles style setzen indem man eine eigens erstellte id anspricht
                                          div(id ="lang_div", #prettySwitch(inputId = "switch","Spaltendiagramm",slim = T, value = TRUE),#div() um eigene ID zu setzen fürs ansprechen (individuelle style tags z.b.)
                                              tags$span(shiny::tags$button(id = paste0("btn-", 1, "-front"), class = "btn btn-primary btn-rotate navitem",style = "float: center;",shiny::tags$i(class = "fa fa-long-arrow-right"), i18n$t("Data basis and methodology"))),
                                              tags$span(style='float: left; width:20%; bottom: 200;',prettyToggle(plain = TRUE,inputId = "switch",shape = "curve", label_on = "", label_off = "",inline = FALSE,value = TRUE, bigger = FALSE,fill = FALSE,outline= TRUE,icon_off = icon("bars", class = "c_border"),icon_on = icon("chart-bar", class =  "c_border"))),
                                              
                                              
                                              
                                              # 
                                              # radioGroupButtons(
                                              #   inputId = "Id069",
                                              #   label = "Choose a graph :", 
                                              #   choices = c(`<i class='fa fa-bar-chart'></i>` = "bar", `<i class='fa fa-line-chart'></i>` = "line", 
                                              #               `<i class='fa fa-pie-chart'></i>` = "pie"),
                                              #   justified = TRUE
                                              # ),
                                              # 
                                              # 
                                              
                                              tags$span(style='float: right; bottom: 200;',
                                                        selectInput(width = 105,
                                                inputId='selected_language',
                                                label= NULL,
                                                choices = c("English" = "en", "Deutsch" = "de", "Français" = "fr"),
                                                selected = "de"
                                              ) )
                                              #,
                                        #       tags$span(  
                                        #           style='float: right;width: 100px;',                                                  
                                        # 
                                        # dropdown(style = "unite",icon = icon("gear"), inline = TRUE, right = TRUE,
                                        #          status = "danger", 
                                        #          tooltip = tooltipOptions(placement = "left",title= "Options"),
                                        #          tags$div(  
                                        #            
                                        #            style='float: right;width: 100px;',
                                        #           tags$hr(style="border-color: black;"),
                                        #           selectInput(
                                        #               inputId='selected_language',
                                        #               label=i18n$t('Change language'),
                                        #               choices = c("English" = "en", "Deutsch" = "de", "Français" = "fr"),
                                        #               selected = i18n$get_key_translation()
                                        #           )
                                        #           )
                                        #       )                                        
                                        # 
                                        #   )
                                        ),
                                        
                                 ),      
                                 
                             ),
                    )       
             )     
    )
) 

# Server ------------------
server <- function(input, output,session) {
  
  i18n_r <- reactive({
    i18n
  })
     
  ## use JS to add an id attribute to the elements where you want to add the popover ----
  add_id_js <- paste0(
    "$('#test').find('.btn-group-vertical > .btn-group-toggle').attr('id', function(i) {",
    "return 'test_row_' + i})
    
     $('#lang_div').find('.form-group > .pretty > .state ').attr('id', function(i) {",
    "return 'shadow_row_' + i})
    
    
     $('#thema').find('.btn-group > .btn-group-toggle').attr('id', function(i) {",
    "return 'thema_row_' + i})

document.querySelectorAll('button.action').forEach(button =>
    button.addEventListener('click', e =>
        chart.exportChart({
            scale: 5
        })
    )
);
    ")
  
  # add_id_js <- paste0(
  #   "$('#hcchart1').find('.highcharts-container > .highcharts-root > .highcharts-subtitle').attr('id', function(i) {",
  #   "return 'test_row_' + i})")


  ## once the UI is loaded, call JS function and attach popover to it. For dependency loading one call has to come from UI. e.g. bstootltips ------------
 
  session$onFlushed(function() {
    runjs(add_id_js)
  }, once = FALSE)
    
  
  language  <- reactive({input$selected_language})
        
  observeEvent(language() ,{
    
    delay(600, #haudruff loesung
          {
      addPopover(session,"thema_row_2",NULL,i18n_r()$t("<p style='text-align:justify'><strong>Differences </strong>&ndash; Only persons over 16 considered.</p>"),"right", options = list(delay=list(show= 500, hide = 100), html = "true",container = "body"),trigger = "hover")
      addPopover(session,"thema_row_1",NULL,i18n_r()$t("<p style='text-align: justify;'><strong>2020 </strong>&ndash; YAC+</p>"),"right", options = list(delay=list(show= 500, hide = 100), html = "true",container = "body"))
      addPopover(session,"test_row_1",NULL,i18n_r()$t("<p style='text-align: justify;'><strong>Age</strong>&nbsp;- While the Youth Survey Luxembourg 2019 has asked 16-29 year old people residing in Luxembourg, the YAC+ survey 2020, which is an additional survey based on the Youth Survey Luxembourg, has surveyed 12-29-year olds.</p>"),"right", options = list(delay=list(show= 500, hide = 100), html = "true",container = "body")) #quotesign trick, cuz fked up package
      addPopover(session,"test_row_2",NULL,i18n_r()$t("<p style='text-align: justify;'><strong>Gender </strong>&ndash; The Youth Survey Luxembourg offers their respondents the possibility to define their gender apart from the binary CIS-categories of male and female. However, the number of answers was too small to be able to conduct statistically sound analyses</p>"),"right", options = list(delay=list(show= 500, hide = 100), html = "true",container = "body"))
      addPopover(session,"test_row_3",NULL,i18n_r()$t("<p style='text-align: justify;'><strong>Status</strong>&ndash; NEET is the acronym for &lsquo;not in education, employment or training&rsquo;. This entails every respondent of the Youth Survey who solely answered to be either unemployed and looking for work (unemployed) or unemployed and not looking work (economically inactive). Meaning that every respondent who, at the time of the survey, declared that they are either in education, employment or training and who were a pupil, apprentice or student, were excluded from the NEET category.</p>"),"right", options = list(delay=list(show= 500, hide = 100), html = "true",container = "body"))
      addPopover(session,"thema_row_0",NULL,i18n_r()$t("<p style='text-align: justify;'><strong>2019 </strong>&ndash; Youth Survey Luxembourg</p>"),"right", options = list(delay=list(show= 500, hide = 100), html = "true",container = "body"))
      addTooltip(session,id = "switch",title = i18n_r()$t("Switch between horizontal and vertical orientation"),"left", options = list(container = "body"))
          
    }
  )
  }
    
  )  
    
  
    
  # export -------
  export <- function() {
    
  
      export <- list(
        list(text=i18n_r()$t("Download PNG image"),
             onclick=JS("function () { 
                      this.exportChart({ type: 'image/png' }); }")),
        list(text=i18n_r()$t("Download JPEG image"),
             onclick=JS("function () { 
                      this.exportChart({ type: 'image/jpeg' }); }")),
        list(text=i18n_r()$t("Download SVG vector image"),
             onclick=JS("function () { 
                      this.exportChart({ type: 'image/svg+xml' }); }")),
        list(text=i18n_r()$t("Download PDF document"),
             onclick=JS("function () { 
                      this.exportChart({ type: 'application/pdf' }); }")),
        list(separator=TRUE),
        list(text=i18n_r()$t("Download CSV file"),
             onclick=JS("function () { this.downloadCSV(); }")),
        list(text=i18n_r()$t("Download XLS file"),
             onclick=JS("function () { this.downloadXLS(); }"))
        
      )
  }
 
     # title tooltips render in HS
     # hover <- JS("function() {
     # var chart = this
     #    this.title.on('mouseover', e => {
     #      chart.myLabel = this.renderer.label('this.title.textStr', e.x, e.y, 'rectangle')
     #        .css({
     #          color: '#FFFFFF'
     #        })
     #        .attr({
     #          fill: 'rgba(0, 0, 0, 0.75)',
     #          padding: 8,
     #          r: 4,
     #          
     #      })
     #        .add()
     #        .toFront();
     #    })
     #    
     #    this.title.on('mouseout', e => {
     #      if(chart.myLabel){
     #      	chart.myLabel.destroy();
     #      }
     #    })
     # 
     #  }")
     # 

    runjs(Download)
    
    ClickFunction <-  JS("function(event) {var rr = event.point.index; var rr = {rr, '.nonce': Math.random()};Shiny.onInputChange('Clicked',rr);}")
    #ClickFunction <-  JS("function(event) {Shiny.onInputChange('Clicked',event.point.index);}")
    colors <- c("#e41618","#52bde7","#4d4d52","#90b36d","#f5951f","#6f4b89","#3fb54e","#eea4d8")
    #source("module_global.R")
    
    # Tab2 Vis ----------------------------------------------------------------
    output$hcchart1 <- renderHighchart({
      
      #switch proxy für charttype
      if (input$switch == T)
      {switch <-"column"
      } else { switch <- "bar"
      }  
      if (input$thema == i18n_r()$t("2019") ){
        subtitle <- "Source: Youth Survey Luxembourg 2019, n = 2593 (16 - 29 year olds)"
      } 
      else if (input$thema == i18n_r()$t("Differences")) {
        subtitle <- "Source: Youth Survey Luxembourg 2019, n = 2593 & Young People and COVID-19 2020, n = 3562 (16 - 29 year olds)"
        
      }
      else{
        
        subtitle <- "Source: Young People and COVID-19 2020, n = 4189 (12 - 29 year olds)"
      }
    
       # only relevant for old method
        # dfn <- tibble(name = i18n$t(c("Alcohol","Tobacco","Cannabis")),y = asc )
        # dfx <- tibble(name = i18n$t(c("Alcohol","Tobacco","Cannabis")),y = ma[1,], y1 = ma[2,],y2= ma[3,],y3= ma[4,], n = c("9","8","1") )
        # df3 <- tibble(name = i18n$t(c("Alcohol","Tobacco","Cannabis")),y = mg[1,], y1 = mg[2,])
        # df2 <- tibble(name = i18n$t(c("Alcohol","Tobacco","Cannabis")),y = ms[1,], y1 = ms[2,],y2= ms[3,])
        # 
        # df_l  <- lst(dfn,dfx) 
        # print(head(df_l))
        # l2<-lapply(df_l, function(df) 
        #     cbind(df, b = df$y *1.1, c = df$y *1.2, d = df$y *0.7))
        
        #uebersetzung hier noetig, da das Dataframe format nicht wie vorher zerlegt ist
        df_gender <- df_gender %>% mutate(Var2 = i18n$t(as.character(Var2)))
        df_status <- df_status %>% mutate(Var2 = i18n$t(as.character(Var2)))
          

        
        hc <-   highchart() %>%   #hc_opts = list(lang = list(contextButtonTitle = "Chart Download"))
            hc_xAxis(labels = list(style = list(fontSize = "16px"))) %>% 
            hc_yAxis(labels= list(format = "{value} %", style = list(fontSize = "16px"))) %>%
            hc_chart(type = switch) %>% #, events = list(load = hover))%>%
            hc_colors(colors) %>% 
            hc_title(style = list(fontSize = "18px")) %>%
            hc_subtitle(text = i18n$t(subtitle)) %>%
          hc_credits(enabled = TRUE, href= "www.jugend-in-luxemburg.lu", text = "jugend-in-luxemburg.lu") %>%
            hc_plotOptions(series = list(#column = list(stacking = "normal"), 
                borderWidth=0,
                dataLabels = list(style = list(fontSize = "14px"),enabled = TRUE),
                events = list(click = ClickFunction)))%>%
           # hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>',pointFormatter= JS("function () { return  '<tr><td style = color:'+ this.color +';font-size:16px;padding:0;>'+  this.series.name + ':' +'</td>'+ '<td style =font-size:16px;padding:0;>' +'<b>' + this.y.toFixed(1) +'%' +'</td>' + '<td>'+ '<b/>' + ' \u00B1' + (Math.sqrt(((this.y/100)*(1-(this.y/100)))  /1000)*2*100).toFixed(1) + '%' + '</b>'+ '</td>'+'</tr>';  }"), shared= TRUE,footerFormat = '{series.n}{this.n}</table> ',useHTML =T) %>%
             hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>',pointFormatter= JS("function () { return  '<tr><td style = color:'+ this.color +';font-size:16px;padding:0;>'+  this.series.name + ':' +'</td>'+ '<td style =font-size:16px;padding:0;>' +'<b>' + this.y.toFixed(1) +'%' +'</td>' + '<td>'+ '<b/>' + ' \u00B1' + (this.se *1.96*100).toFixed(1)                                          + '%' + '</b>'+ '</td>'+'</tr>';  }"), shared= TRUE,footerFormat = '</table> ',useHTML =T) %>%
          #hc_tooltip(headerFormat = '<span style="font-size:16px"><b>{point.key}{point.n}</b></span><table>', pointFormat = '<tr><td style="color:{series.color};font-size:16px;padding:0">{series.name}{point.n}: </td><td style="padding:0;font-size:16px;"><b>{point.y:.1f} % {point.y}</b></td></tr>', footerFormat = '{series.n}{this.n}</table> ', shared = T, useHTML =T) %>%
            #hc_exporting(enabled = T, buttons = list(contextButton = list( symbol = "menu"  )), filename = "custom-file-name_Luxembourg_Data") 
        hc_exporting(enabled = TRUE, filename="Drug_Consumption_2019_2020", formAttributes=list(target="_blank"),buttons=list(contextButton=list(symbol = "download",symbolStrokeWidth =4, symbolX =17 , symbolY = 16 ,height= 30, width = 36,symbolSize= 25,text="", theme=list(fill="transparent"),menuItems=export())) )
        #hc_exporting(enabled = T, buttons = list(contextButton = list( symbol = "menu",text = "Download", menuItems = "null", onclick = JS("function () { this.renderer.label('efwfe',100,100).attr({fill:'#a4edba',r:5,padding: 10, zIndex: 10}) .css({ fontSize: '1.5em'}) .add();}") )), filename = "custom-file-name_Luxembourg_Data") 
        #switch <- switch(input$switch, TRUEE = "column", "FALSE" = "column", "column")
        
        source("highchart_server.R", local = T)
        highchart_server()
        
        
    })
    

      
    
    # observe -----------------------------------------------------------------
    #ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.category);}") # sollte man global regeln
    
    
    
    
    
    delay(10000, print(paste0(input$Clicked))) #monitor
    fxn <- "click"
    fxn <- paste0("shinyjs-", fxn)
    params <- list(id = "btn-1-front", asis = TRUE)
    
    params[["id"]] <- session$ns(params[["id"]])
    # session$sendCustomMessage(type = fxn , message = params) # Works quite well!
    
    #register handler for back button to null hc? from js to r?
    
    
    #observe fuer  back button
    # observe(input$btn-1-front, print("fj"))
    
    

    
    
    
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
   
    
    #sorgt fuer die hakeligen resets bei Theme aenderung.
    # Observe for third topic update of inputselections
    # observeEvent(input$thema, {
    #     if (input$thema == i18n_r()$t("Differences ")) {
    #         updateRadioGroupButtons(session,"test",label = i18n_r()$t("Sociodemographic"),size = "normal",choices = i18n_r()$t(c("None","Migration")))
    #     } 
    #     else {
    #         updateRadioGroupButtons(session,"test",size = "normal",choices = i18n_r()$t(c("None", "Age", "Gender","Status")))
    #     }
    #     
    #     
    # })
    # rename #github examp.

    # sprache observe -------------------------------------------------------------
    observe({  #reactive update for labels
      #Achtung. translate rbaucht einer übersetzung in den radiobuttons, ansonsten spinnt der abru der hcs.
        updateRadioGroupButtons(session,"thema",label = i18n_r()$t("Year"),size = "normal",choices = i18n_r()$t(c("2019","2020","Differences")))
    })
    
    observe({  #reactive update for labels
      #Achtung. translate braucht einer übersetzung in den radiobuttons, ansonsten spinnt der abru der hcs.
        updateRadioGroupButtons(session,"test",label = i18n_r()$t("Sociodemographic"),size = "normal",choices = i18n_r()$t(c("None", "Age", "Gender","Status")))
    })
    
    observe({  #reactive update for labels
      #Achtung. translate braucht einer übersetzung in den radiobuttons, ansonsten spinnt der abru der hcs.
      updateFlipBox(1,session)
    })
    
    
    
    observeEvent(input$selected_language,ignoreInit = T, {
        # This print is just for demonstration
        print(paste("Language change!", input$selected_language))
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$selected_language)
        
     
        delay(700,{runjs(i18n_html)}) # delay wegen Reihenfolge nötig. Change language, dann neu setzen. js_code is für translation workaround notwendig. i18n setzt html als plain ein. Nicht als HTML. 
    })
    
    #doesnt work in deployed applications
    # observeEvent(input$mydata,{  
    #   if (input$mydata == 116) {
    #     print("works")
    #     browseURL("https://hdbt.shinyapps.io/translate/")
    #     
    #   }
    #   print(input$mydata)
    # })

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

#----- To Do------ 
# 
