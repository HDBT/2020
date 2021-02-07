library(shiny)
library(shiny.i18n)
library(shinyWidgets)
library(highcharter)
i18n <- Translator$new(translation_csvs_path = "../Alpha0")

ui <- fluidPage(
    usei18n(i18n),
    h1(i18n$t("Hello")),
    actionButton("change", i18n$t("Change language")),
    radioGroupButtons("radio", i18n$t("Radio"), c("Identitaet", "Theme")),
    selectInput("select", i18n$t("Choose"), c("one", "two", "three")),
    radioGroupButtons("test",i18n$t("Thema"), choiceNames = c("Identitaet","Politisches Interesse","Politische Aktion"),choiceValues = c("Identität","Politisches Interesse","Politische Aktion"), size = "normal",direction = "horizontal")
    ,highchartOutput("hcchart1")
)

server <- function(input, output, session) {
    
    i18n_r <- reactive({
        i18n
    })
    
    observeEvent(input$change, {
        lang <- ifelse(as.numeric(input$change) %% 2, "de", "en")
        shiny.i18n::update_lang(session, lang)
        i18n$set_translation_language(lang)
    })
    
    observe({
        updateRadioGroupButtons(session, "radio", label = i18n_r()$t("Radio"),
                                choices = i18n_r()$t(c("Identitaet", "Theme")))
    })
    
    output$hcchart1 <- renderHighchart({
        #switch proxy für charttype
        
        dfn <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,26,91,90,89) )
        dfx <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,35,90,82,82), y1 = c (51,24,82,81,81),y2= c(37,36,76,80,82) )
        df2 <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(50,30,80,76,75), y1 = c (48,26,81,80,81),y2= c(48,30,79,82,82) )
        df3 <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(48,27,82,82,81), y1 = c(47,27,79,78,79))
        df4 <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(47,26,92,92,85), y1 = c (48,34,89,93,94), y2 = c (49,42,84,76,75))
        
        df_l  <- lst(dfn,dfx)
        print(head(df_l))
        l2<-lapply(df_l, function(df) 
            cbind(df, b = df$y *1.1, c = df$y *1.2, d = df$y *0.7))

        colors <- c("#e41618","#52bde7","#4d4d52","#90b36d","#f5951f","#6f4b89","#3fb54e","#eea4d8")
        
        
        hc <-   highchart() %>%
            hc_xAxis(labels = list(style = list(fontSize = "16px"))) %>% 
            hc_yAxis(labels= list(format = "{value} %", style = list(fontSize = "16px"))) %>%
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
        
        if (input$radio == i18n_r()$t("Theme")) {
            #dfn <- tibble(name = i18n$t(c("Being Born in Lux.","Having Lux. Ancestors","Speaking Lux. Well","Lived for a long time in Lux.","Identifying with Lux.")),y = c(49,26,91,90,89) )
            
            hc %>% 
                hc_title(text = "Percentage of answers “Very Important” and “Important” according to the dimensions of National Identity.")%>%
                hc_xAxis(categories = dfn$name ,additonialInfo = 1:4 ) %>% 
                hc_add_series(name= " ",data =l2$dfn[c("name","y")] ,showInLegend = F)
            
        }
    # if (input$test == i18n$t("Theme")) {
    #     print("Ok")}
        
    })
    
}
shinyApp(ui, server)