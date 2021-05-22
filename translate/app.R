# Only run examples in interactive R sessions

    library(shiny)
    library(editData)
    library(jsonlite)
    library(shinyjs)
    library(googlesheets4)
    # define js function for opening urls in new tab/window
    js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"
    
  gs <-  "https://docs.google.com/spreadsheets/d/1__BIT475NLesFhRhsQJwi5_1srPFG7dOkSiehemhi_w"

    # plumber
    ui=fluidPage(  useShinyjs(),
                   extendShinyjs(text = js_code, functions = 'browseURL'),
                   
        tags$head(tags$style(HTML('#editableDT-edit {background-color:white}'))),
        tags$head(tags$style(HTML('#editableDT-deleteAll {display:none}'))),
        tags$head(tags$style(HTML('#editableDT-insert {display:none}'))),
        tags$head(tags$style(HTML('#editableDT-reset {display:none}'))),
        hr(),
        editableDTUI("editableDT"),downloadButton("downloadjson", "Download JSON"),
        actionButton("browse", "HTML Editor"),
        hr(),
        verbatimTextOutput("test"),
        actionButton("action", "Speichern", icon = icon("save"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
    server=function(input,output,session){
      googlesheets4::gs4_deauth()
      
       gs_data <-  read_sheet(gs)
      
        data=reactive({
            readRDS("mydata.RDS")
        })
        observeEvent(input$select,{
            updateTextInput(session,"mydata",value=input$select)
        })
        result=callModule(editableDT,"editableDT",data= data)
        output$test=renderPrint({
            timestamp<- readRDS("timestamp.RDS")
            paste("Last saved on", timestamp)
        })
        observeEvent(input$action,{
           timestamp <-  timestamp()
           saveRDS(timestamp, "timestamp.RDS")
           
         data    <- result()
        saveRDS(data, "mydata.RDS")
        df0 %>% 
          sheet_write(gs)
        })
        observeEvent(input$browse,{
                             js$browseURL("https://onlinehtmleditor.dev")
        } 
        )
        
        output$downloadjson <- downloadHandler(filename = function() {
            "translation_general.json"
        }, content = function(file) {
            data    <- result()
            trans <- list("languages" = c("en","de","fr"), "translation" = data)
            jsonlite::write_json(trans, file)
        })
        #jsonlite::write_json(data, "translation_general_neu.json")

        
    }

    shinyApp(ui = ui, server = server)
