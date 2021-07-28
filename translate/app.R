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

  
   

   data <- as.data.frame(x= 1:3)
   #save_data_gsheets(df019)
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

      sheetid <- "1__BIT475NLesFhRhsQJwi5_1srPFG7dOkSiehemhi_w"
      save_data_gsheets <- function(data) {
        data <- data %>% as.list() %>% data.frame()
        googlesheets4::sheet_write(sheet = "Tabellenblatt1",ss = sheetid, data = data)
      }
    
      load_data_gsheets <- function() {
        gs4_auth(path = "able-math-198615-c979755ea451.json")
        
        read_sheet(sheetid)
      }
      
      
        data=reactive({
          load_data_gsheets()
          #as.data.frame(read_sheet(sheetid))
         #class(readRDS("mydata.RDS"))
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
       # saveRDS(data, "mydata.RDS")
        gs4_auth(path = "able-math-198615-c979755ea451.json")
        
        save_data_gsheets(data)
        
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
            jsonlite::write_json(trans,path = file)
        })
        #jsonlite::write_json(data, "translation_general_neu.json")

        
    }

    shinyApp(ui = ui, server = server)
