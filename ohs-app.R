library(shiny)
source("ohs-check-eaf.R")

# define UI for data upload app ----
ui <- fluidPage(
  
  # app title ----
  titlePanel("OHS transcription error spotter"),
  
  # sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # sidebar panel for inputs ----
    sidebarPanel(
      
      # input: annotation file ----
      fileInput("file1", "Choose your annotation file",
                accept = c("text/tab-separated-values",
                           ".txt")),
      
      # submit button:
      actionButton("submit", "Submit")
    ),
    # main panel for displaying outputs ----
    mainPanel(
      uiOutput("report"),
      uiOutput("downloadErrors")
    )
  )
)

# define server logic to read selected file ----
server <- function(input, output) {
  report <- eventReactive(input$submit, {
    req(input$file1)
    check.annotations(input$file1$datapath, input$file1$name)
  })
  
  output$report <- renderUI({
    req(report())
    
    tagList(
      tags$br(),
      renderText(paste0("Number of potential errors and/or warnings detected: ",
                        as.character(report()$n.a.alerts))),
      renderText("(downloadable list below)"),
      tags$br()
    )
  })
  
  output$downloadErrors <- renderUI({
    # output file name
    time.now <- gsub('-|:', '', as.character(Sys.time()))
    time.now <- gsub(' ', '_', time.now)
    
    errors <- report()$alert.table
    
    output$downloadErrorsHandler <- downloadHandler(
      filename = paste0("minCHATerrorcheck-",time.now,"-possible_errors.csv"),
      content = function(file) {
        write_csv(errors, file)
      },
      contentType = "text/csv"
    )
    
    downloadButton("downloadErrorsHandler", 
                   "Errors and/or warnings detected? 
                   Download a list of suspected issues here.")
  })
}

# create Shiny app ----
shinyApp(ui, server)
