library(shiny)
library(shinyBS)
library(shinyjs)
library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(pscl)
library(ggplot2)
library(imager)

# Define server logic ----


  # combinations <- combn(6,3, nbins = 3, simplify=FALSE)
  # print(combinations[[1]])
  # print(combinations[[length(combinations)]])
  # # The following lines present two methods for producing a single dataset that combines all of my results into a 'full join'. I'm going to use df2 method.
  # # df1 <- reduce(datahere, full_join, by = ID) %>% replace(., is.na(.), 0);
  # # colnames(df1) <- c(ID,1,2,3,4,5,6)
  # df2 <- dplyr::bind_rows(datahere, .id="ID") %>% tidyr::spread(ID, numcolumn, fill=0)
  # print(df2)
  # df3 <- sapply(seq_along(df2), function(x){zeroinfl( . | 1 , df2[x])})
  # print(df3)

shinyServer(function(input, output){
  
  this.dir <- dirname('server.r')
  setwd(this.dir)
  
  runsamesame <- function() {
    numbercolumntouse <- input$Column
    ID <- input$Column2
    datahere <- producefile(ID, numbercolumntouse)
    qmethod <- input$method
    assign('ID',ID,envir=.GlobalEnv)
    assign('datahere',datahere,envir=.GlobalEnv)
    assign('PQFDR', strtoi(input$PQFDRValue)/100, envir=.GlobalEnv)
    assign('qmethod', qmethod,envir=.GlobalEnv)
    source("samesame.R")
  }
  
  producefile <- function(id, num) {
    data <- lapply(input$file$datapath, fread, select = c(id, num), header=input$header)
    return(data)
    }
  
  output$uploadedfiles <- renderTable({    
    if(is.null(input$file)){return ()}
    input$file
  })
  
  output$uploadedfilesdatapath <- renderTable({
    if(is.null(input$file)){return ()}
    input$file$datapath
  })
  
  output$fileobject <- renderPrint({
    if(is.null(input$file)){return ()}
    str(input$file)
  })
  
  output$selectfile <- renderUI({
    if(is.null(input$file)) {return()}
    list(helpText("Browse through your data here"),
         selectInput("Select", "Select", choices=input$file$name)
    )
  })
  
  output$IDselect <- renderUI({
    if(is.null(input$file)) {return()}
    list(
         h4("Please select appropriate columns:"),
         helpText("Column that contains numerical data:"),
         # For the line below, we can also use:
         # choices=c(1:ncol(read.table(...
         # choices=c(colnames(read.table(...
         selectInput("Column", "Column", choices=c(colnames(read.table(file=input$file$datapath[input$file$name==input$Select], sep=input$seperator, header = input$header, comment.char = ""))))
    )
  })
  
  output$numberselect <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(),
         helpText("Column that contains the Identifiers:"),
         # For the line below, we can also use:
         # choices=c(1:ncol(read.table(...
         # choices=c(colnames(read.table(...
         selectInput("Column2", "Column2", choices=c(colnames(read.table(file=input$file$datapath[input$file$name==input$Select], sep=input$seperator, header = input$header, comment.char = ""))))
    )
  })
  
  output$run <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(),
         hr(),
        actionButton(inputId = 'run', label = "Run Same/Same"))
  })

  observeEvent(input$run, {
    shinyjs::disable('run')
    shinyjs::show("text1")
    runsamesame()
    shinyjs::enable('run')
    shinyjs::hide('text1')
    shinyjs::hide('finaltable2')
    output$check <- renderUI({
      list(hr(),
           actionButton(inputId = 'check', label = "Check Results"))
    })
  })
  
  observeEvent(input$check, {
    shinyjs::hide('finaltable')
    shinyjs::show('finaltable2')
  })
  
  output$data <- renderTable({ 
    if(is.null(input$file)){return()}
    read.table(file=input$file$datapath[input$file$name==input$Select], sep=input$seperator, header = input$header, comment.char = "", check.names = FALSE)
  })
  
  output$finaltable <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
          tabPanel("Summary", tableOutput("uploadedfiles"), tableOutput("uploadedfilesdatapath")),
          tabPanel(paste("Data: ", toString(input$Select)), tableOutput("data"))
        )
  })
  
  output$finaltable2 <- renderUI({
    if(is.null(input$check)) {return()}
    else
      tabsetPanel(
        tabPanel("Q Bins", renderPlot(replayPlot(qplot), height = 600)),
        tabPanel("P Value Histogram", renderPlot(replayPlot(tplot), height = 600)),
        tabPanel("Q Value to Use", h3(toString(qaverage)))
      )
  })
})