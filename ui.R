library(shiny)
library(shinyjs)
# Define UI ----
shinyUI(
  
  fluidPage(
    shinyjs::useShinyjs(),
  
    titlePanel("Same/Same Analysis"),
    
    sidebarPanel(
           # Upload manager
           fileInput("file", "Upload your fles here", multiple = TRUE),
           uiOutput("selectfile"),
           h4("File Options:"),
           checkboxInput(inputId = "header", label = "Use Headers", value = TRUE),
           radioButtons(inputId = 'seperator', label = "Seperator", choices = c(Comma=',', Semicolon=';', Tab='\t', Space=' '), selected = ','),
           textInput(inputId = 'PQFDRValue', label = "PQ-FDR Value", value = '1', width = 100 ),
           radioButtons(inputId = 'method', label = "Multiple Test Correction Method", choices = c(BioConductorQ = "BioQ", BenH = "BH", Bonferroni = "bonferroni", BenY = "BY", VanillaFDR="fdr"), selected = 'BH'),
           width = 2),
    
    fluidRow(

      column(2,
        uiOutput("IDselect"),
        uiOutput("numberselect"),
        uiOutput("run"),
        uiOutput("check"),
        shinyjs::hidden(p(id = "text1", "...Processing..."))
        ),
      
      column(7,
        uiOutput("finaltable"),
        uiOutput("finaltable2")
      )
    )
))
