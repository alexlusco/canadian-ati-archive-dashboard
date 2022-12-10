##########################
# load dependencies and read prepped data
##########################

library(shiny)
library(shinythemes)
library(DT)

source('dashboard-data-prep.R')

##########################
# ui
##########################

ui <- function(){
  
  fluidPage(
    
    theme = shinytheme("simplex"),
    
    titlePanel(title = "🇨🇦 Canadian ATI Archive", 
               windowTitle = "Canadian ATI Archive"),
    
    br(),
    
    includeHTML("index.html"),
    
    br(),
    
    tabsetPanel(
      
      tabPanel(
        
        title = "English",
        
        fluid = TRUE,
        
        br(),
        
        downloadButton(outputId = "download_filtered_en",
                       label = "filtered data"),
        
        downloadButton(outputId = "download_all_en",
                       label = "all data"),
        
        br(), br(),
        
        p("Total filtered rows: "), 
        
        verbatimTextOutput("filtered_row_en"),
        
        br(),
        
        dataTableOutput("dt_en")
        
      ),
      
      tabPanel(
        
        title = "Français",
        
        fluid = TRUE,
        
        br(),
        
        downloadButton(outputId = "download_filtered",
                       label = "données filtrée"),
        
        downloadButton(outputId = "download_all_fr",
                       label = "toutes les données"),
        
        br(), br(),
        
        p("Nombre de lignes :"), 
        
        verbatimTextOutput("filtered_row_fr"),
        
        br(),
        
        dataTableOutput("dt_fr")
      )
    )
  )
}

##########################
# server
##########################

server <- shinyServer(
  
  function(input, output, session){
    
    output$dt_en <- 
      
      # make en lang table
      renderDataTable(
        
        datatable(ati_summaries_en,
                  autoHideNavigation = TRUE,
                  escape = FALSE,
                  options = list(autoWidth = FALSE, columnDefs = list(list(width = '200px', targets = "_all"))), 
                  filter = list(position = 'top', clear = TRUE),
                  style = "bootstrap",
                  rownames = FALSE,
                  class = "compact",
                  selection = "none"
        )
      )
    
    # make fr lang table
    output$dt_fr <- 
      
      renderDataTable(
        
        datatable(ati_summaries_fr,
                  autoHideNavigation = TRUE,
                  escape = FALSE,
                  options = list(autoWidth = FALSE, columnDefs = list(list(width = '200px', targets = "_all"))), 
                  filter = list(position = 'top', clear = TRUE),
                  style = "bootstrap",
                  rownames = FALSE,
                  class = "compact",
                  selection = "none"
        )
      )
    
    # make download all button english
    output$download_all_en <- 
      
      downloadHandler(
        
        filename = "ati_summaries_all.csv",
        
        content = function(file){
          
          write_csv(ati_summaries_pp, file)
        
          }
      )
    
    # make download all button french
    output$download_all_fr <- 
      
      downloadHandler(
        
        filename = "ati_summaries_all.csv",
        
        content = function(file){
          
          write_csv(ati_summaries_pp, file)
          
        }
      )
    
    # make download all button english
    output$download_filtered_en <- 
      
      downloadHandler(
        
        filename = "ati_summaries_filtered_en.csv",
        
        content = function(file){
          
          write_csv(ati_summaries_en[input[["dt_en_rows_all"]], ], file)
        
          }
      )
    
    # make download all button french
    output$download_filtered_fr <- 
      
      downloadHandler(
        
        filename = "ati_summaries_filtered_fr.csv",
        
        content = function(file){
          
          write_csv(ati_summaries_fr[input[["dt_fr_rows_all"]], ], file)
          
        }
      )
    
    # console output of numbers filtered english
    output$filtered_row_en <- 
      
      renderPrint({
        
        cat(length(input[["dt_en_rows_all"]]))})
    
    # console output of numbers filtered french
    output$filtered_row_fr <- 
      
      renderPrint({
        
        cat(length(input[["dt_fr_rows_all"]]))})
    
  })

shinyApp(ui, server)
