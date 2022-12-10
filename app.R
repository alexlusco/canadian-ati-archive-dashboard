##########################
# load dependencies and read prepped data
##########################

library(shiny)
library(shinythemes)
library(DT)

source('data-prep.R')

##########################
# ui
##########################

ui <- function(){
  
  fluidPage(
    
    tags$style(
      ".container-fluid {
      margin-left: 20px;
      margin-right: 20px;
      }"
      ),
    

    
    theme = shinytheme("simplex"),
    
    titlePanel(title = "🇨🇦 Canadian ATI Archive", 
               windowTitle = "Canadian ATI Archive"),
    
    br(),
    
    includeHTML("www/index.html"),
    
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
        
        p("Total filtered rows: ", style="font-weight: bold;"), 
        
        htmlOutput("filtered_row_en"),
        
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
        
        p("Nombre de lignes :", style="font-weight: bold;"), 
        
        htmlOutput("filtered_row_fr"),
        
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
        
        ati_summaries_en %>%
        
          datatable(autoHideNavigation = TRUE,
                    escape = FALSE,
                    options = list(autoWidth = TRUE,
                                   scrollX = FALSE,
                                   columnDefs = list(list(width = '75px', targets = c(0)), #date
                                                     list(width = '175px', targets = c(1)), #request_number
                                                     list(width = '200px', targets = c(2)), #summary
                                                     list(width = '100px', targets = c(3, 6)), #disposition, org
                                                     list(width = '50px', targets = c(4)), #pages
                                                     list(width = '100px', targets = c(5)), #org_ac
                                                     list(width = '75px', targets = c(7, 8)) #pinpoint, internet_archive
                                                     
                                   )),
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
        
        ati_summaries_fr %>%
          

          
          datatable(autoHideNavigation = TRUE,
                    escape = FALSE,
                    options = list(autoWidth = TRUE,
                                   scrollX = FALSE,
                                   columnDefs = list(list(width = '75px', targets = c(0)), #date
                                                     list(width = '175px', targets = c(1)), #request_number
                                                     list(width = '200px', targets = c(2)), #summary
                                                     list(width = '100px', targets = c(3, 6)), #disposition, org
                                                     list(width = '50px', targets = c(4)), #pages
                                                     list(width = '100px', targets = c(5)), #org_ac
                                                     list(width = '75px', targets = c(7, 8)) #pinpoint, internet_archive
                                                     
                                   )),
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
