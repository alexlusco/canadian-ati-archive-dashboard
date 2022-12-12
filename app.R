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
        
        uiOutput("archived_filter_en"),
        
        br(),
        
        p("Total filtered rows: ", style="font-weight: bold;"), 
        
        htmlOutput("filtered_row_en", style="color: #D9230F;"),
        
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
        
        uiOutput("archived_filter_fr"),
        
        br(),
        
        p("Nombre de lignes :", style="font-weight: bold;"), 
        
        htmlOutput("filtered_row_fr", style="color: #D9230F;"),
        
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
    
    # show only archived button english
    output$archived_filter_en <- renderUI({
      
      checkboxInput("archived_filter_en",
                    label = "Display only archived entries")
    })
    
    # show only archived button french
    output$archived_filter_fr <- renderUI({
      
      checkboxInput("archived_filter_fr",
              label = "Résultats archivés uniquement")
    })
    
    # make en lang table
    output$dt_en <- 
      
      renderDataTable(
          ati_summaries_en %>%
            filter(if (input$archived_filter_en == TRUE) str_detect(pinpoint, "Pinpoint") else TRUE) %>%
            mutate(order_var = str_detect(pinpoint, "Pinpoint"),
                   order_var = factor(order_var, levels = c("TRUE", "FALSE"))) %>%
              arrange(order_var) %>%
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
                                                         list(width = '75px', targets = c(7, 8)), #pinpoint, internet_archive
                                                         list(visible = FALSE, targets = c(9)) #hide order_var
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
          filter(if (input$archived_filter_fr == TRUE) str_detect(pinpoint, "Pinpoint") else TRUE) %>%
          mutate(order_var = str_detect(pinpoint, "Pinpoint"),
                 order_var = factor(order_var, levels = c("TRUE", "FALSE"))) %>%
          arrange(order_var) %>%

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
                                                     list(width = '75px', targets = c(7, 8)), #pinpoint, internet_archive
                                                     list(visible = FALSE, targets = c(9)) #hide order_var
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
