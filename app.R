##########################
# load dependencies
##########################

library(shiny)
library(shinythemes)
library(DT)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

##########################
# read in data, clean etc.
##########################

# read in data from LC's GitHub scraper
ati_summaries <- read_csv("https://raw.githubusercontent.com/lchski/gc-ati-summaries-data/main/ati-summaries.csv") %>% select(-umd_number)

# merge year and month and convert to 'date' format
ati_summaries_pp <- ati_summaries %>%
  mutate(date = paste(year, month, "01", sep = "-")) %>%
  mutate(date = lubridate::ymd(date)) %>%
  select(-year, -month)

# separate fr and en org names
ati_summaries_pp <- ati_summaries_pp %>%
  separate(owner_org_title, into = c("org_en", "org_fr"), sep = "\\|") %>%
  mutate_at(vars(org_en, org_fr), str_squish)

# separate fr and en org acronyms 
ati_summaries_pp <- ati_summaries_pp %>%
  separate(owner_org, into = c("org_ac_en", "org_ac_fr"), sep = "-") %>%
  mutate(org_ac_fr = case_when(
    is.na(org_ac_fr) ~ org_ac_en,
    TRUE ~ org_ac_fr
  )) %>%
  mutate_at(vars(org_ac_en, org_ac_fr), str_to_upper)

# relocate some vars
ati_summaries_pp <- ati_summaries_pp %>%
  relocate(date, .before = request_number) %>%
  relocate(org_ac_en, .after = org_en) %>%
  relocate(org_ac_fr, .after = org_fr)

# make org names and acronym factors
ati_summaries_pp <- ati_summaries_pp %>%
  mutate_at(vars(org_ac_en, org_ac_fr, org_en, org_fr), as.factor)

# make pages disclosed chr to kill slider filter in DT
ati_summaries_pp <- ati_summaries_pp %>%
  mutate(pages = as.character(pages))

# add placeholder variables for PINPOINT info
ati_summaries_pp <- ati_summaries_pp %>%
  mutate(archived = "???",
         url = "???")

# split file in two two (fr and en)
ati_summaries_en <- ati_summaries_pp %>%
  select(date, request_number, summary_en, pages, org_ac_en, org_en, archived, url)

ati_summaries_fr <- ati_summaries_pp %>%
  select(date, request_number, summary_fr, pages, org_ac_fr, org_fr, archived, url)

##########################
# ui
##########################

ui <- function(){
  
  fluidPage(
    
    theme = shinytheme("simplex"),
    
    titlePanel(title = "🇨🇦 Canadian ATI Archive", 
               windowTitle = "Canadian ATI Archive"),
    
    br(),
    
    includeHTML("https://raw.githubusercontent.com/jdunca/cdn-ati-archive-website/main/index.html"),
    
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
                       label = "données filtrée_fr"),
        
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
