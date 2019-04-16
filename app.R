# Load packages ----
library(shiny)
library(DT)
library(dplyr)
library(futile.logger)
library(EDANr)
library(RSQLite)
library(shinyWidgets)

app_name <- "Virtual Barcodes"
app_ver <- "0.2.0"
github_link <- "https://github.com/Smithsonian/VirtualBarcodes/"

options(stringsAsFactors = FALSE)
options(encoding = 'UTF-8')

# Settings -----
source("settings.R")

#Logfile
logfile <- paste0("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")

#Load data
#load(data_file)



# UI -----
ui <- fluidPage(
  titlePanel(project_name),
   br(),
   fluidRow(
     column(width = 2, 
            textInput("search_term", "Enter ID, title, or text from item"),
            actionButton("submit", "Search"),
            #checkboxInput("takenfilter", "Search imaged objects", FALSE),
            br(),
            hr(),
            uiOutput("item_image")
     ),
     column(width = 7, 
            uiOutput("item_info")
     ),
     column(width = 3, 
            uiOutput("item_filename"),
            imageOutput("item_barcode"),
            uiOutput("delbutton")
     )
   ),
  #hr(),
  DT::dataTableOutput("table1"),
  hr(),
  if (kiosk){
    HTML(paste0("<p><img src=\"dpologo.jpg\" title=\"Digitization Program Office\"> | ", 
                app_name, 
                " ver. ", 
                app_ver, 
                "</p>"))  
  }else{
    HTML(paste0("<p><a href=\"http://dpo.si.edu\" target = _blank><img src=\"dpologo.jpg\"></a> | ",
                app_name,
                " ver. ",
                app_ver,
                " | <a href=\"",
                github_link,
                "\" target = _blank>Source code</a></p>"))
  }
)


# Server ----
server <- function(input, output, session) {

  # table1 ----
  observeEvent(input$submit, {
    
    req(input$search_term)
    
    # if (input$takenfilter){
    #   results <<- search_db(input$search_term, database_file, TRUE)
    # }else{
    #   results <<- search_db(input$search_term, database_file, FALSE)
    # }
    results <<- search_db(input$search_term, database_file, FALSE)
    
    output$table1 <- DT::renderDataTable({
      
      results_table <- dplyr::select(results, ID_NUMBER, ITEM_NAME, TITLE, DESCRIPTION, MEASUREMENTS)
  
      DT::datatable(results_table, 
                    escape = FALSE, 
                    rownames = FALSE,
                    selection = "single",
                    caption = "Items found",
                    options = list(searching = FALSE, 
                                   ordering = FALSE, 
                                   pageLength = 15, 
                                   paging = FALSE, 
                                   rownames = FALSE, 
                                   selection = 'single')
      ) %>% formatStyle(c("TITLE", "ID_NUMBER"), "white-space"="nowrap")
    })
  
    # item_barcode ----
    output$item_barcode <- renderImage({
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      req(res)
      
      unique_id <- paste0(image_prefix, res$MKEY)
      
      system(paste("python3 scripts/barcode.py", unique_id, barcode_size))
      filename <- paste0("data/", unique_id, ".png")
      
      # Return a list containing the filename and alt text
      list(src = filename, alt = "Item data matrix")
    })
    
    
    # item_filename ----
    output$item_filename <- renderUI({
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      req(res)
      
      unique_id <- paste0("<p><strong>", image_prefix, res$MKEY, "</strong></p>")
      
      output$insert_msg <- renderUI({
        HTML("&nbsp;")
      })
      
      HTML(unique_id)
    })
    
    
    # item_image ----
    output$item_image <- renderUI({
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      req(res)
      
      query <- res$ID_NUMBER
      
      results <- EDANr::searchEDAN(query = query, 
                                   AppID = AppID, 
                                   AppKey = AppKey, 
                                   rows = 1, 
                                   start = 0)
      
      if (length(results$rows) == 0){
        req(FALSE)
      }
      
      if(exists("results$rows$content$descriptiveNonRepeating$online_media")){
        ids_id <- results$rows$content$descriptiveNonRepeating$online_media$media[[1]]$idsId
        
        img_url <- paste0("http://ids.si.edu/ids/deliveryService?id=", ids_id, "&max_w=250")
        
        tagList(
          p("Object image from EDAN:"),
          tags$img(src = img_url)
        )
      }
    })
    
    
    # item_info ----
    output$item_info <- renderUI({
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      #Continue only if there is a single row
      req(res)
      
      row_data <- "<dl class=\"dl-horizontal\">"
      
      for (j in 1:dim(results)[2]){
        if (!is.na(res[1, j])){
          row_data <- paste0(row_data, "<dt>", names(results)[j], "</dt><dd>", res[1, j], "</dd>")
        }
      }
      
      row_data <- paste0(row_data, "</dl>")
      
      shinyWidgets::panel(
        HTML(row_data),
        heading = "Row selected",
        status = "primary"
      )

    })
    
    
    output$delbutton <- renderUI({
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      req(res)
      
      tagList(
        actionButton("delrecord", 
                     label = "Image Taken", 
                     class = "btn btn-primary",
                     icon = icon("remove", lib = "glyphicon")),
        br(),
        uiOutput("insert_msg")
      )
      })
    
    observeEvent(input$delrecord, {
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      req(res)
      
      unique_id <- res$MKEY
      
      db <- dbConnect(RSQLite::SQLite(), database_file)
      
      n <- dbExecute(db, paste0("UPDATE posters SET taken = 1 WHERE MKEY = '", unique_id, "'"))
      
      dbDisconnect(db)
      
      output$insert_msg <- renderUI({
        HTML("<br><div class=\"alert alert-success\" role=\"alert\">Object marked as Done</div>")
      })
    })
  })  
}


# Run app ----
shinyApp(ui = ui, server = server, onStart = function() {
  cat("Loading<dt>")
  onStop(function() {
    cat("Closing")
    cat("Removing objects")
    rm(list = ls())
  })
})
