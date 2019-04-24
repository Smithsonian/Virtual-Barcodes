# Load packages ----
library(shiny)
library(DT)
library(dplyr)
library(futile.logger)
library(EDANr)
library(RSQLite)
library(shinyWidgets)
library(RPostgres)
library(shinycssloaders)

app_name <- "Virtual Barcodes"
app_ver <- "0.2.2"
github_link <- "https://github.com/Smithsonian/VirtualBarcodes/"

options(stringsAsFactors = FALSE)
options(encoding = 'UTF-8')

# Settings -----
source("settings.R")

#Logfile
logfile <- paste0("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
flog.logger("barcode", INFO, appender=appender.file(logfile))



if (!exists("kiosk")){
  kiosk <- FALSE
}



# UI -----
ui <- fluidPage(
  titlePanel(project_name),
  br(),
  fluidRow(
    column(width = 2, 
           textInput("search_term", "Enter ID, title, or text from item"),
           actionButton("submit", "Search"),
           checkboxInput("takenfilter", "Search imaged objects", FALSE),
           hr(),
           uiOutput("loading"),
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
    
    flog.info(paste0("search_term: ", input$search_term), name = "barcode")
    
    if (input$takenfilter){
      results <<- search_db(input$search_term, database_file, TRUE)
    }else{
      results <<- search_db(input$search_term, database_file, FALSE)
    }
    
    output$table1 <- DT::renderDataTable({
      
      results_table <- dplyr::select(results, ID_NUMBER, ITEM_NAME, TITLE, DESCRIPTION, MEASUREMENTS)
      
      flog.info(paste0("number of results: ", dim(results_table)[1]), name = "barcode")
      
      DT::datatable(results_table, 
                    escape = FALSE, 
                    rownames = FALSE,
                    selection = "single",
                    caption = "Items found",
                    options = list(searching = TRUE, 
                                   ordering = FALSE, 
                                   pageLength = 15, 
                                   paging = FALSE, 
                                   rownames = FALSE, 
                                   selection = 'single')
      ) %>% formatStyle(c("TITLE", "ID_NUMBER"), "white-space"="nowrap")
    })
    
    output$loading <- renderUI({
      HTML("&nbsp;")
    })
    
    #})
    
    # item_barcode ----
    output$item_barcode <- renderImage({
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      req(res)
      
      if (is.na(res$MKEY)){
        req(FALSE)
      }
      
      flog.info(paste0("item_barcode_res: ", paste(res, collapse = ';')), name = "barcode")
      
      unique_id <- paste0(image_prefix, res$MKEY)
      
      flog.info(paste0("unique_id: ", unique_id), name = "barcode")
      
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
      if (is.na(res$MKEY)){
        req(FALSE)
      }
      
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
      if (is.na(res$MKEY)){
        req(FALSE)
      }
      
      try({
        query <- gsub('[\"]', '', res$ID_NUMBER)
        flog.info(paste0("edan_query: ", query), name = "barcode")
        
        try(results <- EDANr::searchEDAN(query = query, 
                                         AppID = AppID, 
                                         AppKey = AppKey, 
                                         rows = 1, 
                                         start = 0), silent = TRUE)
        
        if (length(results$rows) > 0){
          ids_id <- results$rows$content$descriptiveNonRepeating$online_media$media[[1]]$idsId
          
          if (!is.null(ids_id)){
            flog.info(paste0("edan_query_ids: ", ids_id), name = "barcode")
            
            img_url <- paste0("http://ids.si.edu/ids/deliveryService?id=", ids_id, "&max_w=250")
            cat
            tagList(
              p("Object image from EDAN:"),
              tags$img(src = img_url)
            )
          }
          
        }
      }, silent = TRUE)
    })
    
    
    # item_info ----
    output$item_info <- renderUI({
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      req(res)
      if (is.na(res$MKEY)){
        req(FALSE)
      }
      
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
      if (is.na(res$MKEY)){
        req(FALSE)
      }
      
      tagList(
        actionButton("delrecord", 
                     label = "Image Taken", 
                     class = "btn btn-primary",
                     icon = icon("remove", lib = "glyphicon")),
        br(),
        uiOutput("insert_msg")
      )
    })
    
    #image_taken----
    observeEvent(input$delrecord, {
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      req(res)
      if (is.na(res$MKEY)){
        req(FALSE)
      }
      
      unique_id <- res$MKEY
      
      db <- dbConnect(RPostgres::Postgres(), dbname = pg_db,
                      host = pg_host, port = 5432,
                      user = pg_user, password = pg_pass)
      
      flog.info(paste0("res_db: ", paste(res, collapse = ";")), name = "barcode")
      flog.info(paste0("insert_db: ", unique_id), name = "barcode")
      
      check_q <- paste0("SELECT COUNT(*) as no_rows FROM posters_taken WHERE \"MKEY\" = ", unique_id)
      no_rows_taken <- dbGetQuery(db, check_q)
      
      #Avoid adding again or database logic for this
      if (no_rows_taken == 0){
        insert_query <- paste0("INSERT INTO posters_taken (\"MKEY\") VALUES (", unique_id, ")")
        
        flog.info(paste0("insert_db_query: ", insert_query), name = "barcode")
        
        n <- dbExecute(db, insert_query)
      }
      
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
