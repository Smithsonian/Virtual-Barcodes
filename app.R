# Load packages ----
library(shiny)
library(DT)
library(dplyr)
library(futile.logger)
library(shinyWidgets)
library(shinycssloaders)
library(DBI)


app_name <- "Virtual Barcodes"
app_ver <- "0.3.0"
github_link <- "https://github.com/Smithsonian/VirtualBarcodes/"

options(stringsAsFactors = FALSE)
options(encoding = 'UTF-8')

# Settings -----
source("settings.R")
if (search_edan == TRUE){
  library(EDANr)
}

#Logfile
logfile <- paste0("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
flog.logger("barcode", INFO, appender=appender.file(logfile))



# UI -----
ui <- fluidPage(
  titlePanel(paste0(project_name, " - ", app_name)),
  br(),
  fluidRow(
    column(width = 9, 
           fluidRow(
             column(width = 3, 
                    textInput("search_term", search_field_title),
                    hr(),
                    uiOutput("item_edan")
             ),
             column(width = 9, 
                    uiOutput("item_info")
             )
           ),
           shinycssloaders::withSpinner(DT::dataTableOutput("table1"))
    ),
    column(width = 3, 
           #uiOutput("item_filename"),
           shinycssloaders::withSpinner(imageOutput("item_barcode"))#,
           #uiOutput("extras")
    )
  ),
  
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


# Server
server <- function(input, output, session) {
  
  observeEvent(input$search_term, {
    Sys.sleep(1)
    
    #table1 ----
    output$table1 <- DT::renderDataTable({
    
      req(input$search_term)
      req(nchar(input$search_term) > min_search_size)
    
      flog.info(paste0("search_term: ", input$search_term), name = "barcode")
        
      #results <<- search_db(input$search_term)
      results <- search_db(input$search_term)
      session$userData$results <- results
      
      flog.info(paste0("number of results: ", dim(results)[1]), name = "barcode")
      
      DT::datatable(results, 
                    escape = FALSE, 
                    rownames = FALSE,
                    selection = "single",
                    options = list(searching = FALSE, 
                                   ordering = TRUE, 
                                   pageLength = 15, 
                                   paging = TRUE, 
                                   rownames = FALSE)
      )
    })
    

    # item_barcode ----
    output$item_barcode <- renderImage({
      
      req(input$search_term)
      req(nchar(input$search_term) > min_search_size)
      
      results <- session$userData$results
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      # req(input$table1_rows_selected)
      # res <- results[input$table1_rows_selected, ]
      
      req(res)
      
      # if (is.na(res$taxonomy_irn)){
      #   req(FALSE)
      # }
      
      flog.info(paste0("item_barcode_res: ", paste(res, collapse = ';')), name = "barcode")
      
      unique_id <- paste0(image_prefix, res$mkey, image_suffix)
      
      flog.info(paste0("unique_id: ", unique_id), name = "barcode")
      
      system(paste("python scripts/barcode.py", unique_id, barcode_size))
      
      filename <- paste0("data/", unique_id, ".png")
      
      if (!file.exists(filename)){
        #Didn't work, try python3
        system(paste("python3 scripts/barcode.py", unique_id, barcode_size))
      }
      
      # Return a list containing the filename and alt text
      list(src = filename, alt = "Item data matrix")
    })
    
    
    # item_filename ----
    # output$item_filename <- renderUI({
    #   
    #   req(input$search_term)
    #   req(nchar(input$search_term) > min_search_size)
    #   
    #   if ((dim(results))[1] == 1){
    #     res <- results
    #   }else{
    #     req(input$table1_rows_selected)
    #     res <- results[input$table1_rows_selected, ]
    #   }
    #   
    #   req(res)
    #   if (is.na(res$taxonomy_irn)){
    #     req(FALSE)
    #   }
    #   
    #   unique_id <- paste0("<p><strong>", image_prefix, res$taxonomy_irn, image_suffix, "</strong></p>")
    #   
    #   output$insert_msg <- renderUI({
    #     HTML("&nbsp;")
    #   })
    #   
    #   HTML(unique_id)
    # })
    
    
    # item_edan ----
    output$item_edan <- renderUI({
      if (search_edan == TRUE){
        req(input$search_term)
        req(nchar(input$search_term) > min_search_size)
        
        results <- session$userData$results
        
        if ((dim(results))[1] == 1){
          res <- results
        }else{
          req(input$table1_rows_selected)
          res <- results[input$table1_rows_selected, ]
        }
        
        req(res)

        try({
          query <- gsub('[\"]', '', res$id_number)
          flog.info(paste0("edan_query: ", query), name = "barcode")
          
          results1 <- try(EDANr::searchEDAN(query = query, 
                                            AppID = AppID, 
                                            AppKey = AppKey, 
                                            rows = 1, 
                                            start = 0), silent = TRUE)
          
          if (length(results1$rows) > 0){
            ids_id <- results1$rows$content$descriptiveNonRepeating$online_media$media[[1]]$idsId
            
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
        }
      })
    
    
    
    
    # item_info ----
    output$item_info <- renderUI({
      
      req(input$search_term)
      req(nchar(input$search_term) > min_search_size)
      
      results <- session$userData$results
      
      if ((dim(results))[1] == 1){
        res <- results
      }else{
        req(input$table1_rows_selected)
        res <- results[input$table1_rows_selected, ]
      }
      
      req(res)
      
      # if (is.na(res$taxonomy_irn)){
      #   req(FALSE)
      # }
      
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
    
    
    
    # extras ----
    output$extras <- renderUI({
      # 
      # req(input$table1_rows_selected)
      # res <- results[input$table1_rows_selected, ]
      # 
      # 
      # tagList(
      #   textInput("Specimen:", "specimen_id"),
      #   actionButton(label = "Save record", inputId = "submit_specimen")
      # )
      
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
