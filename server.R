library(shiny)
library(shinyjs)
library(dplyr)
library(stringr)
library(png)
library(DT)
library(rintrojs)
library(qdapTools)
library(bslib)
library(httr)
library(zip)
library(sortable)
library(stringi)
library(visNetwork)
library(rcrossref)
library(purrr)


# Define server logic required to draw a histogram
function(input, output, session) {
###########################
### Layout ###
###########################
  
  # Navbar ------------------------------------------------------------------
  shinyjs::addClass(id = "navBar", class = "navbar-right")
  
  # DT Options --------------------------------------------------------------
  options(DT.options = list( lengthMenu = c(10, 20),
                             dom = 'tl'
  ))  # table and lengthMenu options
  
  
  observeEvent(input$btn_MA, {
    shinyjs::runjs('fakeClick("MA")')
  })
  observeEvent(input$btn_WH, {
    shinyjs::runjs('fakeClick("WH")')
  })
  observeEvent(input$btn_fa, {
    shinyjs::runjs('fakeClick("fa")')
  })
  observeEvent(input$btn_IP, {
    shinyjs::runjs('fakeClick("IP")')
  })
  observeEvent(input$btn_DIY, {
    shinyjs::runjs('fakeClick("DIY")')
  })
  
###Temp directory###
  tmp_dir <- file.path(getwd(), "tmp")
  dir.create(tmp_dir, showWarnings = FALSE)
  
###########################
### Home ###
###########################
  
  main_nodes <- data.frame(id = 1:3, 
                           label = c("Home", "The Space", "Contribute"), 
                           value = c(100, 100, 100), 
                           title = "Click to see information", 
                           shape = "dot",
                           color = "#174c92c9")
  main_edges <- data.frame(from = c(1, 1), 
                           to = c(2, 3))
  
  output$network_home <- renderVisNetwork({
    visNetwork(main_nodes, main_edges, width = "100%") %>%
      visEvents(click = "function(properties) {
        var nodeId = properties.nodes[0];
        if(nodeId) {
          var label = this.body.data.nodes.get(nodeId).label;
          Shiny.onInputChange('node_clicked', label);
        }
      }")
  })
  
  observeEvent(input$node_clicked, {
    info <- switch(input$node_clicked,
                   "Home" = "Home is this page. Click the Home button at the top to go back to this page and explore other features.",
                   "The Space" = "The Space: A Network of Preprocessing and Analysis Steps. The network displayed on this tab visualizes the preprocessing and analysis steps identified from graph-based fMRI studies.",
                   "Contribute" = "Contribute here; you can also contribute to the paper base of this app by typing information and uploading a paper either yours or another."
    )
    
    output$node_description <- renderUI({
      wellPanel(
        style = "background-color: #f0f0f0; padding: 10px; border: 1px solid #ddd;",  # Light background with padding and border
        div(style = "font-size: 18px; font-weight: bold;", input$node_clicked),  # Smaller title
        div(style = "font-size: 14px;", info),
        div(style = "margin-top: 20px;", actionButton("go_to_tab", "Go to Tab"))  # Space between text and button
      )
    })
  })
  
  observeEvent(input$go_to_tab, {
    tab_value <- switch(input$node_clicked,
                        "The Space" = "TheSpace",
                        "Contribute" = "Upload",
                        "home"  # Default to home if no match
    )
    updateTabsetPanel(session, "navBar", selected = tab_value)
  })
  
###########################
### The Space ###
###########################
  # This should be a data frame that holds your list of papers
  papers <- reactiveVal(data.frame())
  
  # Download the papers dataframe from the WebDAV server
  urlPapers <- file.path(dav, "papers.csv")
  res <- GET(urlPapers, authenticate(username, password))
  if (res$status_code == 200) {
    papers(read.csv(text = content(res, "text")))
  }
  
  # This should be a vector that holds your DecisionPoints
  DecisionPoints <- reactiveVal()
  
  # Download the DecisionPoints from the WebDAV server
  urlDP <- file.path(dav, "nodes.csv")
  res <- GET(urlDP, authenticate(username, password))
  if (res$status_code == 200) {
    DecisionPoints(read.csv(text = content(res, "text", encoding = "UTF-8")))
  }
  
  
  
  # Define nodes as a reactive value
  nodes <- reactiveVal()
  
  ###Drop down groups
  output$DropDownGroup <- renderUI({
    selectInput("group", "Select Group", choices = DecisionPoints()$Groups)
  })
  
  ###Drop down groups
  output$DropDownStep <- renderUI({
    selectInput("category", "Related Step(s)", c(DecisionPoints()$Steps), multiple = TRUE)
  })
  
  ###Drop down groups
  output$DropDownGroup2 <- renderUI({
    selectInput("new_group", "Group", c(DecisionPoints()$Groups))
  })
  
  ###Output network graph###
  output$network <- renderVisNetwork({
    # Filter the DecisionPoints dataframe to only include rows where the Groups column matches the selected group
    filtered_decision_points <- subset(DecisionPoints(), Groups == input$group)
    
    # Check if filtered_decision_points has at least one row
    if (nrow(filtered_decision_points) > 0) {
      # Include the original row numbers in the nodes dataframe
      nodes(data.frame(id = filtered_decision_points$Names, label = filtered_decision_points$Steps))
      
      # Create a random edges dataframe
      edges <- data.frame(
        from = links$source,
        to = links$target,
        value = links$value
      )
      
      edges <- subset(edges, value != 0)
      
      visNetwork(nodes(), edges) %>% 
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visInteraction(multiselect = FALSE) %>%
        visEvents(select = "function(nodes) {
                  Shiny.onInputChange('current_node_selection', nodes.nodes);
              }")
    } else {
    }
  })
  
  
  ###Output table articles###
  show_all <- reactiveVal(FALSE)
  
  observeEvent(input$show_all, {
    # Set show_all to TRUE when the button is clicked
    show_all(TRUE)
  })
  
  observeEvent(input$current_node_selection, {
    show_all(FALSE)
    selected_node <- input$current_node_selection
    if (length(selected_node) > 0) {
      # Use selected_node as an id instead of an index
      selected_dp <- nodes()$label[nodes()$id == selected_node]
      
      # Update the Definition output based on the selected decision point
      output$Definition <- renderText({
        # Find the matching definition in the Definition dataframe
        matching_definition <- DecisionPoints()$Definition[DecisionPoints()$Steps == selected_dp]
        if (length(matching_definition) > 0) {
          matching_definition
        } else {
          "No definition found for the selected decision point"
        }
      })
    }
  })
  
  output$articles_table <- DT::renderDataTable({
    if (show_all()) {
      papers()
    } else {
      # The existing code to filter papers based on the selected decision point
      selected_node <- input$current_node_selection
      if (length(selected_node) > 0) {
        selected_dp <- nodes()$label[nodes()$id == selected_node]
        papers_filtered <- papers()[papers()$DecisionPoint == selected_dp, ]
        papers_filtered
      } else {
        # Return an empty data frame if no node is selected
        data.frame()
      }
    }
  }, escape = FALSE, rownames = FALSE, options = list(pageLength = 20, searching = TRUE, ordering = TRUE))
  
#################################
### Contribute ###
#################################
  ###Fetch details of the paper###
  observeEvent(input$fetch, {
    if (nzchar(input$doi)) {
      details <- tryCatch(
        cr_works(dois = input$doi)$data,
        error = function(e) NULL
      )
      if (!is.null(details)) {
        # Extract the author names
        author_names <- map(details$author, ~ paste(.x$given, .x$family)) %>% unlist() %>% paste(collapse = ", ")
        
        updateTextInput(session, "title", value = details$title)
        updateTextInput(session, "authors", value = author_names)
        updateTextInput(session, "journal", value = details$container.title)
        updateTextInput(session, "year", value = details$published.print)
        output$doi_preview <- renderUI({
          tags$iframe(style="height:800px; width:100%; scrolling=yes", src=paste0("https://doi.org/", input$doi))
        })
      }
    }
  })
  
   ###Upload/Submit paper###
   observeEvent(input$submit_paper, {
     # Check if the paper is already in the database
     title_match <- agrepl(input$title, papers()$Title, max = 0.1)
     if (any(title_match)) {
       # If the paper is already in the database, show a dialog box
       matching_papers <- papers()[title_match, ]
       showModal(modalDialog(
         title = "Duplicate Paper",
         paste("The paper is already in the database under the following steps:", 
               paste(matching_papers$DecisionPoint, collapse = ", ")),
         easyClose = TRUE
       ))
     } else {
       # Determine the category folder
       category_folder <- if (input$new_category != "") {
         input$new_category
       } else {
         input$category
       }
       
       # Check if category_folder is not null or empty
       if (is.null(category_folder) || category_folder == "") {
         # If category_folder is null or empty, show a dialog box
         showModal(modalDialog(
           title = "Error",
           "Related Step(s) and the corresponding Group are required. Please input the related Step(s) from the drop-down menu
           or type a new one and select the corresponding Group.",
           easyClose = TRUE
         ))
       } else {
         # Save the uploaded file to the WebDAV server
         if (!is.null(input$file_upload)) {
           # Save the uploaded file to the WebDAV server
           if (grepl("^([A-Za-z0-9_/ -])+$", category_folder)) {
             # Modify the filename to include the category
             filename <- URLencode(paste0(category_folder, "_", input$file_upload$name), reserved = TRUE)
             urlFile <- paste0(dav, "/", filename)
             PUT(urlFile, authenticate(username, password), body = upload_file(input$file_upload$datapath))
             
             # Show a pop-up message
             showModal(modalDialog(
               title = "Success",
               "Paper is uploaded successfully!",
               easyClose = TRUE
             ))
           } else {
             print("Invalid category")
           }
         } else {
           # Show a pop-up message
           showModal(modalDialog(
             title = "Success",
             "Paper is added in the database!",
             easyClose = TRUE
           ))
         }
         
         # Add the new category to the DecisionPoints if it's not empty
         if (input$new_category != "") {
           new_category <- data.frame("", input$new_category, input$new_group, "", "", "")
           names(new_category) <- names(DecisionPoints())
           DecisionPoints(rbind(DecisionPoints(), new_category))
           
           # Save the updated DecisionPoints to the WebDAV server
           urlDP <- file.path(dav, "nodes.csv")
           # Write the DecisionPoints to a CSV file
           write.csv(DecisionPoints(), "nodes.csv", row.names = FALSE)
           # Upload the CSV file
           PUT(urlDP, authenticate(username, password), body = upload_file("nodes.csv"))
         }
         
         # Determine the category
         category_input <- if (input$new_category != "") {
           input$new_category
         } else {
           input$category
         }
         
         # Add the new paper to your list of papers
         new_papers <- lapply(category_input, function(category) {
           data.frame(
             DecisionPoint = category,
             Authors = input$authors, 
             Year = input$year,
             Title = input$title,  
             Publisher = input$journal,
             DOI = ifelse(startsWith(tolower(trimws(input$doi)), "https://doi.org/"),
                          paste0('<a href="', trimws(input$doi), '" target="_blank">', trimws(input$doi), '</a>'),
                          paste0('<a href="https://doi.org/', trimws(input$doi), '" target="_blank">', trimws(input$doi), '</a>')),
             Summary = input$summary
           )
         })
         
         papers(rbind(papers(), do.call(rbind, new_papers)))
         
         # Save the updated papers dataframe to the WebDAV server
         urlPapers <- paste0(dav, "/papers.csv")
         # Write the papers dataframe to a CSV file
         write.csv(papers(), "papers.csv", row.names = FALSE)
         # Upload the CSV file
         PUT(urlPapers, authenticate(username, password), body = upload_file("papers.csv"))
         
         # Clear the text inputs
         updateTextInput(session, "doi", value = "")
         updateTextInput(session, "authors", value = "")
         updateTextInput(session, "title", value = "")
         updateTextInput(session, "year", value = "")
         updateTextInput(session, "journal", value = "")
         updateTextInput(session, "summary", value = "")
         updateTextInput(session, "category", value = "")
         updateTextInput(session, "new_category", value = "")
       }
     }
   })
  
################################################################################
}
