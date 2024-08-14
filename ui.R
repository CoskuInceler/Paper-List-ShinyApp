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
library(shinyalert)
library(visNetwork)
library(rcrossref)

shinyWidgets::shinyWidgetsGallery()

source("Source_function.r")

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(
  
##########################
### Design Layout ###
##########################
  title = div(
    img(src = "UOL_blue.png", height = "50px", style = "margin-right: 20px;"), 
    img(src = "metarep.jpg", height = "50px")
  ),
  id = "navBar",
  theme = "bootstrap.css",
  collapsible = TRUE,
  inverse = TRUE,
  windowTitle = "METEOR - fMRI Literature Base App",
  position = "fixed-top",
  header = tags$style(
    HTML("
      .navbar {
        min-height: 70px; /* Adjust this value to increase the height */
        background-color: #f8f9fa; /* Light grey color */
      }
      .navbar-brand {
        height: 70px; /* Adjust this value to match the navbar height */
        padding: 10px 15px; /* Adjust padding to center the images vertically */
      }
      .navbar-nav > li > a {
        padding-top: 25px; /* Adjust this value to center the text vertically */
        padding-bottom: 25px; /* Adjust this value to center the text vertically */
        color: #007bff; /* Blue color for the text */
        font-size: 14px; /* Increase font size */
        font-weight: bold; /* Make text bold */
      }
      .navbar-nav > li > a:hover {
        color: grey !important; /* Darker blue color for the text on hover */
      }
      body {
        padding-top: 100px;
      }
    ")
  ),
  tags$head(
    tags$style(HTML('
      .navbar-nav { float: right !important; }
      .hidden-tab { display: none !important; }
      .custom-home-icon {
        cursor: pointer;
        padding: 25px;
        display: flex;
        align-items: center;
      }
      .custom-home-icon i {
        margin-right: 5px;
      }
      .custom-home-icon span {
        font-weight: bold;
        color: dark blue; /* Blue color for the text */
        font-size: 14px; /* Increase font size */
      }
      .navbar-nav > li > a:hover {
        color: grey !important; /* Darker blue color for the text on hover */
      }
      body {
        padding-top: 100px;
      }
    ')),
    tags$script(HTML('
      $(document).ready(function() {
        $(".navbar-nav > li").addClass("hidden-tab");
        $(".navbar-nav > li:last-child").removeClass("hidden-tab");
        
        // Add custom home icon with text
        $(".navbar-nav").prepend(\'<li class="custom-home-icon"><i class="fa fa-home"></i><span>HOME</span></li>\');
        
        // Click event for custom home icon
        $(".custom-home-icon").click(function() {
          var homeTab = $(".navbar-nav > li > a[data-value=\'home\']");
          homeTab.click();
        });
      });
    '))
  ),

##########################
### Home ###
##########################       
  tabPanel("Home", value = "home",  icon = icon("home"),
           shinyjs::useShinyjs(),
           tags$head(tags$script(HTML('
      function fakeClick(tabName) {
        var dropdownList = document.getElementsByTagName("a");
        for (var i = 0; i < dropdownList.length; i++) {
          var link = dropdownList[i];
          if(link.getAttribute("data-value") == tabName) {
            link.click();
          }
        }
      }

      var sendToShiny = function(label) {
        Shiny.onInputChange("node_clicked", label);
      };
    '))),
           
           fluidRow(
             align = "left",
             column(
               width = 12,
               wellPanel(
                 style = "background-color: #f0f0f0; color: #000; padding: 10px; border: 1px solid #ddd;",
                 shiny::tags$h1("App Information", style = "color: #000;"),
                 shiny::tags$h2("Welcome!", style = "color: #000;"),
                 shiny::tags$h3("This application provides a platform to explore and share articles related to the analytical decisions in fMRI
                                preprocessing and analysis. When preprocessing and analyzing fMRI data, researchers face a number of analytical decisions (e.g., 
                                whether to use global signal regression). Thhis app collect and show a collection of articles that empirically studied the effect
                                of analytical decisions in every step. The aim is that researchers are able to find empirical evidence of the analytical decisions
                                they want to explore or impelement.", style = "color: #000; font-weight: normal;"),
                 shiny::tags$h2("How to use the app", style = "color: #000;"),
                 shiny::tags$h3("The application uses a tab-based navigation system. You can navigate between different sections of the
                                application (Home, TheSpace, Contribute, About). TheSpace tab provides a network graph visualization of the preprocessing and analysis steps. 
                                Clicking on a node in the graph will display the articles associated with the step in a table format and a brief definition of the step. 
                                Contribute tab provides a platform where you can contribute by providing articles related
                                the steps.", style = "color: #000; font-weight: normal;"),
                 shiny::tags$h3("Click on the node below to explore the features of this app:", style = "color: #000;"),
                 shiny::HTML("<br>")
               )
             )
           ),
           
           fluidRow(
             column(8,
                    visNetworkOutput("network_home", width = "100%", height = "600px")
             ),
             column(4,
                    align = "left",
                    uiOutput("node_description")  # Move this line here
             )
           )
  ),

##########################
### The Space ###
##########################
tabPanel(
  "The Space", value = "TheSpace",
  shiny::HTML("<h1>The Space: A Network of Preprocessing and Analysis Steps</h1>"),
  fluidRow(
    column(12, 
           shiny::HTML("<h3><justify-align>The network visualizes the preprocessing and analysis steps identified from graph-based fMRI studies. 
                                Note that the steps are grouped into several groups. To find a specific step, first
                                select the group of the step. The step can be found on one of the node in the graph.
                                Clicking a node or step will show a brief definition of the step
                                and a collection of empirical studies related to the step. Please also note that you can zoom in
                                on the network and move the nodes for better visibility.</justify-align></h3>"),
    ),
    column(6, 
           uiOutput("DropDownGroup"),
           br(),
           actionButton("show_all", "Show all articles"),
           br(),
           br(),
           visNetworkOutput("network")
    ),
    
    column(6, 
           textOutput("Definition"), 
           DT::dataTableOutput("articles_table")
    )
  )
),

##########################
### Contribute ###
##########################
tabPanel(
  "Contribute", value = "Upload",
  shiny::HTML("<h1>Contribute here!</h1>"),
  fluidRow(
    column(12, 
           shiny::HTML("<h3>To contribute, insert the DOI of the article and click the Fetch Details button. The information of the article, including authors, title, year, and journal
                                will be completed automatically. Next, select the steps that are addressed in the article. Note than you can select multiple steps. 
                                If the step is not included in the list of step form the drop-down menu, you can enter a new step by typing it and select the group where for the step.
                                Please note that the related Step(s), either new or existing, will be displayed in the text box below the drop-down menu, are required.
                                You have an option to upload the PDF file of the article. 
                                Click the Submit button to submit the article in the database (and upload the article). If the article already exists in the database, a dialog box will appear 
                                notifying you of the duplication and informing you in which steps the article has been assigned.</h3>"),
    ),
    column(width = 6,
           ## Show box for uploading files
           textInput("doi", "Enter DOI"),
           actionButton("fetch", "Fetch Details"),
           ## Details of the article
           textInput("title", "Title"),
           textInput("authors", "Authors"),
           textInput("year", "Year of Publication"),
           textInput("journal", "Journal Name"),
           textInput("summary", "Summary of the article"),
           ## select category for the file
           uiOutput("DropDownStep"),
           verbatimTextOutput("selected_categories"),
           ## Add new category
           textInput("new_category", "New Step"),
           conditionalPanel(
             condition = "input.new_category.length > 0",
             uiOutput("DropDownGroup2"),
           ),
           fileInput("file_upload", "Choose PDF file", accept = c(".pdf")),
           actionButton("submit_paper", "Submit the paper details")
    ),
    column(width = 9,
           uiOutput("doi_preview")
    )
  )
),

##########################
### About ###
##########################                   
tabPanel("About", value = "about",  icon = icon("info-circle"),
         
         column(1),
         column(10,
                shiny::HTML("<h3>The METEOR project is based at the University of Oldenburg funded 
                  by priority program <a href='https://www.meta-rep.uni-muenchen.de'> META-REP</a> 
                  (SPP 2317). Meta-REP involves 15 individual projects with 50+ scholars analyzing 
                              and optimizing replicability in the Behavioral, Social, and Cognitive Sciences.</h3><br>
                              <h2><center>Our team</center></h2><br>")
         ),
         
         # TEAM BIO
         fluidRow(
           
           style = "height:50px;"),
         
         fluidRow(
           column(2),
           
           # Andrea
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px",
                          align = "center",
                          div(
                            tags$img(src = "andrea2.jpg",
                                     width = "45px", height = "57px")
                          ),
                          div(
                            tags$h5("Hildebrandt, Andrea, Prof. Dr. rer. nat."),
                            tags$h6( tags$i("Project Investigator"))
                          ),
                          div(
                            "Professor for Psychological Methods and Statistics, Carl von Ossietzky Universitat Oldenburg."
                          )
                      )
                  )
           ),
           # Stefan
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px",
                          align = "center",
                          div(
                            tags$img(src = "stefan.jpg",
                                     width = "50px", height = "70px")
                          ),
                          div(
                            tags$h5("Debener, Stefan, Prof. Dr. rer. nat."),
                            tags$h6( tags$i("Project Investigator"))
                          ),
                          div(
                            "Professor for Neuropsychology, Carl von Ossietzky Universitat Oldenburg."
                          )
                      )
                  )
           ),
           # Carsten
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px",
                          align = "center",
                          div(
                            tags$img(src = "carsten.jpg",
                                     width = "50px", height = "50px")),
                          div(
                            tags$h5("Giessing, Carsten, Dr. rer. nat."),
                            tags$h6( tags$i("Project Investigator"))
                          ),
                          div(
                            "Senior Scientist in Biological Psychology, Carl von Ossietzky Universitat Oldenburg."
                          )
                      )
                  )
           ),
           
           # Christiane
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px",
                          align = "center",
                          div(
                            tags$img(src = "christiane2.jpg",
                                     width = "58px", height = "60px")),
                          div(
                            tags$h5("Thiel, Christiane, Prof. Dr. rer. nat."),
                            tags$h6( tags$i("Project Investigator"))
                          ),
                          div(
                            "Professor for Biological Psychology, Carl von Ossietzky Universitat Oldenburg."
                          )
                      )
                  )
           ),
           column(2)
           
         ),
         
         fluidRow(
           column(2),
           # Nadine
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px",
                          align = "center",
                          div(
                            tags$img(src = "nadine.jpg",
                                     width = "40px", height = "55px")),
                          div(
                            tags$h5("Jacobsen, Nadine, Dr. rer. nat."),
                            tags$h6( tags$i("Postdoctoral Fellow"))
                          ),
                          div(
                            "Carl von Ossietzky Universitat Oldenburg."
                          )
                      )
                  )
           ),
           # Daniel
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px",
                          align = "center",
                          div(
                            tags$img(src = "Daniel.jpg",
                                     width = "40px", height = "50px")),
                          div(
                            tags$h5("Kristanto, Daniel, PhD."),
                            tags$h6( tags$i("Postdoctoral Fellow"))
                          ),
                          div(
                            "Carl von Ossietzky Universitat Oldenburg."
                          )
                      )
                  )
           ),
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px",
                          align = "center",
                          div(
                            tags$img(src = "cassie.jpg",
                                     width = "50px", height = "68px")),
                          div(
                            tags$h5("Short, Cassie, PhD."),
                            tags$h6( tags$i("Postdoctoral Fellow"))
                          ),
                          div(
                            "Carl von Ossietzky Universitat Oldenburg."
                          )
                      )
                  )
           ),
           column(2,
                  div(class="panel panel-default",
                      div(class="panel-body",  width = "600px",
                          align = "center",
                          div(
                            tags$img(src = "micha.jpg",
                                     width = "50px", height = "68px")),
                          div(
                            tags$h5("Burkhardt, Micha Msc."),
                            tags$h6( tags$i("Doctoral student"))
                          ),
                          div(
                            "Carl von Ossietzky Universitat Oldenburg."
                          )
                      )
                  )
           ),
           column(10,
                  shiny::HTML("<h3><center>For questions and suggestion, please contact us at: cosku.inceler@uni-oldenburg.de </center></h3><br>")
           ),
         ),
         fluidRow(style = "height:150px;")
)
##############################
)
)
