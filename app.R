if (Sys.info()[["sysname"]] == "Darwin") {
  options(browser = "open")
} else if (.Platform$OS.type == "windows") {
  options(browser = "shell.exec")
} else {
  options(browser = Sys.which("xdg-open"))
}

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(bslib)
library(leaflet)
library(data.table)
library(tidyverse)
library(rgbif)
library(htmlwidgets)
library(leaflet.extras)
library(sf)
library(wk)
library(geojsonio)
library(DT)
library(reticulate)
library(future)
library(promises)
library(digest)
library(geojsonsf)

source("functions/download_silva.R")
source("functions/import_custom_fasta.R")
source("functions/download_bold.R")
source("functions/download_ncbi.R")
source("functions/download_gbif.R")
source("functions/create_database.R")
source("functions/database_comparison.R")
source("functions/database_results.R")
source("functions/wkt_polygon.R")
source("functions/longtermdata.R")
source("functions/updatetaxonomy.R")
source("functions/blastprep.R")
source("functions/runblast.R")
source("functions/blastresults.R")
source("functions/updatelongterm.R")
source("functions/blastvisualisations.R")
source("functions/detection_controls.R")
source("functions/detection_replication.R")
source("functions/species_differentiation.R")

options(shiny.maxRequestSize = 500 * 1024^2)

docker_mode <- Sys.getenv("DOCKER", "FALSE") == "TRUE"

if (docker_mode) {
  options(shiny.launch.browser = FALSE)
} else {
  options(shiny.launch.browser = TRUE)
}

 ui <- fluidPage(
   tags$style(HTML("
  html, body {
    min-height: 100vh;
    width: 100%;
    margin: 0;
    padding: 0;
    background: radial-gradient(circle at top, #4EC9E3, #0C0A26) !important;
    background-attachment: fixed;
    overflow-x: hidden;
  }
  .content-wrapper {
    min-height: 100vh;
    width: 100%;
    padding-bottom: 50px;
  }
  .main-header, .main-sidebar, .main-footer {
    background: transparent !important;
  }
")), 
  
   tags$head(
     tags$style(HTML("
    .nav-tabs {
      display: flex;
      justify-content: center;
      border-bottom: 2px solid #ddd;
    }
    .nav-tabs > li > a {
  color: #254550 !important;
    }
   
    "))
 ),
 
 tags$style(HTML("
   #metadata_source .shiny-options-group label,
    #metadata_source .shiny-options-group .radio label,
    #metadata_source .radio label,
    #metadata_source .radio-inline,
    #metadata_source .shiny-options-group .checkbox label,
    #metadata_source .form-check-label {
      color: white !important;
      font-size: 16px !important;
      font-weight: 600 !important;
      margin-right: 15px !important;
      line-height: 1.2 !important;
    }

    /* enlarge the radio circle */
    #metadata_source input[type='radio'] {
      transform: scale(1.35) !important;
      margin-right: 6px !important;
      vertical-align: middle !important;
    }

    /* fallback for Bootstrap 4/5 markup */
    #metadata_source .form-check-input,
    #metadata_source .form-check-label {
      color: white !important;
      font-size: 20px !important;
    }
  ")),
 
 tags$style(HTML("
  #historic_plot {
    background: none !important;  /* Make sure the plot is transparent */
    margin-top: 10px; /* Adjust the value as needed */
  }
  
  .panel-body {
    background: none !important;  /* Remove Bootstrap's default white */
  }

  .well {
    background-color: transparent !important;
    border: none !important;
  }
  
  .shiny-plot-output {
    background: none !important; /* Force transparency on plotOutput */
  }
")),
 
 
 tags$style(HTML("
  .sidebarPanel {
    color: white !important;
  }
  
  .control-label {
    color: white !important;
  }

  .form-control {
    background-color: #254550 !important; /* Dark background for dropdowns */
    color: white !important;
  }
  
  .selectize-input {
    background-color: #254550 !important;
    color: white !important;
  }
")),
 
 
 tags$style(HTML("
  /* Change slider text color and font size */
  .irs-single, .irs-bar-edge, .irs-bar {
    background: #b7f09b !important; /* Change slider bar color */
    border-color: #b7f09b !important;
  }
  
  .irs-line {
    background: #444 !important; /* Change track background */
  }
  
  .irs-grid-text {
    font-size: 16px !important; /* Increase grid text size */
    color: white !important;
  }

  .irs-min, .irs-max, .irs-from, .irs-to, .irs-single {
    color: blank !important;
    font-size: 18px !important; /* Bigger text for range labels */
    display: none !important; /* Hide min and max labels */
  }
")), 
 
 tags$style(HTML("
  #query_mode label {
    color: white !important;
    font-size: 16px; /* Adjust size if needed */
  }
")),
 
 tags$style(HTML("
  #species_summary {
    background-color: transparent !important;
    border: none !important;
    color: white; /* Change text color if needed */
  }
")),
 
tags$style(HTML("
    /* Change background of editable cells when editing */
    .dataTable input {
      background-color: #254550 !important;  /* dark gray */
      color: white !important;               /* white text */
      border: none !important; 
      outline: none !important;              /* remove blue focus ring */
      box-shadow: none !important;  
    }
  ")),

tags$style(HTML("
    .custom-link {
      color: #00ffcc;
      font-weight: 600;
    }
    .custom-link:hover {
      color: #ffffff;
      text-decoration: underline;
    }
  ")),

tags$style(HTML("
    .vis-panel {
      color: white;
      font-size: 20px;
      margin-top: 20px;
    }

    .vis-scroll {
      overflow-x: auto;
      overflow-y: auto;
      max-height: 800px;
    }
  ")),
 
 tags$style(HTML("
  #wkt_output {
    color: white !important; 
    background: transparent !important;
    font-size: 16px;
    white-space: pre-wrap; /* Ensures line breaks are respected */
    padding: 10px;
    border: none !important; 
    word-wrap: break-word;  /* Allows long words to break and fit in the container */
    max-width: 100%;  /* Ensures it fits the container width */
    overflow-wrap: break-word; /* Word wrapping for long text */
  }
")),
 
 
  useShinyjs(),
  
  tabsetPanel(id = "tabs",
    # Home Tab
    tabPanel("Home", value = "home",
             fluidRow(
               column(12, align = "center",
                      div(style = "position: relative; width: 800px; height: 600px; margin: auto;",
                          
                          img(src = "KAUST_logo_transparent.png", height = "150px", 
                              style = "position: absolute; top: 40%; left: 52%; transform: translate(-50%, -50%); border-radius: 10px;"),
                            
                          img(src = "BEM_logo2.png", height = "140px", 
                              style = "position: absolute; top: 100%; left: -15%; transform: translate(-50%, -50%); border-radius: 10px;"),
                          
                          # Central Box
                          actionButton("central", "BIOWATCH", 
                                       style = "position: absolute; top: 60%; left: 50%; transform: translate(-50%, -50%);
                                    width: 275px; height: 75px; font-size: 48px; background-color: #2F4E7D; color: white; border: none; outline: none; box-shadow: 2px 2px 7.5px rgba(0,0,0,0.5);"),
                          
                          # Four Surrounding Clickable Boxes
                          actionButton("btn1", "Run BLAST", 
                                       style = "position: absolute; top: 25%; left: 10%; transform: translate(-50%, -50%);
                                    width: 275px; height: 50px; font-size: 24px; background-color: #b0dce9; color: white; border: none; outline: none;"),
                          
                          actionButton("btn2", "Detection Results", 
                                       style = "position: absolute; top: 25%; left: 90%; transform: translate(-50%, -50%);
                                    width: 275px; height: 50px; font-size: 24px; background-color: #b0dce9; color: white; border: none; outline: none;"),
                          
                          actionButton("btn3", "Long Term Data", 
                                       style = "position: absolute; top: 80%; left: 10%; transform: translate(-50%, -50%);
                                    width: 275px; height: 50px; font-size: 24px; background-color: #b0dce9; color: white; border: none; outline: none;"),
                          
                          actionButton("btn4", "Database Construction", 
                                       style = "position: absolute; top: 80%; left: 90%; transform: translate(-50%, -50%);
                                    width: 275px; height: 50px; font-size: 24px; background-color: #b0dce9; color: white; border: none; outline: none;"),
                          
                          actionButton("btn5", "Learn More", 
                                       style = "position: absolute; top: 100%; left: 50%; transform: translate(-50%, -50%);
                                    width: 150px; height: 40px; font-size: 16px; background-color: #d2d2d2; color: white; border: none; outline: none;")
                      )
               )
             )
    ),
    
    tabPanel("Run BLAST", value = "blast",
             fluidPage(
               div(
                 style = "display: flex; justify-content: space-between; gap: 20px; min-height: 400px;",
                 
                 # Left Section
                 div(
                   style = "flex: 1; margin-top: 50px; margin-left: 100px;",
                   div(
                     style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 5px;",
                     "Upload Query File (FASTA)"
                   ),
                   div(style = "margin-bottom: 110px;",
                       fileInput("query_file", NULL, accept = c(".fasta"))
                   ),
                   div(style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 5px;",
                       "Select Database"
                   ),
                   selectInput("db_choice", NULL, choices = c("Loading..." = "")),
                   div(
                     style = "flex: 1; display: flex; align-items: center; justify-content: center; margin-top: 70px;",
                     img(src = "BEM_logo2.png", height = "140px", style = "border-radius: 10px;")
                   )
                 ),
                 
      
                 # Centre Section — KAUST logo top, Max Target Seqs nudged down
                 div(
                   style = "flex: 1; display: flex; flex-direction: column; align-items: center; margin-top: 50px;",
                   img(src = "KAUST_logo_transparent.png", height = "180px", style = "border-radius: 10px;"),
                   div(
                     style = "width: 100%; margin-top: 15px; margin-left: 300px;",
                     div(style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 5px;",
                         "Max Target Sequences"
                     ),
                     numericInput("max_target_seqs", NULL, value = 10, min = 1, max = 100, step = 1)
                   )
                 ),
                 
                 # Right Section
                 div(
                   style = "flex: 1; margin-top: 50px; margin-left: 95px;",
                   div(style = "color: white; font-size: 20px; margin-bottom: 110px;",
                       fileInput("table_file", "Upload ASV Table (Optional)", accept = c(".txt", ".csv"))
                   ),
                   div(style = "color: white; font-size: 20px; white-space: nowrap;",
                       fileInput("coord_file", "Upload Coordinate File (Optional)", accept = c(".txt", ".tsv"))
                   )
                 )
               ),
               
               # Action Button
               div(
                 style = "position: relative; height: 150px;",
                 actionButton("run_blast", "Run BLAST",
                              style = "position: absolute; top: 10%; left: 50%; transform: translate(-50%, -50%);
                          width: 250px; height: 75px; font-size: 36px; background-color: #2F4E7D; color: white;
                          border: none; outline: none; box-shadow: 2px 2px 7.5px rgba(0,0,0,0.5);")
               ),
               
               # Download Buttons
               div(
                 style = "position: relative; height: 200px; text-align: center;",
                 h4("Download Example Files", style = "font-size: 20px; color: white;"),
                 div(
                   style = "position: absolute; top: 25%; left: 50%; transform: translate(-50%, -50%); display: flex; gap: 20px;",
                   downloadButton("download_18S", "18S FASTA", style = "width: 210px; font-size: 16px; background-color: #d2d2d2; color: black;"),
                   downloadButton("download_COI", "COI FASTA", style = "width: 210px; font-size: 16px; background-color: #d2d2d2; color: black;"),
                   downloadButton("download_18S_table", "18S ASV Table", style = "width: 210px; font-size: 16px; background-color: #d2d2d2; color: black;"),
                   downloadButton("download_18S_coord", "18S Coordinates", style = "width: 210px; font-size: 16px; background-color: #d2d2d2; color: black;"),
                   downloadButton("download_COI_table", "COI ASV Table", style = "width: 210px; font-size: 16px; background-color: #d2d2d2; color: black;"),
                   downloadButton("download_COI_coord", "COI Coordinates", style = "width: 210px; font-size: 16px; background-color: #d2d2d2; color: black;")
                 )
               )
             )
    ),

    # Results Tab
    tabPanel("Detection Results", value = "soiresults",
             tabsetPanel(
             tabPanel("Species Detection",
             fluidRow(
               column(3, 
                      # Sidebar Panel for filtering
                      wellPanel(
                        div(style = "color: white; font-size: 20px; white-space: nowrap; ",
                        h4("Filter BLAST Settings")),
                        
                        
                        tags$div(
                          style = "font-size: 18px; color: white;",
                          "Percentage Identity (%): ", 
                          textOutput("slider_pident_value", inline = TRUE)
                        ),
                        
                        sliderInput("pident_filter", "", min = 95, max = 100, value = c(97, 100)),
                        
                        
                        tags$div(
                          style = "font-size: 18px; color: white;",
                          "Sequence Coverage (%): ", 
                          textOutput("slider_length_value", inline = TRUE)
                        ),
                        
                        sliderInput("length_filter", "", min = 90, max = 100, value = c(90, 100)),
                        
                        tags$script(HTML("
    $(document).on('input', 'input[type=range]', function() {
      let val = $(this).val();
      Shiny.setInputValue('slider_live_value', val, {priority: 'event'});
    });
  ")),
                      
                        img(src = "KAUST_logo_transparent.png", height = "135px", 
                            style = "position: absolute; top: 130%; left: 50%; transform: translate(-50%, -50%); border-radius: 10px;"),
                        img(src = "BEM_logo2.png", height = "120px", 
                            style = "position: absolute; top: 180%; left: 50%; transform: translate(-50%, -50%); border-radius: 10px;"),
                      )
               ),
               column(9,
                      fluidRow(
                        column(6, downloadButton("download_full_result", "Download Full Blast Results", style = "margin-top: 40px; margin-bottom: 20px;")),
                        column(6, downloadButton("download_filtered_result", "Download Filtered Blast Results", style = "margin-top: 40px; margin-bottom: 20px;"))
                        ),
                      div(style = "color: white; font-size: 20px; white-space: nowrap; ",
                      h3("Likely Species of Interest detection"),
                      p("The best similarity match was found for only a single Species of Interest species. Similarity scores are denoted in brackets.")),
                      uiOutput("soi_species_boxes_likely"),
                      div(style = "color: white; font-size: 20px; white-space: nowrap; ",
                      h3("Putative Species of Interest detection"),
                      p("Best similarity match is shared by multiple species. Similarity scores are denoted in brackets. Further investigation is required.")),
                      uiOutput("soi_species_boxes_potential"),
                      div(
                        style = "position: fixed;
      bottom: 10px;
      left: 0;
      width: 100%;
      text-align: center;
      color: white;
      z-index: 1000;
      pointer-events: none;",
                        
                        div(style = "font-size: 16px;", "Disclaimer:"),
                        div(style = "font-size: 14px;",
                            "We are not responsible for incompleteness of the reference database and undiscovered/undetected species"),
                        div(style = "font-size: 14px;",
                            "Please assess completion of the databases through the database creation step"),
                        div(style = "font-size: 14px;",
                            "This website is free and open to all users")
                      )
             )
             ) 
    ),
    tabPanel(
      "Controls", value = "control_res",
      fluidPage(
        div(style = "color: white; white-space: nowrap; ",
        h3("Species of Interest in the Controls")),
        uiOutput("download_controls_table_ui"),
        tags$ul(
          tags$li(style = "color: #d4edda;", "Green: Not present in controls"),
          tags$li(style = "color: #deabaf;", "Red: Detected in controls")),
        uiOutput("species_cards_controls")
      )
    ),
    tabPanel(
      "Replication", value = "replication_res",
      fluidPage(
        div(style = "color: white; white-space: nowrap; ",
        h3("Detection Heatmap (Replicate-Based Confidence)")),
        sliderInput("abundance_threshold", 
                    "Minimum Abundance Threshold:",
                    min = 0, max = 500, value = 0, step = 10),
        selectInput(
          "soi_grouping",
          "Summarise Species of Interest counts by:",
          choices = c("Site", "Region"),
          selected = "Site",
          width = "250px"
        ),
        uiOutput("download_replication_table_ui"),
        tags$ul(
          tags$li(style = "color: #d4edda;", "Green: Species of Interest observed in > 75% of replicates (requires at least 3 replicates)"),
          tags$li(style = "color: #FAC589;", "Orange: Species of Interest observed in between 50 and 75% of replicates or there are 2 or fewer replicates"),
          tags$li(style = "color: #deabaf;", "Red: Species of Interest found in less than 50% of repliactes")
        ),
        uiOutput("heatmap_ui")
      )
    ),
    tabPanel("Diversity",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "soi_grouping",
                   "Summarise Species of Interest counts by:",
                   choices = c("Site", "Region"),
                   selected = "Site",
                   width = "250px"
                 ),
                 div(style = "color: white; font-size: 20px; ",
                 p("# of ASVs for Species of Interest ASV at each site."),
                 p("Bubbles are scaled by number of ASVs. Click on site to see values"))
               ),
               mainPanel(
                 div(
                   class = "vis-panel",
                   div(
                     class = "vis-scroll",
                     leafletOutput("soi_map", height = "500px")
                   )
                 )
               )
             )
    ),
    
    
    tabPanel("Distribution",
              sidebarLayout(
                sidebarPanel(
                  div(style = "color: white; font-size: 20px; white-space: nowrap; ",
                  selectInput("species_choice", "Select Species of Interest:", choices = NULL, selected = NULL)
                  ),
                  
                  div(style = "color: white; font-size: 20px; ",
                  p("The points are color-coded to indicate the presence or absence of species of interest in the provided data."),
                  p("Purple points indicate locations where species of interest are present, and grey circles represent locations where they were not present."),
                  p("Click on points for further information on sample and site name and read counts.")
                  ),
                  img(src = "KAUST_logo_transparent.png", height = "135px", 
                      style = "position: absolute; top: 120%; left: 50%; transform: translate(-50%, -50%); border-radius: 10px;"),
                  img(src = "BEM_logo2.png", height = "120px", 
                      style = "position: absolute; top: 155%; left: 50%; transform: translate(-50%, -50%); border-radius: 10px;"),
                ),
             
                mainPanel(
                  div(style = "color: white; font-size: 24px; white-space: nowrap; ",
                  h3("Species distribution")
                  ),
                  leafletOutput("map", height = 500)
              )
              )
    ),
      # Display historic data table
  
    tabPanel("Long Term Data Management",
             fluidRow(
               column(12,
                      div(style = "margin-top: 20px; text-align: center;",
                      h3("Select species of interest for Historic Data", style = "color: white; font-size: 24px;"))
               )
             ),
             fluidRow(
               column(6,
                      div(style = "padding: 15px; border-radius: 5px;",
                          h4("Likely Species of Interest", style = "color: #9b99cd; font-size: 24px;"),
                          div(style = "color: white;",
                              uiOutput("select_likely_species_ui")
                          )
                      )
               ),
               column(6,
                      div(style = "padding: 15px; border-radius: 5px; ",
                          h4("Putative Species of Interest", style = "color: #98C7B1; font-size: 24px;"),
                          div(style = "color: white;",
                              uiOutput("select_potential_species_ui")
                          )
                      )
               )
             ),

             fluidRow(
               column(12,
                      div(style = "margin-top: 20px; text-align: center; font-size: 24px;",
                          radioButtons("metadata_source", "Metadata Source:",
                                       choices = c("Manual entry" = "manual",
                                                   "Upload file" = "upload"),
                                       selected = "manual",
                                       inline = TRUE),
                          
                          conditionalPanel(
                            condition = "input.metadata_source == 'manual'",
                            fluidRow(
                              column(
                                4,
                                div(
                                  style = "display: flex; align-items: center; color: white; font-size: 18px; gap: 10px;",
                                  tags$label("Year:", `for` = "year_fill", style = "margin: 0;"),
                                  textInput("year_fill", NULL, value = "", width = "33%")
                                )
                              ),
                              column(
                                4,
                                div(
                                  style = "display: flex; align-items: center; color: white; font-size: 18px; gap: 10px;",
                                  tags$label("Gene:", `for` = "gene_fill", style = "margin: 0;"),
                                  textInput("gene_fill", NULL, value = "", width = "33%")
                                )
                              ),
                              column(
                                4,
                                div(
                                  style = "display: flex; align-items: center; color: white; font-size: 18px; gap: 10px;",
                                  tags$label("Methodology:", `for` = "methodology_fill", style = "margin: 0;"),
                                  textInput("methodology_fill", NULL, value = "", width = "33%")))
                            )
                          ),
                          
                          conditionalPanel(
                            condition = "input.metadata_source == 'upload'",
                            div(style = "color: white; font-size: 16px;",
                            fileInput("metadata_file", "Upload metadata CSV:", accept = ".csv"))
                          ),
                          div(style = "margin-top: 20px;",
                          actionButton("generate_editable_table", "Generate Editable Table", 
                                       class = "btn-primary", 
                                       style = "font-size: 24px; padding: 10px 30px; background-color: #2F4E7D; border: none; outline: none;"))
                      )
               )
             ),
             fluidRow(
               column(12,
                      div(style = "margin-top: 20px;",
                          h4("Editable P/A Table with Metadata", style = "color: white;"),
                          DTOutput("editable_pa_table"),
                          downloadButton("download_pa_table", "Download P/A Table", 
                                         style = "margin-right: 10px;")
                      )
               )
             ),
             fluidRow(
               column(12,
                      div(style = "margin-top: 20px; text-align: center;",
                          
                          actionButton("add_to_historic", "Add to Historic Data", 
                                       class = "btn-success",
                                       style = "font-size: 24px; padding: 10px 30px; background-color: #b0dce9; border: none; outline: none;")
                      )
               )
             )
    ))),
    
    tabPanel("Long Term Data", value = "historic",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("historic_species_select"),
                 uiOutput("historic_gene_select"),
                 uiOutput("historic_method_select"),
                 uiOutput("historic_year_slider"),
                 
                 
                 div(
                   style = "flex: 1; display: flex; align-items: center; justify-content: center; margin-top: 60px;",
                   img(src = "KAUST_logo_transparent.png", height = "150px", style = "border-radius: 10px;")
                 ),
                 
                 div(
                   style = "flex: 1; display: flex; align-items: center; justify-content: center; margin-top: 40px;",
                   img(src = "BEM_logo2.png", height = "125px", style = "border-radius: 10px;")
                 )
               ),
               mainPanel(
                 leafletOutput("historic_map"),
                 plotOutput("historic_plot", width = "990px", height = "290px")
               )
             )
    ),
    
    tabPanel("Database Construction", value = "construction",
             tabsetPanel(id = "main_tabs",
                         tabPanel("Configuration",
                                  sidebarLayout(
                                    sidebarPanel(
                                      fileInput("species_file", "Upload species list file (one species per line):", 
                                                accept = c(".txt")),
                                      
                                      div(style = "margin-bottom: 20px;",
                                          textInput("amplicon_of_interest", "Enter database name suffix (required):",
                                                    value = "", 
                                                    placeholder = "Enter name for database:")),
                                      
                                      div(style = "margin-bottom: 20px;",
                                          fileInput("custom_fasta", "Upload custom fasta file (optional):", accept = c(".fasta")),
                                          downloadButton("download_custom", "Download example fasta", style = "width: 220px; font-size: 16px; background-color: #d2d2d2; color: black;")
                                      ),
                                      
                                      div(style = "margin-bottom: 20px; color: white;",
                                          checkboxGroupInput("source", "Select Database Source(s):",
                                                             choices = c("NCBI" = "ncbi", 
                                                                         "BOLD" = "bold", 
                                                                         #"MIDORI" = "midori",
                                                                         "SILVA" = "silva"),
                                                             selected = "ncbi",
                                                             inline = TRUE)),
                                      
                                      div(style = "margin-bottom: 20px; color: white;",
                                          textInput("forward_primer", "Forward Primer:",
                                                    value = "", 
                                                    placeholder = "Enter forward primer sequence")),
                                      
                                      div(style = "margin-bottom: 20px; color: white;",
                                          textInput("reverse_primer", "Reverse Primer:",
                                                    value = "", 
                                                    placeholder = "Enter reverse primer sequence")),
                                      
                                      
                                      div(style = "margin-bottom: 20px;",
                                          conditionalPanel(
                                            condition = "input.source.includes('ncbi')",
                                            radioButtons("query_mode", "Query Mode:",
                                                         choices = c("Select options" = "default", "Custom Queries" = "add"), 
                                                         selected = "default",
                                                         inline = TRUE))),
                                      
                                      div(style = "margin-bottom: 20px;",
                                          conditionalPanel(
                                            condition = "(input.source.includes('ncbi')) && input.query_mode == 'default'",
                                            actionButton("add_selection", "Add Selection"),
                                            actionButton("remove_selection", "Remove Last Selection"),
                                            uiOutput("selection_fields"))),
                                      
                                      div(style = "margin-bottom: 20px;",
                                          conditionalPanel(
                                            condition = "input.query_mode == 'add'",
                                            textAreaInput("custom_query", "Enter Custom Query (e.g., COI[gene] OR rbcL[gene]):", 
                                                          value = "", 
                                                          placeholder = "Write your query here...", 
                                                          rows = 4, width = "100%"))),
                                      
                                      div(style = "margin-bottom: 10px;",
                                          conditionalPanel(
                                            condition = "input.source.includes('bold')",
                                            selectInput("bold_marker", "Select BOLD Marker Gene(s):",
                                                        choices = c("COI-5P", "rbcL", "ITS", "matK"),
                                                        selected = "COI-5P"))),
                                      
                                      div(style = "margin-bottom: 10px;",
                                          conditionalPanel(
                                            condition = "input.source.includes('silva')",
                                            selectInput("silva_marker", "Select SILVA Marker Gene(s):",
                                                        choices = c("SSU", "LSU"),
                                                        selected = "SSU"))),
                                      
                                      div(style = "margin-bottom: 10px;",
                                          conditionalPanel(
                                            condition = "input.source.includes('midori')",
                                            selectInput("midori_marker", "Select MIDORI Marker Gene(s):",
                                                        choices = c("A6", "A8", "CO1", "CO2", "CO3", "Cytb", "ND1", "ND2", "ND3", "ND4", "ND5", "ND6", "lrRNA", "srRNA"),
                                                        selected = "CO1")))
                                    ),
                                      
                                    
                                    mainPanel(
                                    
                                      div(style = "display: flex; justify-content: center;", 
                                          actionButton("generate_run", "Generate and Run",
                                                       style = "margin-top: 10px; width: 320px; height: 60px; font-size: 36px; background-color: #2F4E7D; color: white; 
                              border: none; outline: none; box-shadow: 2px 2px 7.5px rgba(0,0,0,0.5);")),
                                    
                                      div(style = "color: white; font-size: 18px; margin-top: 20px;",
                                          p("Construct a polygon encompassing the region of interest to generate WKT coordinates or input previously obtained WKT coordinates into selection box"),
                                      ),
                                      div(style = "margin-bottom: 20px; margin-top: 20px;",
                                          leafletOutput("database_map")),
                                      
                                      div(style = "margin-bottom: 5px;",
                                          tags$style("
                                                     #WKT_input { margin-bottom: 0 !important; }
                                                     #WKT_input-label { margin-bottom: 0 !important; }
                                                     #wkt_output pre { margin-top: 0 !important; }
                                                     "),
                                          textInput("WKT_input", "Enter WKT coordinates", 
                                                    value = "", 
                                                    placeholder = "Enter WKT coordinates of region of study")),
                                      
                                      div(style = "color: white; font-size: 16px; background-color: none;",
                                                    verbatimTextOutput("wkt_output")
                                      )
                                      
                              
                                    )
                                  )
                         ),
                         
                         tabPanel(
                           "Results",
                           
                           conditionalPanel(
                             condition = "output.show_results",
                             
                             # ---- Database Completion ----
                             div(
                               style = "color: white; font-size: 18px; font-weight: bold;",
                               h3("Database Completion")
                             ),
                             
                             verbatimTextOutput("species_summary"),
                             
                             div(
                               downloadButton("download_presence", "Download Presence Data"),
                               downloadButton("download_database_species_list", "Download List of Species in Database"),
                               tags$div(style = "margin-bottom: 30px;")
                             ),
                             
                             div(uiOutput("dynamic_heatmap")),
                             
                             # ---- Generated NCBI Query (NCBI only) ----
                             conditionalPanel(
                               condition = "input.source && input.source.includes('ncbi')",
                               
                               div(
                                 style = "color: white; font-size: 18px; font-weight: bold;",
                                 h3("Generated (NCBI) Query")
                               ),
                               
                               div(
                                 style = "margin-top: 10px; color: white; background-color: none;",
                                 tags$pre(
                                   style = "background: none;
                   color: white;
                   background-color: transparent;
                   border: none;
                   padding: 0;
                   margin: 0;",
                                   verbatimTextOutput("query_output")
                                 )
                               )
                             )
                           )
                         ),
                         
                    tabPanel(
                           "Species Differentiation", value = "differentiation_res",
                           conditionalPanel(
                             condition = "output.show_differentiation",
                             
                             div(
                               style = "color: white; font-size: 18px; font-weight: bold;",
                               h3("Species Differentiation Analysis")
                             ),
                             
                             p("Shows which species have sequences with 100% identity matches to other species in the database",
                               style = "color: white;"),
                             
                             downloadButton("download_differentiation", "Download Differentiation Analysis"),
                             
                             tags$div(style = "margin-bottom: 20px;"),
                             
                             DTOutput("differentiation_table")
                           )
                         ),
                         
              tabPanel("Database Comparison", value = "comparison",
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput(
                                        inputId = "dbcomparison_prefix",
                                        label = "Select database:",
                                        choices = NULL   # will populate dynamically
                                      ),
                                      
                                      uiOutput("download_dbcomparison_table_ui")
                                    ),
                                    mainPanel(
                                      div(style = "height:30px;"),
                                      tags$ul(
                                        tags$li(style = "color: #716fa8; font-size:18px", "Present in database"),
                                        tags$li(style = "color: #d2d2d2; font-size:18px", "Absent in database")),
                                      div(style = "height:10px;"),
                                      plotOutput("dbcomparison_heatmap")
                                    )
                                  )
                         )
           
                         
             )),
    
    # Help Tab
    tabPanel("Learn More", value = "help",
             sidebarLayout(
               sidebarPanel(
                 div(style = "margin-bottom: 20px;",
                     selectInput("info", "Learn More:",
                                 choices = c("Learn More" = "", 
                                             "BLAST and Results" = "learnblast", 
                                             "Long Term Data" = "learnexisting",
                                             "Database Construction" = "learndatabase", 
                                             "Software Requirements" = "learnsoftware",
                                             "Acknowledgements" = "learnacknowledge"), 
                                 selected = "")),
                 div(
                   img(src = "KAUST_logo_transparent.png", height = "150px", 
                       style = "position: absolute; top: 250%; left: 50%; transform: translate(-50%, -50%); border-radius: 10px;")
                 ),
                 
                 div(
                   img(src = "BEM_logo2.png", height = "120px",
                       style = "position: absolute; top: 400%; left: 50%; transform: translate(-50%, -50%); border-radius: 10px;"))
                 
               ),

               
               mainPanel(
                 conditionalPanel(
                   condition = "input.info == 'learnblast'",
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                   h3("BLAST Overview")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                   p("A fasta file is queried against custom databases using blastn (", tags$a(href = "https://doi.org/10.1186/1471-2105-10-421", class = "custom-link", target = "_blank", "Camacho et al 2009"), ") with options options -max_target_seqs 5, -qcov_hsp_perc 80 and -outfmt '6 qseqid sseqid staxids pident length mismatch gapopen qstart qend sstart send evalue bitscore'.")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                   p("Optional ASV and coordinate files can be uploaded to enable further investigation of the results.")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                   p("A ", tags$i("Likely"), " detection is when an ASV's highest bit score corresponds to only one species.")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                   p("A ", tags$i("Putative"), " detection is when  multiple species share the top bitscore for an ASV. This indicates the marker could not distinguish between species and further investigation is required.")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                   p("If a ASV table and coordinate file have been include visualistation of the detections are available. This includes a heatmap showing detection per sample/site/region, a map showing the number of detections per sample/site/region and a map showign where species of interest were detected")
                   ),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                       p("The detection of species of interest in the controls and the frequency of detection in replicates can also be assessed if the ASV table and coordinates are included.")
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.info == 'learnexisting'",
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                   h3("Long Term Data")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                       p("Temporal and spatial trends in the detection of species can be identified in the Long Term Data tab.")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                       p("Results contained in the historic.csv file in the data folder are visualised. This file can be amended with results from data run by the user.")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                       p("The spatial distribution of a species is shown on a map with an summary of temporal detections in a bar chart. Clicking on a site will show detections temporally for that site."))
                   
                 ),
                 
                 conditionalPanel(
                   condition = "input.info == 'learndatabase'",
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                   h3("Database Construction")),
                   
                   
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                       p("The construction of the custom databases is based on the parsing of sequences contained in the National Center for Biotechnology Information (", 
                         tags$a(href = "https://www.ncbi.nlm.nih.gov", class = "custom-link", target = "_blank", "NCBI"),"; ", 
                         tags$a(href = "https://doi.org/10.1093/nar/gkac1012", class = "custom-link", target = "_blank", "Sayers et al 2023"), ") and Barcode of Life Database (BOLD;", 
                         tags$a(href = "https://doi.org/10.1111/1755-0998.13741", class = "custom-link", target = "_blank", "Ratnasingham et. al., 2024"), ") and SILVA (", 
                         tags$a(href = "https://doi.org/10.1093/nar/gkaf1247", class = "custom-link", target = "_blank", "Chuvochina et. al., 2026"), ") public databases")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;", 
                       p("Sequences belonging to the customised list were queried, downloaded, trimmed to the primers and filtered using the software package CRABS; Creating Reference databases for Amplicon-based Sequencing (", tags$a(href = "https://doi.org/10.1111/1755-0998.13741", class = "custom-link", target = "_blank", "Jeunen et al 2023"), ")")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;",    
                       p("A BLAST database was constructed using makeblastdb (", tags$a(href = "https://doi.org/10.1186/1471-2105-10-421", class = "custom-link", target = "_blank", "Camacho et al 2009"), ")")),
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;",    
                       p("A presence/absence heatmap is created (with a downloadable table) to indicate the completeness of the database for that gene/primer")), 
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;",    
                       p("For each species of interest with sequences in the database sequence similarity is assessed with all the other sequences in the databse to assess the ability to differentiate species of interest with the gene/primer set.")), 
                   div(style = "color: white; font-size: 16px; margin-bottom: 40px;",    
                       p("A comparison tab enables completion to be assessed amongst different databases allowing the suitability of different genes/primers to be assessed.")) 
                   
                 ),
                 
                 conditionalPanel(
                   condition = "input.info == 'learnsoftware'",
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                   h3("Software Requirements")),
                   
                   div(style = "color: white; font-size: 16px;", 
                       p("Please reference the software appropriately")),
                   
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                   h4("R packages")),
                   
                   
                   div(style = "color: white; font-size: 16px;", 
                   p("shiny, shinyWidgets, shinyjs, bslib, leaflet, data.table, tidyverse, rgbif, htmlwidgets, leaflet.extras, sf, wk, geojsonio, DT, reticulate, future, promises")),
                   
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                   h4("Other Software")),
                   
                   div(style = "color: white; font-size: 16px;", 
                   p("makeblastdb v 2.9.0+ - A tool from the BLAST+ suite for creating BLAST databases."),
                   p("CRABS; Creating Reference databases for Amplicon-based Sequencing"))
                   
                   
                 ),
                 
                 conditionalPanel(
                   condition = "input.info == 'learnacknowledge'",
                   
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                       h4("App design and development")),
                  
                   div(style = "color: white; font-size: 16px;", 
                   p("Dr. John Pearman - Cawthron Institute"),
                   p("Dr. Susana Carvalho - King Abdullah University of Science and Technology"),
                   p("Dr. Eva Aylagas - King Abdullah University of Science and Technology")),
                   
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                       h4("Example species lists and data")),
                   
                   div(style = "color: white; font-size: 16px; ", 
                       p("Dr. Susana Carvalho - King Abdullah University of Science and Technology"),
                       p("Dr. Eva Aylagas - King Abdullah University of Science and Technology"),
                       p("Example lists and data were based on", tags$a(href = "https://doi.org/10.1002/edn3.583", class = "custom-link", target = "_blank", "Aylagas et al 2024"), ", ", 
                         tags$a(href = "https://doi.org/10.1038/s41598-024-60336-8", class = "custom-link", target = "_blank", "Villalobos et al 2024"), ", ", 
                         tags$a(href = "http://dx.doi.org/10.1098/rspb.2018.2697", class = "custom-link", target = "_blank", "Carvalho et al 2019"), " and ", 
                         tags$a(href = "http://dx.doi.org/10.3389/fmars.2023.1295997", class = "custom-link", target = "_blank", "Pearman et al 2024"))),
                   
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                       h4("Database Construction")),
                   
                   div(style = "color: white; font-size: 16px;", 
                       p("Dr. John Pearman - Cawthron Institute")),
                   
                   div(style = "color: white; font-size: 18px; font-weight: bold; margin-bottom: 20px; margin-top: 20px;", 
                       h4("Funding")),
                   
                   div(style = "color: white; font-size: 16px;", 
                       p("THe development of the app was funded by:")),
                   
                   div(
                     img(src = "KAUST_logo_transparent.png", height = "100px", width = "225px", 
                         style = "position: absolute; top: 130%; left: 15%; transform: translate(-50%, -50%); border-radius: 10px;")
                   ),
                   
                   div(
                     img(src = "Cawthron_logo.png", height = "100px", width = "225px", 
                         style = "position: absolute; top: 130%; left: 50%; transform: translate(-50%, -50%); border-radius: 10px;")
                   )
                   
                 )
               )
             )
    )
    ))


server <- function(input, output, session) {
  
  observeEvent(input$btn1, { updateTabsetPanel(session, "tabs", selected = "blast") })
  observeEvent(input$btn2, { updateTabsetPanel(session, "tabs", selected = "soiresults") })
  observeEvent(input$btn3, { updateTabsetPanel(session, "tabs", selected = "historic") })
  observeEvent(input$btn4, { updateTabsetPanel(session, "tabs", selected = "construction") })
  observeEvent(input$btn5, { updateTabsetPanel(session, "tabs", selected = "help") })
  
  Sys.setenv(PYTHONWARNINGS="ignore::UserWarning")
#Set variables
  crabs_path <- Sys.which("crabs")
  ncbi_email <- Sys.getenv("NCBI_EMAIL")
  
  #Create reavtiveVal's
  genera_rv       <- reactiveVal(NULL)
  species_list_rv <- reactiveVal(NULL)
  query_mode_rv   <- reactiveVal(NULL)
  selected_sources_rv <- reactiveVal(NULL)
  gbif_request_id <- reactiveVal(NULL)
  gbif_results <- reactiveVal(NULL)
  additional_species_rv <- reactiveVal(NULL)
  species_dataframe_rv  <- reactiveVal(NULL)
  cache_path_rv <- reactiveVal(NULL)
  queries <- reactiveVal(list())
  show_results <- reactiveVal(FALSE)
  polygon_wkt <- reactiveVal(NULL)
  blast_results <- reactiveVal("")
  historic_data <- reactiveVal(data.frame())
  error_log    <- reactiveVal(character(0))
  pipeline_failed <- reactiveVal(FALSE)
  show_differentiation <- reactiveVal(FALSE)
  # Helper to log an error message and mark pipeline as failed
  log_error <- function(msg) {
    error_log(c(error_log(), msg))
    pipeline_failed(TRUE)
  }
  
  
  # Auto popup on failure
  observe({
    req(pipeline_failed())
    showModal(modalDialog(
      title = div(icon("triangle-exclamation"), " Pipeline Error",
                  style = "color: #c0392b; font-weight: bold;"),
      verbatimTextOutput("error_output"),
      footer = tagList(
        actionButton("restart_pipeline", "Restart Pipeline",
                     style = "background-color: #c0392b; color: white;
                            font-size: 16px; width: 180px;"),
        modalButton("Dismiss")
      )
    ))
  })
  
  output$error_output <- renderText({
    if (length(error_log()) == 0) return(NULL)
    paste("ERRORS:\n", paste(error_log(), collapse = "\n"))
  })
  
  # Reset handler
  observeEvent(input$restart_pipeline, {
    error_log(character(0))
    pipeline_failed(FALSE)
    removeModal()
    
    # All your reactiveVals listed here
    show_results(FALSE)
    gbif_request_id(NULL)
    gbif_results(NULL)
    species_list_rv(NULL)
    genera_rv(NULL)
    species_dataframe_rv(NULL)
    additional_species_rv(NULL)
    polygon_wkt(NULL)
    query_mode_rv(NULL)
    selected_sources_rv(NULL)
    show_differentiation(FALSE)
    showNotification("Ready to try again.", type = "message", duration = 4)
  })
  
####Check and update taxonomy files####
  #Check if the taxonomy files exist and how old they are
  observeEvent(TRUE, {
    data_dir <- "data"
    res      <- check_taxonomy_files(data_dir)
    #See if files exist
    if (res$status == "missing") {
      showModal(modalDialog(
        title  = "Taxonomy files missing",
        paste("Missing files:", paste(res$missing, collapse = ", ")),
        "They will now be downloaded automatically.",
        footer = NULL
      ))
      download_taxonomy_async(data_dir)
      removeModal()
     #See if they are old than 6 months or not 
    } else if (res$status == "old") {
      showModal(modalDialog(
        title = "Taxonomy files are old",
        paste(
          "Some taxonomy files are older than 6 months:",
          paste(names(res$ages), res$ages, "days", collapse = ", ")
        ),
        "Would you like to update them now?",
        footer = tagList(
          modalButton("Later"),
          actionButton("update_taxonomy", "Update Now")
        )
      ))
    }
  }, once = TRUE)
  
  #If files don't exist or too old then download new versions
  observeEvent(input$update_taxonomy, {
    data_dir <- "data"
    removeModal()
    
    showModal(modalDialog(
      title  = "Updating taxonomy",
      tagList(
        "Downloading taxonomy files. This may take several minutes.",
        br(),
        tags$div(class = "spinner-border text-primary", role = "status")
      ),
      footer = NULL
    ))
    
    download_taxonomy_async(data_dir) %...>% {
      removeModal()
      showNotification("Taxonomy update complete", type = "message", duration = 5)
    } %...!% {
      removeModal()
      showNotification(paste("Taxonomy update failed:", .), type = "error", duration = 10)
    }
  })
  
  
########################################  
####Running BLAST####  
########################################
####Blast prep####
  
  #get available databases
  observeEvent(input$tabs, {
    if (input$tabs == "blast") {
      db_names <- get_available_databases("databases/")
      updateSelectInput(session, "db_choice", choices = db_names)
    }
  })  
  
  species_list_path <- reactive({
    
    if (!is.null(input$db_choice)) {
      db <- input$db_choice
      
      # Remove .blast.db from the basename to get the prefix
      db_prefix <- gsub("\\.blast\\.db$", "", db)
      
      # Construct the SOI synonyms file path
      soi_file <- file.path("databases", paste0(db_prefix, "_synonyms.txt"))
      
      # Return the file path (not the content)
      if (file.exists(soi_file)) {
        return(soi_file)
      } else {
        return(NULL)
      }
    }
    
    # If nothing matches
    return(NULL)
  })
  
  #give text of slider length
  output$slider_length_value <- renderText({
    paste(input$length_filter[1], "to", input$length_filter[2])
  })
  
  #delay reaction by 1 secont
  debounced_length_filter <- reactive({
    input$length_filter
  }) %>% debounce(1000)
  
  
  output$slider_pident_value <- renderText({
    paste(input$pident_filter[1], "to", input$pident_filter[2])
  })
  
  debounced_pident_filter <- reactive({
    input$pident_filter
  }) %>% debounce(1000)
  
####RUN BLAST####

  observeEvent(input$run_blast, {
    req(input$query_file, input$db_choice)
    
    # Clear previous errors on fresh run
    error_log(character(0))
    pipeline_failed(FALSE)
    #Show modal that process is running
    showModal(modalDialog(
      title  = "Processing",
      "Running BLAST... Please wait.",
      footer = NULL
    ))
    #Extract input paths and parameters
    query_path  <- input$query_file$datapath
    db_path     <- file.path("databases", input$db_choice)
    output_file <- tempfile(fileext = ".txt")
    max_target_seqs <- input$max_target_seqs
    #Check the query file exists
    if (!file.exists(query_path)) {
      log_error("Query file is missing or invalid.")
      removeModal(); return(NULL)
    }
    #Check for the existance of database files
    if (!file.exists(paste0(db_path, ".nin"))) {
      log_error("Database files are missing or the path is incorrect.")
      removeModal(); return(NULL)
    }
    #Build blastn command
    blast_command <- build_blast_command(query_path, db_path, output_file, max_target_seqs)
    
    tryCatch({
      #Execute the blast and then merge the taxonomy information
      blast_results.df <- run_blast(blast_command, output_file)
      blast_results.df <- merge_blast_taxonomy(blast_results.df)
      #Store results
      blast_results(blast_results.df)
      #Dismiss modal on success and move to the results tab
      removeModal()
      updateTabsetPanel(session, "tabs", selected = "soiresults")
      
      
    }, error = function(e) {
      #On failure load an error popup and allow restart
      log_error(paste("An error occurred while running BLAST:", e$message))
      removeModal()
    })
  })

  
####Blast results####
  
  #Filter the results based on percentage identity (pident_range) and coverage (length_range)
  filtered_results <- reactive({
    req(blast_results())
    filter_blast_results(
      blast_results = blast_results(),
      pident_range  = debounced_pident_filter(),
      length_range  = debounced_length_filter(),
      species_path  = species_list_path()
    )
  })
  
  #Further filtering based on top bitscores and flag if multiple instances.
  filtered_results2 <- reactive({
    req(filtered_results())
    get_top_bitscore_results(filtered_results())
  })
  
  #Make species boxes for those species that are likely
  output$soi_species_boxes_likely <- renderUI({
    req(filtered_results2())
    
    soi_results <- filtered_results2() %>%
      filter(SOI_flag == TRUE, Multiple_Instances == FALSE)
    
    if (nrow(soi_results) == 0) return(NULL)
    
    showModal(modalDialog(
      title  = "Processing",
      "Loading species boxes (likely)... Please wait.",
      footer = NULL
    ))
    # Build one species box per unique species in the filtered results
    box_list <- lapply(unique(soi_results$sscinames), function(species) {
      build_species_box(species, soi_results)
    })
    
    removeModal()
    do.call(tagList, box_list)
  })
  
  #Make species boxes for those species that are potential
  output$soi_species_boxes_potential <- renderUI({
    req(filtered_results2())
    
    soi_results <- filtered_results2() %>%
      filter(SOI_flag == TRUE, Multiple_Instances == TRUE)
    
    if (nrow(soi_results) == 0) return(NULL)
    
    showModal(modalDialog(
      title  = "Processing",
      "Loading species boxes (Putative)... Please wait.",
      footer = NULL
    ))
    
    box_list <- lapply(unique(soi_results$sscinames), function(species) {
      build_species_box(species, soi_results)
    })
    
    removeModal()
    #Disclaimer
    showModal(modalDialog(
      title = div(icon("triangle-exclamation"), " Important Disclaimer",
                  style = "color: #e67e22; font-weight: bold; font-size: 20px;"),
      div(
        style = "font-size: 15px; line-height: 1.6;",
        p("The BLAST results presented in this application are provided for informational 
         purposes. The results should not be used as the sole basis for regulatory, 
         management, or conservation decisions."),
        p("Species identifications are based on top bitscores after meeting similarity and coverage thresholds and are subject to 
         the limitations of the reference database used. Results may be affected by, amongst others:"),
        tags$ul(
          tags$li("Incomplete or biased reference databases"),
          tags$li("Primer or amplification bias"),
          tags$li("Taxonomic ambiguity between closely related species")
        ),
        p("All results should be interpreted carefully and validated 
         against additional evidence where possible."),
        p(strong("By clicking 'Accept' you acknowledge that you have read and understood 
                this disclaimer."),
          style = "color: #e67e22;")
      ),
      footer    = modalButton("Accept"),
      size      = "m",
      easyClose = FALSE
    ))
    
    do.call(tagList, box_list)
  })
  
####Blast controls####
  
  #Built ASV table for SOI flagged ASVs
  asv_table_filtered <- reactive({
    req(asv_table(), coordinates(), filtered_results2())
    
    detected     <- filtered_results2()
    detected <- detected %>% filter(SOI_flag == TRUE)
    soi_species  <- unique(detected$qseqid)
    
    meta         <- coordinates()
    meta$SampleID <- as.character(meta$SampleID)
    sample_ids   <- meta$SampleID
    
    asv_raw <- asv_table()
    
    keep_cols <- intersect(c("ASV", sample_ids), colnames(asv_raw))
    
    out <- asv_raw %>%
      dplyr::select(all_of(keep_cols)) %>%
      dplyr::filter(ASV %in% soi_species)
    
    out_long <- tidyr::pivot_longer(
      out,
      cols      = tidyselect::all_of(sample_ids),
      names_to  = "SampleID",
      values_to = "Abundance"
    )
    
    out_long$SampleID <- as.character(out_long$SampleID)
    out_long_metadata <- left_join(out_long, meta, by = "SampleID")
    
    seqID_species     <- detected %>% select(qseqid, sscinames)
    out_long_metadata <- left_join(out_long_metadata, seqID_species,
                                   by = c("ASV" = "qseqid"))
    
    out_long_metadata
  })
  
  #Make the controls detection sumamry
  controls_summary_reactive <- reactive({
    req(asv_table_filtered())
    compute_controls_summary(asv_table_filtered())
  })
  
  # Conditionally render the controls table download button
  output$download_controls_table_ui <- renderUI({
    downloadButton("download_controls_table", "Download Table",
                   style = "width:200px; font-size:16px; margin-top:10px;")
  })
  
  # Download the controls summary table as a CSV
  output$download_controls_table <- downloadHandler(
    filename = function() paste0("controls_table_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- controls_summary_reactive()
      req(nrow(df) > 0)
      write.csv(prepare_controls_download(df), file, row.names = FALSE)
    }
  )
  
  #Render species cards for the controls. One card per species which shows teh detection in a heatmap for each ASV. 
  output$species_cards_controls <- renderUI({
    summary_df   <- controls_summary_reactive()
    req(summary_df)
    species_list <- unique(summary_df$sscinames)
    
    tagList(
      lapply(seq_along(species_list), function(i) {
        sp   <- species_list[i]
        asvs <- summary_df %>% filter(sscinames == sp)
        build_control_species_card(sp, asvs, i)
      })
    )
  })
  
  #Render heatmap.
  observe({
    summary_df   <- controls_summary_reactive()
    req(summary_df)
    species_list <- unique(summary_df$sscinames)
    
    for (i in seq_along(species_list)) {
      local({
        my_i <- i
        sp   <- species_list[my_i]
        asvs <- summary_df %>% filter(sscinames == sp)
        
        output[[paste0("control_heatmap_", my_i)]] <- renderPlot({
          plot_control_heatmap(asvs)
        }, bg = "transparent")
      })
    }
  })  
  
  
####Blast replication####
  
  #Compute replication detection summary. Group by Site or Region with default Site
  replicate_summary <- reactive({
    req(asv_table_filtered())
    
    group_var <- if (input$soi_grouping %in% c("Site", "Region")) {
      input$soi_grouping
    } else {
      "Site"
    }
    
    compute_replicate_summary(asv_table_filtered(), input$abundance_threshold, group_var)
  })
  
  #Download of the replicate table
  output$download_replicate_table <- downloadHandler(
    filename = function() paste0("replicate_table_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- replicate_summary()
      req(nrow(df) > 0)
      write.csv(prepare_replicate_download(df), file, row.names = FALSE)
    }
  )
  #Render heatmap layout
  output$heatmap_ui <- renderUI({
    req(replicate_summary())
    build_replicate_heatmap_ui(replicate_summary())
  })
  
  #Make one plot per species
  observe({
    req(replicate_summary())
    species_list <- unique(replicate_summary()$sscinames)
    
    for (i in seq_along(species_list)) {
      local({
        my_i <- i
        sp   <- species_list[my_i]
        
        output[[paste0("replicate_heatmap_", my_i)]] <- renderPlot({
          df_sp <- replicate_summary() %>% filter(sscinames == sp)
          plot_replicate_heatmap(df_sp)
        }, bg = "transparent")
      })
    }
  })
  
####Blast visualisation####

  asv_table <- reactive({
    req(input$table_file)
    fread(input$table_file$datapath, header = TRUE)
  })
  
  coordinates <- reactive({
    req(input$coord_file)
    fread(input$coord_file$datapath, header = TRUE, sep = "\t")
  })
  
  output$debug_info <- renderText({
    req(asv_table(), filtered_results2())
    soi_results <- filtered_results2() %>% filter(SOI_flag == TRUE) %>% select(qseqid, sscinames)
    paste("Species of Interest ASVs found:", length(unique(soi_results$qseqid)),
          "\nTotal samples:", ncol(asv_table()) - 1)
  })
  
  soi_summary <- reactive({
    req(asv_table(), filtered_results2(), coordinates())
    prepare_soi_summary(asv_table(), filtered_results2(), coordinates())
  })
  
  output$soi_map <- renderLeaflet({
    df <- soi_summary()
    req(nrow(df) > 0)
    
    df_coords  <- df %>% filter(!is.na(Latitude) & !is.na(Longitude))
    center_lat <- mean(df_coords$Latitude)
    center_lon <- mean(df_coords$Longitude)
    zoom_level <- calculate_zoom_level(
      diff(range(df_coords$Longitude, na.rm = TRUE)),
      diff(range(df_coords$Latitude,  na.rm = TRUE))
    )
    
    summarise_by <- input$soi_grouping
    df_summary   <- prepare_map_summary(df, summarise_by)
    
    leaflet(df_summary) %>%
      addTiles() %>%
      addCircleMarkers(
        lng         = ~Longitude,
        lat         = ~Latitude,
        radius      = ~radius,
        color       = "#716fa8",
        fillOpacity = 0.7,
        stroke      = FALSE,
        popup       = ~popup_text
      ) %>%
      setView(lng = center_lon, lat = center_lat, zoom = zoom_level)
  })
  
  observe({
    req(filtered_results2())
    soi_results  <- filtered_results2() %>% filter(SOI_flag == TRUE)
    species_list <- unique(soi_results$sscinames)
    updateSelectInput(session, "species_choice", choices = species_list, selected = NULL)
  })
  
  selected_asvs <- reactive({
    req(input$species_choice, filtered_results2(), asv_table())
    get_asvs_for_species(filtered_results2(), asv_table(), input$species_choice)
  })
  
  global_max_proportion <- reactive({
    req(asv_table(), filtered_results2(), coordinates())
    
    all_soi_asvs <- filtered_results2() %>% 
      filter(SOI_flag == TRUE) %>% 
      pull(qseqid) %>% 
      unique()
    
    total_community <- asv_table() %>%
      pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
      group_by(SampleID) %>%
      summarise(Total_Community = sum(Abundance))
    
    asv_table() %>%
      filter(ASV %in% all_soi_asvs) %>%
      pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
      group_by(SampleID) %>%
      summarise(Abundance = sum(Abundance)) %>%
      left_join(total_community, by = "SampleID") %>%
      mutate(Proportion = ifelse(Total_Community > 0, Abundance / Total_Community, 0)) %>%
      filter(Proportion > 0) %>%
      pull(Proportion) %>%
      max()
  })
  
  global_min_proportion <- reactive({
    req(asv_table(), filtered_results2(), coordinates())
    
    all_soi_asvs <- filtered_results2() %>% 
      filter(SOI_flag == TRUE) %>% 
      pull(qseqid) %>% 
      unique()
    
    total_community <- asv_table() %>%
      pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
      group_by(SampleID) %>%
      summarise(Total_Community = sum(Abundance))
    
    asv_table() %>%
      filter(ASV %in% all_soi_asvs) %>%
      pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
      group_by(SampleID) %>%
      summarise(Abundance = sum(Abundance)) %>%
      left_join(total_community, by = "SampleID") %>%
      mutate(Proportion = ifelse(Total_Community > 0, Abundance / Total_Community, 0)) %>%
      filter(Proportion > 0) %>%
      pull(Proportion) %>%
      min()
  })
  
  merged_data <- reactive({
    req(selected_asvs(), coordinates(), asv_table())
    prepare_merged_data(
      selected_asvs  = selected_asvs(),
      coordinates    = coordinates(),
      asv_table      = asv_table(),
      max_proportion = global_max_proportion(),
      min_proportion = global_min_proportion()
    )
  })
  
  
  output$map <- renderLeaflet({
    merged     <- merged_data()
    req(merged)
    center_lon <- mean(merged$Longitude, na.rm = TRUE)
    center_lat <- mean(merged$Latitude,  na.rm = TRUE)
    zoom_level <- calculate_zoom_level(
      diff(range(merged$Longitude, na.rm = TRUE)),
      diff(range(merged$Latitude,  na.rm = TRUE))
    )
    
    leaflet(data = merged) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        color       = ~ifelse(SOI_Present, "#716fa8", "#a3a3a3"),
        fillColor   = ~ifelse(SOI_Present, "#716fa8", "#a3a3a3"),
        fillOpacity = ~ifelse(SOI_Present, 1, 0),
        radius      = ~radius_scaled,
        weight      = 1.5,
        popup       = ~paste("Name:", Site, "<br>Read Counts:", Abundance, "<br>Proportion:", Proportion)
      ) %>%
      setView(lng = center_lon, lat = center_lat, zoom = zoom_level)
  }) 

####Add new data to long term dataset####

  output$select_likely_species_ui <- renderUI({
    req(filtered_results2())
    soi_results  <- filtered_results2() %>% filter(SOI_flag == TRUE, Multiple_Instances == FALSE)
    species_list <- unique(soi_results$sscinames)
    if (length(species_list) == 0) return(p("No likely Species of Interest found", style = "color: white;"))
    checkboxGroupInput("selected_likely_species", label = NULL,
                       choices = sort(species_list), selected = NULL)
  })
  
  output$select_potential_species_ui <- renderUI({
    req(filtered_results2())
    soi_results  <- filtered_results2() %>% filter(SOI_flag == TRUE, Multiple_Instances == TRUE)
    species_list <- unique(soi_results$sscinames)
    if (length(species_list) == 0) return(p("No putative Species of Interest found", style = "color: white;"))
    checkboxGroupInput("selected_potential_species", label = NULL,
                       choices = sort(species_list), selected = NULL)
  })
  
  pa_table_with_metadata <- reactiveVal(NULL)
  
  observeEvent(input$generate_editable_table, {
    req(filtered_results2(), asv_table(), coordinates())
    
    selected_species <- c(input$selected_likely_species, input$selected_potential_species)
    shiny::validate(need(length(selected_species) > 0, "Please select at least one species"))
    
    pa_table <- generate_pa_table(
      filtered_results2 = filtered_results2(),
      asv_table         = asv_table(),
      coordinates       = coordinates(),
      selected_species  = selected_species
    )
    shiny::validate(need(!is.null(pa_table) && nrow(pa_table) > 0, "No ASVs found for selected species"))
    
    if (input$metadata_source == "manual") {
      pa_table <- add_manual_metadata(pa_table,
                                      year        = input$year_fill,
                                      gene        = input$gene_fill,
                                      methodology = input$methodology_fill)
    } else if (input$metadata_source == "upload") {
      req(input$metadata_file)
      pa_table <- add_uploaded_metadata(pa_table, input$metadata_file$datapath)
    }
    
    pa_table <- reorder_pa_columns(pa_table)
    pa_table_with_metadata(pa_table)
  })
  
  output$editable_pa_table <- renderDT({
    req(pa_table_with_metadata())
    render_pa_datatable(pa_table_with_metadata())
  })
  
  observeEvent(input$editable_pa_table_cell_edit, {
    info         <- input$editable_pa_table_cell_edit
    updated_data <- update_pa_cell(pa_table_with_metadata(),
                                   row_index = info$row,
                                   col_index = info$col + 1,
                                   value     = info$value)
    pa_table_with_metadata(updated_data)
  })
  
  output$download_pa_table <- downloadHandler(
    filename = function() paste0("SoI_presence_absence_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(pa_table_with_metadata())
      write.csv(pa_table_with_metadata(), file, row.names = FALSE)
    }
  )
  
  observeEvent(input$add_to_historic, {
    req(pa_table_with_metadata())
    showModal(modalDialog(
      title  = "Confirm Addition",
      "Are you sure you want to add this data to the historic dataset?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_historic", "Confirm", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$confirm_add_historic, {
    req(pa_table_with_metadata())
    
    tryCatch({
      updated_historic <- add_to_historic(pa_table_with_metadata(), historic_data())
      
      if (!dir.exists("data")) dir.create("data")
      write.csv(updated_historic, "data/historic.csv", row.names = FALSE)
      historic_data(updated_historic)
      
      removeModal()
      showModal(modalDialog(
        title  = "Success",
        paste("Successfully added records to historic data."),
        footer = modalButton("OK")
      ))
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title  = "Error",
        paste("Failed to update historic data:", e$message),
        footer = modalButton("OK")
      ))
    })
  })
  
####Blast Output Files####
  
  db_name <- reactive({
    req(input$species_file, input$amplicon_of_interest)
    
    filename <- tools::file_path_sans_ext(basename(input$species_file$name))
    gene <- gsub("[^A-Za-z0-9]", "", input$amplicon_of_interest)  
    
    paste0(filename, "_", gene)
  })
  
  output$download_full_result <- downloadHandler(
    filename = function() {
      paste0("blast_full_results_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(blast_results(), file, row.names = FALSE, col.names = TRUE, sep = "\t")
    }
  )
  
  output$download_filtered_result <- downloadHandler(
    filename = function() {
      paste0("blast_filtered_results_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(filtered_results(), file, row.names = FALSE, col.names = TRUE, sep = "\t")
    }
  )
  
#####################
####Long Term DATA####  
#####################  
  
historic_data <- reactiveVal(load_historic_data())
  
  observe({
    runjs('$(".nav-link").css({
    "color": "#a3a3a3",
    "background-color": "transparent",
    "border": "none"
  })')
  })
  
  output$historic_species_select <- renderUI({
    build_historic_ui_selects(historic_data())$species
  })
  
  output$historic_gene_select <- renderUI({
    build_historic_ui_selects(historic_data())$gene
  })
  
  output$historic_method_select <- renderUI({
    build_historic_ui_selects(historic_data())$samplemethod
  })
  
  output$historic_year_slider <- renderUI({
    build_historic_year_slider(historic_data())
  })
  
  #filter data based on selection options
  historic_filtered_data <- reactive({
    req(input$species, input$gene, input$year_range, input$samplemethod)
    filter_historic_data(
      data         = historic_data(),
      species      = input$species,
      gene         = input$gene,
      year_range   = input$year_range,
      samplemethod = input$samplemethod
    )
  })
  
  #plot map using Lat long from data
  output$historic_map <- renderLeaflet({
    data <- historic_data()
    if (nrow(data) == 0) {
      leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2)
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = mean(data$Longitude, na.rm = TRUE),
                lat = mean(data$Latitude,  na.rm = TRUE),
                zoom = 6)
    }
  })
  
  observe({
    filtered_data <- historic_filtered_data()
    if (nrow(filtered_data) == 0 || is.null(filtered_data)) return()
    
    filtered_data_map <- filtered_data %>%
      filter(Presence == "Present" & Species == input$species & Gene == input$gene)
    #put points on map where species are present
    if (nrow(filtered_data_map) > 0) {
      leafletProxy("historic_map", data = filtered_data_map) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng         = ~Longitude,
          lat         = ~Latitude,
          radius      = 6,
          weight      = 1.5,
          color       = "#716fa8",
          fillColor   = "#716fa8",
          fillOpacity = 1,
          layerId     = ~as.character(Site)
        ) %>%
        setView(
          lng  = mean(filtered_data_map$Longitude, na.rm = TRUE),
          lat  = mean(filtered_data_map$Latitude,  na.rm = TRUE),
          zoom = calculate_zoom_level(
            lon_range = diff(range(filtered_data_map$Longitude, na.rm = TRUE)),
            lat_range = diff(range(filtered_data_map$Latitude,  na.rm = TRUE)))
        )
    }
  })
  
  #Click on map and reveal heatmap showing the presence/absence of species at the site. 
  observeEvent(input$historic_map_marker_click, {
    req(input$historic_map_marker_click)
    
    clicked_site <- input$historic_map_marker_click$id
    
    site_data <- historic_filtered_data() %>%
      filter(
        Site    == clicked_site,
        Species == input$species,
        input$samplemethod == "All" | Methodology == input$samplemethod
      ) %>%
      group_by(Year, Site) %>%
      summarize(
        Presence = ifelse("Present" %in% Presence, "Present", "Absent"),
        .groups  = "drop"
      )
    
    if (nrow(site_data) > 0) {
      showModal(modalDialog(
        title     = paste("Heatmap for", clicked_site),
        plotOutput("historic_heatmap"),
        easyClose = TRUE
      ))
      output$historic_heatmap <- renderPlot({
        plot_historic_heatmap(site_data, clicked_site)
      })
    }
  })
  
  #Plot bar plot for the species, gene and year range to show number of species detected each year
  output$historic_plot <- renderPlot({
    req(input$species, input$gene, input$year_range)
    
    data <- historic_data()
    if (nrow(data) == 0) return(NULL)
    
    filtered_data <- data %>%
      filter(
        Species          == input$species,
        Gene             == input$gene,
        as.numeric(Year) >= input$year_range[1],
        as.numeric(Year) <= input$year_range[2],
        if (input$samplemethod != "All") Methodology == input$samplemethod else TRUE
      )
    
    if (nrow(filtered_data) == 0) return(NULL)
    
    plot_historic_bar(filtered_data, input$species)
  }, bg = "transparent")
  
###########################  
###DATABASE CONSTRUCTION###
########################### 
####WKT Selection####

  output$show_results <- reactive({
    show_results()
  })
  outputOptions(output, "show_results", suspendWhenHidden = FALSE)
  
#Map for WKT selection
  output$database_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addDrawToolbar(
        targetGroup = "drawnShapes",
        polygonOptions = drawPolygonOptions(showArea = TRUE),
        rectangleOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        editOptions = editToolbarOptions()
      ) %>%
      onRender("function(el, x) {
            this.on('draw:created', function(e) {
                Shiny.setInputValue('map_draw_new_feature', e.layer.toGeoJSON());
            });
        }")
  })
  
  #Take the polygon drawn by the user to create WKT coordinates
  observeEvent(input$map_draw_new_feature, {
    req(input$map_draw_new_feature)
    
    wkt <- geojson_to_wkt(input$map_draw_new_feature)
    polygon_wkt(wkt)
    
    output$wkt_output <- renderText({
      paste("WKT coordinates: ", polygon_wkt())
    })
  })
  
  
  selected_wkt <- reactive({
    # Use the user-input WKT if provided, otherwise fallback to drawn polygon WKT
    if (nzchar(input$WKT_input)) {
      return(input$WKT_input)
    } else {
      return(polygon_wkt())
    }
  })
  
  #Output the WKT coordinates
  output$wkt_output <- renderText({
    paste("WKT coordinates: ", selected_wkt())
  })
  
  # Clear drawn features
  observeEvent(input$clear, {
    leafletProxy("database_map") %>% clearShapes()
    output$wkt_output <- renderText("")
  })
  
  

####Query Options####

  
  # Dynamic UI for query selection
  output$selection_fields <- renderUI({
    query_list <- queries()
    lapply(seq_along(query_list), function(i) {
      tagList(
        textInput(paste0("query_", i), label = paste("Query", i), value = query_list[[i]]$query),
        selectInput(paste0("field_type_", i), label = paste("Field Type for Query", i),
                    choices = c("gene", "All Fields"), selected = query_list[[i]]$field_type)
      )
    })
  })
  
  # Add new NCBI query
  observeEvent(input$add_selection, {
    query_list <- queries()
    query_list[[length(query_list) + 1]] <- list(query = "", field_type = "gene")
    queries(query_list)
  })
  
  # Remove the last query
  observeEvent(input$remove_selection, {
    query_list <- queries()
    if (length(query_list) > 0) {
      queries(query_list[-length(query_list)])
    }
  })
  
  # Process custom query input
  observeEvent(input$custom_query, {
    if (input$query_mode == "add" && input$custom_query != "") {
      query_list <- queries()
      query_list[[length(query_list) + 1]] <- list(query = input$custom_query, field_type = "gene")
      queries(query_list)
    }
  })
 

####Create taxa list for downloads####

  
  observeEvent(input$generate_run, {
    req(input$species_file)
    
    # Clear any errors from a previous run
    error_log(character(0))
    pipeline_failed(FALSE)
    
    showModal(modalDialog(
      title = "Processing",
      "Running Database Construction... Please wait.",
      footer = NULL
    ))
    
    #get the options selected for the download
    query_mode_rv(input$query_mode)
    selected_sources_rv(input$source)
    
    #read species list
    species_list <- readLines(input$species_file$datapath) %>% trimws()
    species_list_rv(species_list) 
    #get genera from the species list
    genera       <- unique(sapply(species_list, function(x) strsplit(x, " ")[[1]][1]))
    genera_rv(genera) 
    
    # Look up GBIF family taxon IDs for the species list
    family_taxon_ids <- tryCatch(
      get_family_taxon_ids(species_list),
      error = function(e) { log_error(paste("Failed to retrieve taxon IDs:", e$message)); NULL }
    )
    #No taxon ID notification
    if (is.null(family_taxon_ids) || length(family_taxon_ids) == 0) {
      log_error("No valid taxon IDs found for the provided species list.")
      removeModal(); return(NULL)
    }
    
    #WKT error notification
    wkt_polygon <- selected_wkt()
    if (is.null(wkt_polygon) || nchar(wkt_polygon) < 10) {
      log_error("No valid polygon selected. Draw a region on the map first.")
      removeModal(); return(NULL)
    }
    
    #See if there is a gbif cache of taxa for the coordinates
    cache        <- check_gbif_cache(family_taxon_ids, wkt_polygon)
    cache_path_rv(cache$path)
    
    if (cache$use_cache) {
      gbif_results(cache$data)
      showNotification("Using cached GBIF data", type = "message", duration = 5)
    } else {
      # Submit a new GBIF download request if no cache exists
      req_download <- tryCatch(
        submit_gbif_download(family_taxon_ids = family_taxon_ids, wkt_polygon = wkt_polygon),
        error = function(e) { log_error(paste("GBIF download submission failed:", e$message)); NULL }
      )
      if (is.null(req_download)) { removeModal(); return(NULL) }
      gbif_request_id(req_download)
      showNotification("GBIF download started...", type = "message", duration = 5)
    }
    
    #function to get the synonyms for the taxa in the list
    synonyms_df <- tryCatch(
      get_synonyms(
        species_list      = species_list,
        db_name           = db_name(),
        get_synonyms_gbif = get_synonyms_gbif
      ),
      error = function(e) { log_error(paste("Synonym lookup failed:", e$message)); NULL }
    )
    if (is.null(synonyms_df)) { removeModal(); return(NULL) }
    species_dataframe_rv(synonyms_df)
  })

  # Check the GBIF download status and process when ready
  observe({
    req(gbif_request_id())
    invalidateLater(5000)
    
    meta <- tryCatch(
      occ_download_meta(gbif_request_id()),
      error = function(e) return(NULL)
    )
    
    if (!is.null(meta) && meta$status == "SUCCEEDED") {
      occ <- tryCatch(
        process_gbif_zip(gbif_request_id()),
        error = function(e) { log_error(paste("Failed to process GBIF download:", e$message)); NULL }
      )
      if (!is.null(occ)) {
        gbif_results(occ)
        saved <- save_gbif_cache(occ, cache_path_rv())
        if (saved) showNotification("GBIF data cached", type = "message", duration = 3)
      }
      gbif_request_id(NULL)
    }
  })
  
  # Update additional species from GBIF results
  observeEvent(gbif_results(), {
    occ <- gbif_results()
    if (!is.null(occ) && "species" %in% colnames(occ)) {
      additional_species_rv(unique(occ$species[!is.na(occ$species) & nzchar(occ$species)]))
    }
  })

# Build taxa vector when all components ready
taxa_ready <- reactive({
  req(genera_rv(), additional_species_rv(), species_dataframe_rv())
  build_taxa_vector(genera_rv(), gbif_results(), species_dataframe_rv())
})



####Run Downloads####

observeEvent(taxa_ready(), {
  taxa_vector      <- taxa_ready()
  selected_sources <- selected_sources_rv()

###NCBI DOWNLOAD###

  #Create the query command for use in the ncbi download.
  query_commands <- if (query_mode_rv() == "add") {
    custom_query <- input$custom_query
    if (!is.null(custom_query) && nzchar(trimws(custom_query))) {
      custom_query
    } else {
      ""
    }
  } else {
    query_list <- lapply(seq_along(queries()), function(i) {
      paste0(input[[paste0("query_", i)]], "[", input[[paste0("field_type_", i)]], "]")
    })
    paste(query_list, collapse = " OR ")
  }
  
  final_query_str <- query_commands
  output$query_output <- renderText({ final_query_str })
  
  #if ncbi is selected then run the download of ncbi sequences
  if ("ncbi" %in% selected_sources) {
    success <- download_ncbi(
      crabs_path      = crabs_path,
      taxa_vector     = taxa_vector,
      final_query_str = final_query_str,
      ncbi_email      = ncbi_email
    )
    if (!isTRUE(success)) {
      log_error("NCBI download failed.")
      removeModal(); return(NULL)
    }
  }
  
###BOLD DOWNLOAD###

  #if bold is selected then run the download of bold sequences
  if ("bold" %in% selected_sources) {
    success <- download_bold(
      crabs_path  = crabs_path,
      taxa_vector = taxa_vector,
      bold_marker = input$bold_marker
    )
    if (!isTRUE(success)) {
      log_error("BOLD download failed.")
      removeModal(); return(NULL)
    }
  }
  

###SILVA DOWNLOAD###
  
  #if silva is selected then run the download of silva sequences
  if ("silva" %in% selected_sources) {
    success <- tryCatch({
      download_silva(
        crabs_path   = crabs_path,
        silva_marker = input$silva_marker,
        taxa_vector  = taxa_vector
      )
    }, error = function(e) {
      log_error(paste("SILVA download failed:", e$message))
      return(FALSE)
    })
    
    if (!isTRUE(success)) {
      log_error("SILVA download failed.")
      removeModal(); return(NULL)
    }
  }
  
###IMPORT CUSTOM FASTA FILE###
  
  #run function if custom fasta is selected
  if (!is.null(input$custom_fasta)) {
    success <- import_custom_fasta(
      crabs_path        = crabs_path,
      custom_fasta_path = input$custom_fasta$datapath
    )
    if (!isTRUE(success)) {
      log_error("Custom FASTA import failed.")
      removeModal(); return(NULL)
    }
  }
  
####CREATE BLAST DATABASE####
  
  #run function to create the blast database.
  success <- tryCatch({
    create_database(
      crabs_path       = crabs_path,
      selected_sources = selected_sources,
      custom_fasta     = input$custom_fasta,
      forward_primer   = input$forward_primer,
      reverse_primer   = input$reverse_primer,
      db_name          = db_name()
    )
  }, error = function(e) {
    log_error(paste("Pipeline failed:", e$message, "\n", 
                    paste(capture.output(traceback()), collapse = "\n")))
    removeModal()
  }, warning = function(w) {
    log_error(paste("Pipeline warning:", w$message))
    removeModal()
  })
  
  if (!success) {
    removeModal()
    return(NULL)
  }
  
  output$execution_output <- renderText({ paste(all_exec_output, collapse = "\n") })
  show_results(TRUE)
  removeModal()
  updateTabsetPanel(session, "main_tabs", selected = "Results")
})

####Database Creation results####

#Create dataframe for species presence from the download results
species_presence <- reactive({
  req(input$species_file, species_dataframe_rv())
  tryCatch(
    compute_species_presence(
      species_dataframe = species_dataframe_rv(),
      cleaned_data_path = "intermediate/merged_combined_output.tax.cleaned.final.tsv",
      db_name           = db_name()
    ),
    error = function(e) { log_error(paste("Species presence computation failed:", e$message)); NULL }
  )
})

output$presence_table <- renderTable({
  req(species_presence())
  species_presence()$presence_df
})

#Create heatmap to show the presence/absence results

output$heatmap <- renderPlot({
  req(species_presence())
  plot_species_presence(species_presence()$presence_df)
}, bg = "transparent")

output$dynamic_heatmap <- renderUI({
  req(species_presence())
  plot_height <- max(300, nrow(species_presence()$presence_df) * 30)
  plotOutput("heatmap", height = paste0(plot_height, "px"))
})

####Output files for database####

#Downlaod table of species presence absence of listed species
output$download_presence <- downloadHandler(
  filename = function() paste0(db_name(), ".species_presence.csv" ),
  content = function(file) {
    write.csv(species_presence()$presence_df, file, row.names = FALSE)
  }
)

#calculate the percentage of species on list found in database
output$species_summary <- renderText({
  species_info <- species_presence()
  sprintf("Percentage of species from list present in the database: %.2f%%", species_info$percentage_present)
})

#Download table of all species in the database
output$download_database_species_list <- downloadHandler(
  
  filename = function() {
    paste0("database_species_", Sys.Date(), ".txt")
  },
  
  content = function(file) {
    
    
    cleaned_data <- read.delim("intermediate/merged_combined_output.tax.cleaned.final.tsv", header = F, sep = "\t")
    species <- cleaned_data[, 10]
    species <- trimws(species)
    species <- species[!is.na(species) & nzchar(species)]
    species <- sort(unique(species))
    
    writeLines(species, file)
  }
)

####Species Differentiation####

species_presence_detailed <- reactive({
  req(input$species_file)
  req(species_dataframe_rv())
  req(gbif_results())
  
  species_list <- readLines(input$species_file$datapath)
  species_list <- trimws(species_list)
  
  # Show progress
  showModal(modalDialog(
    title = "Processing",
    paste("Analyzing", length(species_list), "species... Please wait."),
    footer = NULL
  ))
  
  # Read the database
  cleaned_data <- read.delim("intermediate/merged_combined_output.tax.cleaned.final.tsv", header = F, sep = "\t")
  db_species <- cleaned_data[, 10]   # species column
  db_genus <- cleaned_data[, 9]      # genus column
  db_family <- cleaned_data[, 8]     # family column
  
  df <- species_dataframe_rv()
  gbif_data <- gbif_results()
  
  # Pre-fetch family info for ALL species in the list (batch processing)
  family_cache <- list()
  for (soi in species_list) {
    Sys.sleep(0.1)  # Rate limiting
    family_cache[[soi]] <- tryCatch({
      backbone_info <- name_backbone(name = soi)
      if (!is.null(backbone_info$family)) {
        backbone_info$family
      } else {
        NA_character_
      }
    }, error = function(e) {
      NA_character_
    })
  }
  
  # Get all synonyms for ALL species upfront
  all_soi_in_list <- species_list
  all_synonyms_in_list <- df$canonicalName[df$SOI %in% all_soi_in_list]
  
  # For each species of interest, find related species
  detailed_results <- lapply(species_list, function(soi) {
    # Get genus and family from cache
    genus <- strsplit(soi, " ")[[1]][1]
    family <- family_cache[[soi]]
    
    # Get all synonyms for this SOI from the dataframe
    soi_synonyms <- df$canonicalName[df$SOI == soi]
    
    # Find congeneric species (same genus but NOT any SOI or their synonyms) in database
    congeneric_in_db <- unique(db_species[db_genus == genus & !is.na(db_species)])
    congeneric_in_db <- setdiff(congeneric_in_db, c(all_soi_in_list, all_synonyms_in_list))
    
    # Check if congeneric species are in the region (from GBIF data)
    congeneric_in_region <- if (!is.null(gbif_data) && "species" %in% colnames(gbif_data) && "genus" %in% colnames(gbif_data)) {
      unique(gbif_data$species[gbif_data$genus == genus & !is.na(gbif_data$species)])
    } else {
      character(0)
    }
    congeneric_in_region <- setdiff(congeneric_in_region, c(all_soi_in_list, all_synonyms_in_list))
    
    # Find congeneric species that are in BOTH region AND database
    congeneric_in_region_and_db <- intersect(congeneric_in_region, congeneric_in_db)
    
    # Find confamilial species (same family but NOT same genus) in database
    if (!is.na(family)) {
      confamilial_in_db <- unique(db_species[db_family == family & db_genus != genus & !is.na(db_species)])
      confamilial_in_db <- setdiff(confamilial_in_db, c(all_soi_in_list, all_synonyms_in_list))
      
      # For confamilial in region - skip this for now as it's too slow
      # Just use database family info
      confamilial_in_region <- character(0)
    } else {
      confamilial_in_db <- character(0)
      confamilial_in_region <- character(0)
    }
    
    # Find confamilial species that are in BOTH region AND database
    confamilial_in_region_and_db <- intersect(confamilial_in_region, confamilial_in_db)
    
    # Check if SOI itself is present
    soi_present <- any(df$canonicalName[df$SOI == soi] %in% db_species)
    
    data.frame(
      SOI = soi,
      Genus = genus,
      Family = ifelse(is.na(family), "Unknown", family),
      SOI_Present = ifelse(soi_present, "Yes", "No"),
      Congeneric_InRegion = length(congeneric_in_region),
      Congeneric_InDB = length(congeneric_in_db),
      Congeneric_InRegion_InDB = length(congeneric_in_region_and_db),
      Confamilial_InRegion = length(confamilial_in_region),
      Confamilial_InDB = length(confamilial_in_db),
      Confamilial_InRegion_InDB = length(confamilial_in_region_and_db),
      Congeneric_List = paste(congeneric_in_db, collapse = "; "),
      Congeneric_Region_List = paste(congeneric_in_region, collapse = "; "),
      Congeneric_Region_DB_List = paste(congeneric_in_region_and_db, collapse = "; "),
      Confamilial_List = paste(head(confamilial_in_db, 10), collapse = "; "),
      Confamilial_Region_DB_List = paste(head(confamilial_in_region_and_db, 10), collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  
  detailed_df <- do.call(rbind, detailed_results)
  
  # Calculate percentage present
  percentage_present <- (sum(detailed_df$SOI_Present == "Yes") / nrow(detailed_df)) * 100
  
  removeModal()
  
  list(
    detailed_df = detailed_df,
    percentage_present = percentage_present
  )
})

#Compute species differentiation by running 100% BLAST on all species of interest. Identify those that can not be uniquely distinguished in the current database
species_differentiation <- reactive({
  req(species_presence_detailed())
  req(file.exists("intermediate/merged_combined_output.tax.cleaned.final.tsv"))
  
  showModal(modalDialog(
    title  = "Processing Species Differentiation",
    "Analyzing 100% sequence matches... Please wait.",
    footer = NULL
  ))
  
  result <- tryCatch({
    run_species_differentiation(
      species_presence_detailed = species_presence_detailed(),
      db_name                   = db_name()
    )
  }, error = function(e) {
    log_error(paste("Species differentiation failed:", e$message))
    NULL
  })
  
  removeModal()
  
  # Flag results as ready if computation succeeded
  if (!is.null(result)) show_differentiation(TRUE)
  
  result
})

output$show_differentiation <- reactive({
  show_differentiation()
})

outputOptions(output, "show_differentiation", suspendWhenHidden = FALSE)

# Render the differentiation results table
output$differentiation_table <- renderDT({
  req(species_differentiation())
  
  diff_data <- species_differentiation() %>%
    distinct(Species, .keep_all = TRUE)
  
  datatable(
    diff_data,
    options  = list(
      pageLength   = 25,
      scrollX      = TRUE,
      initComplete = JS(
        "function(settings, json) {",
        "  $(this.api().table().container()).css({'color': 'white'});",
        "  $(this.api().table().header()).css({'color': 'white', 'background-color': 'transparent'});",
        "  $('#' + settings.sTableId + '_length label').css('color', 'white');",
        "  $('#' + settings.sTableId + '_filter label').css('color', 'white');",
        "  $('#' + settings.sTableId + '_filter input').css({'color': 'white', 'background-color': 'transparent', 'border': '1px solid white'});",
        "}"
      )
    ),
    rownames = FALSE
  ) %>%
    formatStyle(
      columns         = 1:ncol(diff_data),
      color           = 'white',
      backgroundColor = 'transparent'
    ) %>%
    formatStyle(
      'Num_Matching_Species',
      backgroundColor = styleInterval(c(0.5), c('#d4edda', '#deabaf'))
    )
})

# Download the differentiation results as CSV
output$download_differentiation <- downloadHandler(
  filename = function() paste0(db_name(), "_species_differentiation.csv"),
  content  = function(file) {
    write.csv(species_differentiation(), file, row.names = FALSE)
  }
)

observeEvent(input$main_tabs, {
  req(input$main_tabs == "differentiation_res")
  species_differentiation()
})

####Database Comparison####

comparison_data_folder <- "data"

# Populate database choices
observe({
  prefixes <- get_dbcomparison_prefixes(comparison_data_folder)
  updateSelectInput(session, "dbcomparison_prefix",
                    choices  = c("Select database group" = "", prefixes),
                    selected = "")
})

# Load comparison data
dbcomparison_data <- reactive({
  req(input$dbcomparison_prefix != "")
  tryCatch(
    load_dbcomparison_data(comparison_data_folder, input$dbcomparison_prefix),
    error = function(e) { log_error(paste("Failed to load comparison data:", e$message)); NULL }
  )
})

# Download button UI
output$download_dbcomparison_table_ui <- renderUI({
  req(input$dbcomparison_prefix != "")
  downloadButton("download_dbcomparison_table", "Download Table",
                 style = "width:200px; font-size:16px; margin-top:10px;")
})

# Download handler
output$download_dbcomparison_table <- downloadHandler(
  filename = function() {
    paste0(input$dbcomparison_prefix, "_table_", Sys.Date(), ".csv")
  },
  content = function(file) {
    df <- dbcomparison_data()
    req(nrow(df) > 0)
    write.csv(df, file, row.names = FALSE)
  }
)

# Plot Heatmap
output$dbcomparison_heatmap <- renderPlot({
  df <- dbcomparison_data()
  req(nrow(df) > 0)
  tryCatch(
    plot_dbcomparison_heatmap(df),
    error = function(e) { log_error(paste("Comparison heatmap failed:", e$message)) }
  )
}, bg     = "transparent",
height    = function() dbcomparison_plot_height(dbcomparison_data()),
width     = function() dbcomparison_plot_width(dbcomparison_data()))

#####################
###Session cleanup###
#####################

onSessionEnded(function() {
  if (.Platform$OS.type == "windows") {
    system("del /Q intermediate/insilico_*.txt", intern = TRUE)
    system("del /Q intermediate/*.zip", intern = TRUE)
    system("del /Q intermediate/ncbi_*.fasta", intern = TRUE)
    system("del /Q intermediate/bold_*.tsv", intern = TRUE)
    system("del /Q intermediate/merged_combined_output.*", intern = TRUE)
    system("del /Q intermediate/silva_*", intern = TRUE)
    system("del /Q intermediate/merged_silva_output.tax.tsv", intern = TRUE)
    system("del /Q intermediate/*_cleaned.fasta", intern = TRUE)
    system("del /Q intermediate/merged_ncbi_output*", intern = TRUE)
    system("del /Q intermediate/merged_bold_output*", intern = TRUE)
    system("del /Q intermediate/combined.db*", intern = TRUE)
    system("del /Q intermediate/taxonomy.dictionary.*", intern = TRUE)
    system("del /Q intermediate/custom_fasta_output.*", intern = TRUE)
  } else {
    system("rm -f intermediate/insilico_*.txt")
    system("rm -f intermediate/*.zip")
    system("rm -f intermediate/ncbi_*.fasta")
    system("rm -f intermediate/bold_*.tsv")
    system("rm -f intermediate/merged_combined_output.*")
    system("rm -f intermediate/silva_*")
    system("rm -f intermediate/merged_silva_output.tax.tsv")
    system("rm -f intermediate/*_cleaned.fasta")
    system("rm -f intermediate/merged_ncbi_output*")
    system("rm -f intermediate/merged_bold_output*")
    system("rm -f intermediate/combined.db*")
    system("rm -f intermediate/taxonomy.dictionary.*")
    system("rm -f intermediate/custom_fasta_output.*")
  }
})


######################### 
####EXAMPLE DOWNLOADS#### 
#########################

output$download_COI <- downloadHandler(
  filename = function() {
    "example_COI.fasta" # Example COI file
  },
  content = function(file) {
    # Path to the file in the app folder
    file.copy("data/example_COI.fasta", file)
  },
  contentType = "application/octet-stream" 
)

output$download_18S <- downloadHandler(
  filename = function() {
    "example_18S.fasta" # Example 18S file
  },
  content = function(file) {
    
    file.copy("data/example_18S.fasta", file)
  },
  contentType = "application/octet-stream" 
)

output$download_18S_table <- downloadHandler(
  filename = function() {
    "example_18S_asvtable.csv" # Example ASV file
  },
  content = function(file) {
    
    file.copy("data/example_18S_asvtable.csv", file)
  },
  contentType = "application/octet-stream" 
)

output$download_18S_coord <- downloadHandler(
  filename = function() {
    "example_18S_coordinates.tsv" # Example coordinate file 
  },
  content = function(file) {
    
    file.copy("data/example_18S_coordinates.tsv", file)
  },
  contentType = "application/octet-stream" 
)



output$download_COI_table <- downloadHandler(
  filename = function() {
    "example_COI_asvtable.csv" # Example ASV file
  },
  content = function(file) {
    
    file.copy("data/example_COI_asvtable.csv", file)
  },
  contentType = "application/octet-stream" 
)

output$download_COI_coord <- downloadHandler(
  filename = function() {
    "example_COI_coordinates.tsv" # Example coordinate file 
  },
  content = function(file) {
    
    file.copy("data/example_COI_coordinates.tsv", file)
  },
  contentType = "application/octet-stream" 
)

output$download_custom <- downloadHandler(
  filename = function() {
    "example_custom.fasta" # Example custom file
  },
  content = function(file) {
    
    file.copy("data/example.custom.fasta", file)
  },
  contentType = "application/octet-stream" 
)


}
shinyApp(ui, server)
