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
             fluidPage(  # Wrap in fluidPage to ensure layout consistency
               div(
                 style = "display: flex; justify-content: space-between; gap: 20px; min-height: 400px;",  # Arrange two sections in a row
                 
                 # Left Section
                 div(
                   style = "flex: 1; margin-top: 50px; margin-left: 100px;",  # Shifts left and down
                   
                   # Styled File Input
                   div(
                     style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 5px;",
                     "Upload Query File (FASTA)"  # Custom-styled label
                   ),
                   div(style = "margin-bottom: 75px;",
                   fileInput("query_file", NULL, accept = c(".fasta"))  # Remove default label
                   ),
                   # Select Database
                   div(style = "color: white; font-size: 20px; font-weight: bold; margin-bottom: 5px;",
                       "Select Database"
                   ), 
                   selectInput("db_choice", NULL, choices = c("Loading..." = "")),
                   
                   div(
                     style = "flex: 1; display: flex; align-items: center; justify-content: center; margin-top: 70px;",
                     img(src = "BEM_logo2.png", height = "140px", style = "border-radius: 10px;")
                   )
                  
                 ),
                 
                 div(
                   style = "flex: 1; display: flex; align-items: center; justify-content: center; margin-top: 0px;",
                   img(src = "KAUST_logo_transparent.png", height = "180px", style = "border-radius: 10px;")
                 ),
                 
                 # Right Section
                 div(
                   style = "flex: 1; margin-top: 50px; margin-left: 95px;",  # Allows equal width distribution
                   div(style = "color: white; font-size: 20px; margin-bottom: 75px;",
                   fileInput("table_file", "Upload ASV Table (Optional)", accept = c(".txt", ".csv"))
                   ),
                   div(style = "color: white; font-size: 20px; white-space: nowrap; ",
                   fileInput("coord_file", "Upload Coordinate File (Optional)", accept = c(".txt", ".tsv"))
                   )
                 )
                 
               ), 
               # Action Button Below
               div(
                 style = "position: relative; height: 150px;",  # Parent div for positioning
                 actionButton("run_blast", "Run BLAST", 
                              style = "position: absolute; top: 10%; left: 50%; transform: translate(-50%, -50%);
                              width: 250px; height: 75px; font-size: 36px; background-color: #2F4E7D; color: white; 
                              border: none; outline: none; box-shadow: 2px 2px 7.5px rgba(0,0,0,0.5);")
               ),
               
               # Download Buttons Absolutely Positioned Below
               div(
                 style = "position: relative; height: 200px; text-align: center;",  # Parent div for absolute positioning
                 h4("Download Example Files", style = "font-size: 20px; color: white;"),
                 div(
                   style = "position: absolute; top: 30%; left: 50%; transform: translate(-50%, -50%); display: flex; gap: 20px;",
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
                          "Percentage Identity: ", 
                          textOutput("slider_pident_value", inline = TRUE)
                        ),
                        
                        sliderInput("pident_filter", "", min = 95, max = 100, value = c(97, 100)),
                        
                        
                        tags$div(
                          style = "font-size: 18px; color: white;",
                          "Sequence Length: ", 
                          textOutput("slider_length_value", inline = TRUE)
                        ),
                        
                        sliderInput("length_filter", "", min = 50, max = 1000, value = c(200, 1000)),
                        
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
                      p("The best similarity match was found for only a single Species of Interest species")),
                      uiOutput("soi_species_boxes_likely"),
                      div(style = "color: white; font-size: 20px; white-space: nowrap; ",
                      h3("Putative Species of Interest detection"),
                      p("Best similarity match is shared by multiple species. Further investigation is required.")),
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
    
    tabPanel("Visualisations",
             
             # --- TOP ROW WITH LEFT + RIGHT ELEMENTS ---
             fluidRow(
               column(
                 6,
                 div(
                   style = "margin-top: 20px; min-height: 80px; display: flex; align-items: center;",
                   downloadButton("download_heatmap_data", "Download Heatmap Data")
                 )
               ),
               column(
                 6,
                 div(
                   style = "margin-top: 20px; min-height: 80px; display: flex; align-items: center;",
                   selectInput(
                     "soi_grouping",
                     "Summarise Species of Interest counts by:",
                     choices = c("SampleID", "Site", "Region"),
                     selected = "SampleID",
                     width = "250px"
                   )
                 )
               )
             ),
             
             # --- NEXT ROW WITH HEATMAP + MAP ---
             fluidRow(
               column(
                 6,
                 div(
                   class = "vis-panel",
                   h3("Presence of species of interest in each submitted sample"),
                   div(
                     class = "vis-scroll",
                     uiOutput("SOI_heatmap_ui")
                   )
                 )
               ),
               column(
                 6,
                 div(
                   class = "vis-panel",
                   h3("# of ASVs for Species of Interest ASV at each site."),
                   p("Bubbles are scaled by number of ASVs. Click on site to see numbers"),
                   div(
                     class = "vis-scroll",
                     leafletOutput("soi_map", height = "500px")
                   )
                 )
               )
             )),
    
    
    tabPanel("Map",
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
                                          p("Construct an anti-clockwise polygon encompassing the region of interest to generate WKT coordinates or input previously obtained WKT coordinates into selection box"),
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
                       p("A presence/absence heatmap is created (with a downloadable table) to indicate the completeness of the database for that gene/primer"))   
                   
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
                         tags$a(href = "10.3389/fmars.2023.1295997", class = "custom-link", target = "_blank", "Pearman et al 2024"))),
                   
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
  
  Sys.setenv(PYTHONWARNINGS="ignore::UserWarning")

  crabs_path <- Sys.which("crabs")
  
  
  check_taxonomy_files <- function(data_dir, max_age_days = 180) {
    files <- c(
      "nodes.dmp",
      "names.dmp",
      "nucl_gb.accession2taxid"
    )
    
    paths <- file.path(data_dir, files)
    
    exists <- file.exists(paths)
    
    if (!all(exists)) {
      return(list(
        status = "missing",
        missing = files[!exists]
      ))
    }
    
    file_ages <- sapply(paths, function(f) {
      as.numeric(Sys.time() - file.info(f)$mtime, units = "days")
    })
    
    if (any(file_ages > max_age_days)) {
      return(list(
        status = "old",
        ages = round(file_ages, 1)
      ))
    }
    
    list(status = "ok")
  }
  
  
  download_taxonomy_async <- function(data_dir, crabs_path = "crabs") {
    future({
      cmd <- paste(
        shQuote(crabs_path),
        "--download-taxonomy",
        "--output",
        shQuote(data_dir)
      )
      
      system(cmd, intern = TRUE, wait = TRUE)
    })
  }
  
  
  observeEvent(TRUE, {
    data_dir <- "data"
    
    res <- check_taxonomy_files(data_dir)
    
    if (res$status == "missing") {
      
      showModal(modalDialog(
        title = "Taxonomy files missing",
        paste("Missing files:", paste(res$missing, collapse = ", ")),
        "They will now be downloaded automatically.",
        footer = NULL
      ))
      
      download_taxonomy_async(data_dir)
      removeModal()
      
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
  
  
  observeEvent(input$update_taxonomy, {
    
    data_dir <- "data"
    
    removeModal()
    
    showModal(modalDialog(
      title = "Updating taxonomy",
      tagList(
        "Downloading taxonomy files. This may take several minutes.",
        br(),
        tags$div(class = "spinner-border text-primary", role = "status")
      ),
      footer = NULL
    ))
    
    download_taxonomy_async(data_dir) %...>% {
      
      removeModal()
      
      showNotification(
        "Taxonomy update complete",
        type = "message",
        duration = 5
      )
      
    } %...!% {
      
      removeModal()
      
      showNotification(
        paste("Taxonomy update failed:", .),
        type = "error",
        duration = 10
      )
    }
  })


  gbif_request_id <- reactiveVal(NULL)
  gbif_results <- reactiveVal(NULL)
  additional_species_rv <- reactiveVal(NULL)
  species_dataframe_rv  <- reactiveVal(NULL)
  
  ncbi_email <- Sys.getenv("NCBI_EMAIL")
  
  # Load data outside reactive context
  historic_data <- reactive({
    
    # Default empty data
    df <- data.frame()
    
    tryCatch({
      if (file.exists("data/historic.csv")) {
        df <- read.csv("data/historic.csv", header = TRUE, stringsAsFactors = FALSE)
        
        # --- Preprocess ---
        if ("Year" %in% colnames(df)) df$Year <- as.numeric(df$Year)
        if ("Presence" %in% colnames(df)) df$Presence <- trimws(df$Presence)
        if ("Methodology" %in% colnames(df)) df$Methodology <- trimws(df$Methodology)
      }
    }, error = function(e) {
      print(paste("Error loading historic data:", e$message))
    })
    
    df
  })
  
  observe({
    runjs('$(".nav-link").css({
    "color": "#a3a3a3",
    "background-color": "transparent",
    "border": "none"
  })')
  })
  
  observeEvent(input$btn1, { updateTabsetPanel(session, "tabs", selected = "blast") })
  observeEvent(input$btn2, { updateTabsetPanel(session, "tabs", selected = "soiresults") })
  observeEvent(input$btn3, { updateTabsetPanel(session, "tabs", selected = "historic") })
  observeEvent(input$btn4, { updateTabsetPanel(session, "tabs", selected = "construction") })
  observeEvent(input$btn5, { updateTabsetPanel(session, "tabs", selected = "help") })
  
########################################  
####GET SYNONYMS FROM GBIF FOR BLAST####  
########################################
  
  get_synonyms_gbif <- function(species_name) {
    
    # Query GBIF backbone
    bb <- tryCatch({
      name_backbone(name = species_name)
    }, error = function(e) NULL)
    
    # No match → map species to itself
    if (is.null(bb) || is.null(bb$speciesKey)) {
      return(data.frame(
        SOI = species_name,
        canonicalName = species_name,
        stringsAsFactors = FALSE
      ))
    }
    
    key <- bb$speciesKey
    
    # Get synonyms
    syns <- tryCatch({
      name_usage(key = key, data = "synonyms")$data
    }, error = function(e) NULL)
    
    if (is.null(syns) || nrow(syns) == 0) {
      return(data.frame(
        SOI = species_name,
        canonicalName = species_name,
        stringsAsFactors = FALSE
      ))
    }
    
    # Build canonical list (self + synonyms)
    canonical_list <- unique(c(
      species_name,
      syns$species,
      syns$canonicalName
    ))
    canonical_list <- canonical_list[!is.na(canonical_list) & canonical_list != ""]
    
    species_df <- data.frame(
      SOI = rep(species_name, length(canonical_list)),
      canonicalName = canonical_list,
      stringsAsFactors = FALSE
    )
    
    # Clean names to Genus + species
    species_df <- species_df %>%
      tidyr::separate(canonicalName, c("Genus", "Species", "Other"), sep = " ", fill = "right") %>%
      tidyr::unite("canonicalName", Genus, Species, Other, sep = " ", na.rm = TRUE) %>%
      dplyr::select(SOI, canonicalName) %>%
      dplyr::distinct()
    
    return(species_df)
  }
  
  ######################
  ####FIND DATABASES####
  ######################
  
  observeEvent(input$tabs, {
    # Only run when "Run BLAST" tab is selected
    if (input$tabs == "blast") {
      db_folder <- "databases/"
      
      # List .nin files
      db_files <- list.files(db_folder, pattern = "\\.nin$", full.names = FALSE)
      
      # Remove .nin extension
      db_names <- gsub("\\.nin$", "", db_files)
      
      # Update selectInput
      updateSelectInput(session, "db_choice", choices = db_names)
    }
  })
  
################# 
####RUN BLAST####
#################
  
  blast_results <- reactiveVal("") # To store BLAST results
  
  observeEvent(input$run_blast, {
    req(input$query_file, input$db_choice) # Ensure inputs are provided
    
    # Show processing dialog
    showModal(modalDialog(
      title = "Processing",
      "Running BLAST... Please wait.",
      footer = NULL
    ))
    
    # Paths for input and output
    query_path <- input$query_file$datapath
    db_path <- file.path("databases", input$db_choice)
    output_file <- tempfile(fileext = ".txt")
    
    shiny::validate(
      need(file.exists(query_path), "Query file is missing or invalid."),
      need(file.exists(paste0(db_path, ".nin")), 
           "Database files are missing or the path is incorrect.")
    )
    
    
    # BLAST command
 blast_command <- paste(
  "blastn",
  "-query", query_path,
  "-db", db_path,
  "-out", output_file,
  "-max_target_seqs 5",
  "-num_threads 10",
  "-qcov_hsp_perc 80",
  "-outfmt", shQuote("6 qseqid sseqid staxids pident length mismatch gapopen qstart qend sstart send evalue bitscore")
)
 
############################
####CREATE BLAST RESULTS####
############################
 
tryCatch({
  system(blast_command, intern = TRUE)
  
  # Read results with fread and clean quotes
  blast_results.df <- fread(output_file, 
                            header = FALSE, 
                            sep = "\t",
                            quote = "")
  
  # Set column names
  colnames(blast_results.df) <- c("qseqid", "sseqid", "staxids", 
                                  "pident", "length", "mismatch", "gapopen", 
                                  "qstart", "qend", "sstart", "send", 
                                  "evalue", "bitscore")
  
  names_dmp <- read.table("data/names.dmp", sep="|", quote="", fill=TRUE, stringsAsFactors=FALSE)
  names_dmp <- names_dmp[names_dmp$V4 == "\tscientific name\t", ]
  names_dmp$V1 <- as.numeric(trimws(names_dmp$V1))
  names_dmp$V2 <- trimws(names_dmp$V2)
  
  blast_results.df <- merge(blast_results.df, names_dmp[, c(1,2)], 
                   by.x="staxids", by.y="V1", all.x=TRUE)
  names(blast_results.df)[ncol(blast_results.df)] <- "sscinames"
  
  
  # Clean all quotes from character columns
  char_cols <- names(blast_results.df)[sapply(blast_results.df, is.character)]
  
  blast_results.df[, (char_cols) := lapply(.SD, function(x) {
    gsub('"', '', x, fixed = TRUE)
  }), .SDcols = char_cols]
  
  # Store results and display in UI
  blast_results(blast_results.df)
  
  updateTabsetPanel(session, "tabs", selected = "soiresults")
  
}, error = function(e) {
  blast_results(paste("An error occurred while running BLAST:", e$message))
}, finally = {
  # Hide processing dialog
  removeModal()
})
  })
  
  species_list_path <- reactive({

    # -------------------
    # BLAST pipeline
    # -------------------
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
  
    
  
  output$slider_length_value <- renderText({
    paste(input$length_filter[1], "to", input$length_filter[2])
  })
  
  debounced_length_filter <- reactive({
    input$length_filter
  }) %>% debounce(1000)
  
  
  output$slider_pident_value <- renderText({
    paste(input$pident_filter[1], "to", input$pident_filter[2])
  })
  
  debounced_pident_filter <- reactive({
    input$pident_filter
  }) %>% debounce(1000)
  
  
  # Display results
  filtered_results <- reactive({
    req(blast_results())
    
    length_range <- debounced_length_filter()
    pident_range <- debounced_pident_filter()
    # Filter based on percent identity and length
    filtered <- blast_results()
    
    na_pident_rows <- filtered[is.na(filtered$pident), ]
    if(nrow(na_pident_rows) > 0) {
      print("Rows with NA in pident:")
      print(na_pident_rows)
    }
    
    # Filter by percent identity
    filtered <- filtered[!is.na(filtered$pident) & 
    filtered$pident >= pident_range[1] & 
    filtered$pident <= pident_range[2], ]
  
    
    filtered <- filtered[!is.na(filtered$length) & 
                           filtered$length >= length_range[1] & 
                           filtered$length <= length_range[2], ]
    
    print(filtered)
  
    species_path <- species_list_path() 
    
    
    SOI_list.df <- fread(species_path, header = TRUE, sep = "\t")
    
    print(SOI_list.df)
    
    SOI_list.df <- SOI_list.df %>% distinct(SOI, canonicalName)
    
    # Join BLAST results with SOI synonyms
    filtered <- filtered %>%
      left_join(
        SOI_list.df,
        by = c("sscinames" = "canonicalName")
      ) %>%
      filter(sscinames != "N/A") %>%
      mutate(SOI_flag = !is.na(SOI))
    
   print(filtered)
    
    return(filtered)
    
  })
  
  filtered_results2 <- reactive({
    req(filtered_results())
    
    filtered2 <- filtered_results()
  
  top_bitscore_per_asv <- filtered2 %>%
    group_by(qseqid) %>%
    summarise(bitscore = max(bitscore, na.rm = TRUE))
  
  # Merge to get counts of occurrences of top bitscore per ASV
  merged_df <- filtered2 %>%
    inner_join(top_bitscore_per_asv, by = c("qseqid", "bitscore")) %>% 
    select(qseqid, sscinames, SOI_flag) %>% 
    distinct()
  
  # Count occurrences of top bitscore per ASV
  asv_top_bitscore_counts <- merged_df %>%
    group_by(qseqid) %>%
    summarise(Top_bitscore_Count = n())
  
  # Merge back to indicate multiple instances
  result_df <- merged_df %>%
    inner_join(asv_top_bitscore_counts, by = "qseqid") %>%
    mutate(Multiple_Instances = Top_bitscore_Count > 1) %>%
    select(-Top_bitscore_Count)
  
 
})
  
  
  
  output$soi_species_boxes_likely <- renderUI({
  
    
    req(filtered_results2())
    soi_results <- filtered_results2() %>% filter(SOI_flag == TRUE) %>% filter(Multiple_Instances == FALSE)
    
    if (nrow(soi_results) > 0) {
      showModal(modalDialog(
        title = "Processing",
        "Loading species boxes (likely)... Please wait.",
        footer = NULL
      ))
    } else {
      return(NULL) # Stop processing if no data
    }
    
    species_list <- unique(soi_results$sscinames)
    
    box_list <- lapply(species_list, function(species) {
      asvs <- soi_results %>% filter(sscinames == species) %>% pull(qseqid) %>% unique()
      
      multiple_instances <- soi_results %>% filter(sscinames == species) %>% pull(Multiple_Instances) %>% dplyr::first()
      
      # Determine the color based on Multiple_Instances value
      asv_color <- ifelse(multiple_instances, "#98C7B1", "#716fa8")
      
      taxonKey <- tryCatch({
        key <- name_backbone(species)$usageKey
        if (!is.null(key)) key else NA
      }, error = function(e) NA)
      
      # Construct the hyperlink if taxonKey is found
      print(paste("Species:", species, "| TaxonKey:", taxonKey))
      
      # Construct the hyperlink if taxonKey is found
      species_link <- tags$a(
        href = "#",  # Prevents default action
        onclick = sprintf("window.open('%s', '_blank')", paste0("https://www.gbif.org/species/", taxonKey)),
        style = "font-size: 18px; font-weight: bold; color: #b0dce9; text-decoration: underline; cursor: pointer;",
        species
      )
      
      
      asv_content <- paste0("<span style='color:", asv_color, ";'> ASVs: ", paste(asvs, collapse = ", "), "</span>")
      
      box_content <- paste(species_link, "<br>", asv_content)
      
      div(
        style = "border: 1px solid #2171B5; padding: 10px; margin: 5px; background-color: #254550; width: 300px; display: inline-block; vertical-align: top;",
        HTML(box_content) 
      )
    })
    
    removeModal()
    
    do.call(tagList, box_list)
  })
  
  
  
  
  output$soi_species_boxes_potential <- renderUI({
    
    
    req(filtered_results2())
    soi_results <- filtered_results2() %>% filter(SOI_flag == TRUE) %>% filter(Multiple_Instances == TRUE)
    
    if (nrow(soi_results) > 0) {
      showModal(modalDialog(
        title = "Processing",
        "Loading species boxes (Putative)... Please wait.",
        footer = NULL
      ))
    } else {
      return(NULL) # Stop processing if no data
    }
    
    species_list <- unique(soi_results$sscinames)
    
    box_list <- lapply(species_list, function(species) {
      asvs <- soi_results %>% filter(sscinames == species) %>% pull(qseqid) %>% unique()
      
      multiple_instances <- soi_results %>% filter(sscinames == species) %>% pull(Multiple_Instances) %>% first()
      
      # Determine the color based on Multiple_Instances value
      asv_color <- ifelse(multiple_instances, "#98C7B1", "#716fa8")
      
      taxonKey <- tryCatch({
        key <- name_backbone(species)$usageKey
        if (!is.null(key)) key else NA
      }, error = function(e) NA)
      
      # Construct the hyperlink if taxonKey is found
      print(paste("Species:", species, "| TaxonKey:", taxonKey))
      
      # Construct the hyperlink if taxonKey is found
      species_link <- tags$a(
        href = "#",  # Prevents default action
        onclick = sprintf("window.open('%s', '_blank')", paste0("https://www.gbif.org/species/", taxonKey)),
        style = "font-size: 18px; font-weight: bold; color: #b0dce9; text-decoration: underline; cursor: pointer;",
        species
      )
      
      
      asv_content <- paste0("<span style='color:", asv_color, ";'> ASVs: ", paste(asvs, collapse = ", "), "</span>")
      
      box_content <- paste(species_link, "<br>", asv_content)
      
      div(
        style = "border: 1px solid #2171B5; padding: 10px; margin: 5px; background-color: #254550; width: 300px; display: inline-block; vertical-align: top;",
        HTML(box_content) 
      )
    })
    
    removeModal()
    
    do.call(tagList, box_list)
  })
  
################################  
####VISUALISATION RESULT TAB####
################################
  
  asv_table <- reactive({
    req(input$table_file)
    fread(input$table_file$datapath, header = TRUE)
  })
  
  
  
  # Read coordinates file
  coordinates <- reactive({
    req(input$coord_file)
    fread(input$coord_file$datapath, header = TRUE, sep = "\t")
  })
  
  
  output$debug_info <- renderText({
    req(asv_table(), filtered_results2())
    
    soi_results <- filtered_results2() %>%
      filter(SOI_flag == TRUE) %>%
      select(qseqid, sscinames)
    
    ASV_list <- unique(soi_results$qseqid)
    
    paste("Species of Interest ASVs found:", length(ASV_list),
          "\nTotal samples:", ncol(asv_table()) - 1)
  })
  
  #Download heatmap presence/absence data
  output$download_heatmap_data <- downloadHandler(
    filename = function() {
      paste0("SoI_heatmap_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(asv_table(), filtered_results2())
      
      # Get SOI results
      soi_results <- filtered_results2() %>% 
        filter(SOI_flag == TRUE) %>% 
        select(qseqid, sscinames)
      
      # Validate we have data
      shiny::validate(
        need(nrow(soi_results) > 0, "No Species of Interest found in filtered results")
      )
      
      ASV_list <- unique(soi_results$qseqid)
      
      # Filter ASV table
      filtered_asv_table_heatmap <- asv_table() %>%
        filter(ASV %in% ASV_list)
      
      # Prepare data for heatmap (same as plot)
      filtered_asv_table_heatmap_pa <- filtered_asv_table_heatmap %>%
        pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
        left_join(soi_results, by = c("ASV" = "qseqid")) %>%
        group_by(sscinames, SampleID) %>%
        summarise(Abundance = sum(Abundance), .groups = "drop") %>%
        mutate(PresenceAbsence = ifelse(Abundance > 0, "Present", "Absent"))
      
      # Optionally pivot to wide format for better readability
      heatmap_wide <- filtered_asv_table_heatmap_pa %>%
        select(sscinames, SampleID, PresenceAbsence) %>%
        pivot_wider(names_from = SampleID, values_from = PresenceAbsence, values_fill = "Absent")
      
      # Write to file
      write.csv(heatmap_wide, file, row.names = FALSE)
    }
  )
  
  
  # Server - SOI Heatmap
  output$SOI_heatmap <- renderPlot({
    req(asv_table())
    req(filtered_results2())
    
    # Get SOI results
    soi_results <- filtered_results2() %>% 
      filter(SOI_flag == TRUE) %>% 
      select(qseqid, sscinames)
    
    # Validate we have data
    shiny::validate(
      need(nrow(soi_results) > 0, "No species of interest found in filtered results")
    )
    
    ASV_list <- unique(soi_results$qseqid)
    
    # Filter ASV table
    summarise_by <- as.character(input$soi_grouping)[1]
    
    filtered_asv_table_heatmap <- asv_table() %>% 
      filter(ASV %in% ASV_list) # Validate we have matching ASVs validate( need(nrow(filtered_asv_table_heatmap) > 0, "No matching ASVs found in ASV table") )
    
    df_long <- filtered_asv_table_heatmap %>%
      pivot_longer(
        -ASV,
        names_to = "SampleID",
        values_to = "Abundance"
      ) %>%
      left_join(soi_results, by = c("ASV" = "qseqid")) %>%
      left_join(coordinates(), by = "SampleID")   # ensures Site + Region available
    
    # now group by the selected column and summarise
    heatmap_df <- df_long %>%
      group_by(sscinames, .data[[summarise_by]]) %>%   # dynamic grouping
      summarise(Abundance = sum(Abundance, na.rm = TRUE), .groups = "drop") %>%
      mutate(PresenceAbsence = as.integer(Abundance > 0))
    
    # Create heatmap
    ggplot(heatmap_df,
           aes(
             x = .data[[summarise_by]],
             y = sscinames,
             fill = factor(PresenceAbsence)
           )
    ) +
      geom_tile(color = "grey80", linewidth = 0.5) +
      scale_fill_manual(values = c("0" = "#d2d2d2", "1" = "#716fa8"), 
                        name = "Presence",
                        labels = c("Absent", "Present")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1, colour = "white"),
        axis.text.y = element_text(size = 14, face = "italic", colour = "white"),
        axis.title = element_text(size = 18, colour = "white"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size = 16, colour = "white"),
        legend.title = element_text(size = 16, colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
      ) +
      labs(x = "", y = "Species of Interest")
    
  }, bg = "transparent")
  
  output$SOI_heatmap_ui <- renderUI({
    req(asv_table(), filtered_results2())
    
    soi_results <- filtered_results2() %>%
      filter(SOI_flag == TRUE) %>% 
      select(qseqid, sscinames)
    
    ASV_list <- unique(soi_results$qseqid)
    

    # Calculate dimensions
    summarise_by <- input$soi_grouping
    
    # Need metadata for grouping
    df <- asv_table() %>% 
      filter(ASV %in% ASV_list) %>%
      pivot_longer(
        -ASV,
        names_to = "SampleID",
        values_to = "Abundance"
      ) %>% 
      left_join(soi_results,  by = c("ASV" = "qseqid")) %>%
      left_join(coordinates(), by = "SampleID")   # <-- Site + Region now exist!
    
    # Count unique grouping levels (SampleID, Site, or Region)
    group_count <- df %>% pull(!!sym(summarise_by)) %>% unique() %>% length()
    
    # Count SOI species
    species_count <- length(unique(soi_results$sscinames))
    
    # Dynamic heatmap size
    width_px  <- as.numeric(min(max(group_count * 120, 400), 10000))
    height_px <- as.numeric(min(max(species_count * 50, 400), 10000))
    
    
    plotOutput("SOI_heatmap", 
               width = paste0(width_px, "px"), 
               height = paste0(height_px, "px"))
  })
  
#Tally map
  
  soi_summary <- reactive({
    req(asv_table(), filtered_results2(), coordinates())
    
    soi_results <- filtered_results2() %>%
      filter(SOI_flag == TRUE) %>%
      select(qseqid, sscinames)
    
    ASV_list <- unique(soi_results$qseqid)
    
    # All ASV x sample rows
    df <- asv_table() %>%
      filter(ASV %in% ASV_list) %>%
      pivot_longer(
        -ASV,
        names_to = "SampleID",
        values_to = "Abundance"
      ) %>%
      left_join(soi_results, by = c("ASV" = "qseqid")) %>%
      left_join(coordinates(), by = "SampleID")  # <-- full coordinates here
    
    df
  })
  
  output$soi_map <- renderLeaflet({
    df <- soi_summary()
    req(nrow(df) > 0)
    
    # -----------------------------
    # CALCULATE CENTER AND BOUNDS USING RAW DATA
    # -----------------------------
    df_coords <- df %>% filter(!is.na(Latitude) & !is.na(Longitude))
    center_lat <- mean(df_coords$Latitude)
    center_lon <- mean(df_coords$Longitude)
    
    lon_range <- diff(range(df_coords$Longitude, na.rm = TRUE))
    lat_range <- diff(range(df_coords$Latitude, na.rm = TRUE))
    spread <- max(lon_range, lat_range)
    
    zoom_level <- case_when(
      spread > 30 ~ 3,   # very wide spread (continental scale)
      spread > 10 ~ 4,   # country scale
      spread > 2  ~ 5,   # regional scale
      TRUE        ~ 6    # zoom in if very tight cluster
    )
    
    # -----------------------------
    # Prepare summarised data for circle sizes
    # -----------------------------
    summarise_by <- input$soi_grouping
   
     df_summary <- df %>%
      filter(Abundance > 0) %>%
      distinct(!!sym(summarise_by), ASV, Latitude, Longitude) %>% 
      group_by(across(all_of(summarise_by))) %>%
      summarise(
        n = n(),
        Latitude = mean(Latitude, na.rm = TRUE),
        Longitude = mean(Longitude, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        radius = scales::rescale(n, to = c(5, 30))
      )
    
     df_summary <- df_summary %>%
       mutate(
         popup_text = paste0(
           summarise_by, ": ", .data[[summarise_by]],
           "<br># ASVs of species of interest: ", n
         )
       )
     
    # -----------------------------
    # Build the leaflet map
    # -----------------------------
    leaflet(df_summary) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~radius,
        color = "#716fa8",
        fillOpacity = 0.7,
        stroke = FALSE,
        labelOptions = labelOptions(direction = "auto"),
        popup = ~popup_text
      ) %>%
      setView(lng = center_lon, lat = center_lat, zoom = zoom_level)
  })
  
  
  
  
  
  observe({
    req(filtered_results2())  # Ensure data is available
    
    # Extract unique species names
    soi_results <- filtered_results2() %>% filter(SOI_flag == TRUE)
    species_list <- unique(soi_results$sscinames)
    
    # Update selectInput choices dynamically
    updateSelectInput(session, "species_choice", choices = species_list, selected = NULL)
  })
  
  selected_asvs <- reactive({
    req(input$species_choice, filtered_results2(), asv_table())
    
    soi_results <- filtered_results2()
    
    asvs_for_species <- soi_results %>%
      filter(sscinames == input$species_choice) %>%
      pull(qseqid) %>%
      unique()
    
    filtered_asv_table <- asv_table() %>%
      select(c("ASV", everything())) %>%
      filter(ASV %in% asvs_for_species)
    
    return(filtered_asv_table)
  })
  
  # Merge ASV presence with coordinates
  merged_data <- reactive({
    req(selected_asvs(), coordinates())
    
    # Sum ASV abundance per sample
    asv_presence <- selected_asvs() %>%
      pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
      group_by(SampleID) %>%
      summarise(SOI_Present = sum(Abundance) > 0,
                Abundance = sum(Abundance))  # If sum > 0, SOI is present
    
    
    
    merged <- coordinates() %>%
      left_join(asv_presence, by = c("SampleID" = "SampleID")) %>%
      group_by(Site, Latitude,	Longitude)  %>%
      summarise(SOI_Present = sum(Abundance) > 0,
                Abundance = sum(Abundance)) %>%
      mutate(SOI_Present = ifelse(is.na(SOI_Present), FALSE, SOI_Present))  # Default to FALSE if no data
    
  
    return(merged)
  })
  
  # Render the map
  output$map <- renderLeaflet({
    merged <- merged_data()
    req(merged)
    
    center_lon <- mean(merged$Longitude, na.rm = TRUE)
    center_lat <- mean(merged$Latitude, na.rm = TRUE)
    
    # Calculate spread (approximate degrees covered)
    lon_range <- diff(range(merged$Longitude, na.rm = TRUE))
    lat_range <- diff(range(merged$Latitude, na.rm = TRUE))
    spread <- max(lon_range, lat_range)
    
    zoom_level <- case_when(
      spread > 30 ~ 3,   # very wide spread (continental scale)
      spread > 10 ~ 4,   # country scale
      spread > 2  ~ 5,   # regional scale
      TRUE        ~ 6    # zoom in if very tight cluster
    )
    
    leaflet(data = merged) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Longitude, ~Latitude,
        color = ~ifelse(SOI_Present, "#716fa8", "#a3a3a3"),
        fillColor = ~ifelse(SOI_Present, "#716fa8", "#a3a3a3"),  # Fill color
        fillOpacity = ~ifelse(SOI_Present, 1, 0),
        radius = 6,
        weight = 1.5,
        popup = ~paste("Name:", Site, "<br>Read Counts:", Abundance)
      ) %>%
      setView(lng = center_lon, lat = center_lat, zoom = zoom_level)
  })

  
################################  
####ADD NEW DATA TO EXISTING####
################################  
  
  # UI for selecting likely species
  output$select_likely_species_ui <- renderUI({
    req(filtered_results2())
    
    soi_results <- filtered_results2() %>% 
      filter(SOI_flag == TRUE, Multiple_Instances == FALSE)
    
    species_list <- unique(soi_results$sscinames)
    
    if (length(species_list) == 0) {
      return(p("No likely Species of Interest found", style = "color: white;"))
    }
    
    checkboxGroupInput("selected_likely_species",
                       label = NULL,
                       choices = sort(species_list),
                       selected = NULL)
  })
  
  # UI for selecting Putative species
  output$select_potential_species_ui <- renderUI({
    req(filtered_results2())
    
    soi_results <- filtered_results2() %>% 
      filter(SOI_flag == TRUE, Multiple_Instances == TRUE)
    
    species_list <- unique(soi_results$sscinames)
    
    if (length(species_list) == 0) {
      return(p("No putative Species of Interest found", style = "color: white;"))
    }
    
    checkboxGroupInput("selected_potential_species",
                       label = NULL,
                       choices = sort(species_list),
                       selected = NULL)
  })
  
  # Reactive to store the editable table with metadata
  pa_table_with_metadata <- reactiveVal(NULL)
  
  # Generate editable table with metadata in one step
  observeEvent(input$generate_editable_table, {
    req(filtered_results2(), asv_table(), coordinates())
    
    # Combine selected species from both categories
    selected_species <- c(input$selected_likely_species, input$selected_potential_species)
    
    shiny::validate(
      need(length(selected_species) > 0, "Please select at least one species")
    )
    
    # Get ASVs for selected species
    soi_results <- filtered_results2() %>%
      filter(SOI_flag == TRUE, sscinames %in% selected_species) %>%
      select(qseqid, sscinames, Multiple_Instances)
    
    shiny::validate(
      need(nrow(soi_results) > 0, "No ASVs found for selected species")
    )
    
    # Get unique ASVs
    asv_list <- unique(soi_results$qseqid)
    
    # Filter ASV table
    filtered_asv <- asv_table() %>%
      filter(ASV %in% asv_list)
    
    # Create presence/absence table
    pa_table <- filtered_asv %>%
      pivot_longer(-ASV, names_to = "SampleID", values_to = "Abundance") %>%
      left_join(soi_results, by = c("ASV" = "qseqid")) %>%
      group_by(sscinames, SampleID) %>%
      summarise(
        Total_Abundance = sum(Abundance),
        .groups = "drop"
      )
    
    print(pa_table)
    
    # Add coordinates
    pa_table_coord <- left_join(pa_table, coordinates(), by = "SampleID") %>% 
      group_by(SampleID, Site, Region, Latitude, Longitude, sscinames) %>%
      summarise(
        Total_Abundance = sum(Total_Abundance),
        Presence = ifelse(sum(Total_Abundance) > 0, "Present", "Absent"),
        .groups = "drop"
      ) %>%
      select(Species = sscinames, SampleID, Site, Region, Presence, Latitude, Longitude) %>%
      arrange(Species, Site)
    
    # Add metadata columns
    if (input$metadata_source == "manual") {
      pa_table_coord$Year <- input$year_fill
      pa_table_coord$Gene <- input$gene_fill
      pa_table_coord$Methodology <- input$methodology_fill
    } else if (input$metadata_source == "upload") {
      req(input$metadata_file)
      
      uploaded_metadata <- read.csv(input$metadata_file$datapath, stringsAsFactors = FALSE)
      
      
      shiny::validate(
        need("SampleID" %in% colnames(uploaded_metadata),
             "Uploaded metadata must include a 'SampleID' column for joining.")
      )
      
      # Join metadata by SampleID
      pa_table_coord <- left_join(pa_table_coord, uploaded_metadata, by = "SampleID")
    } 
      
    
    # Reorder columns
    pa_table_coord <- pa_table_coord %>%
      select(Species, SampleID, Site, Region, Presence, Gene, Latitude, Longitude, Year, Methodology)
    
    pa_table_with_metadata(pa_table_coord)
  })
  
  # Render editable table with white text
  output$editable_pa_table <- renderDT({
    req(pa_table_with_metadata())
    
    datatable(
      pa_table_with_metadata(),
      editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4, 5))),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().container()).css({'color': 'white'});",
          "}"
        )
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = 1:ncol(pa_table_with_metadata()),
        color = 'white',
        backgroundColor = 'transparent'
      )
  })
  

  # Handle cell edits
  observeEvent(input$editable_pa_table_cell_edit, {
    info <- input$editable_pa_table_cell_edit
    
    print(paste("Edit detected - Row:", info$row, "Col:", info$col, "Value:", info$value))  # DEBUG
    
    updated_data <- pa_table_with_metadata()
    
    # DT sends row as 1-indexed (row 1 = first row in data frame)
    # DT sends col as 0-indexed (col 0 = first column)
    row_index <- info$row
    col_index <- info$col + 1
    
    print(paste("Updating row:", row_index, "col:", col_index))  # DEBUG
    
    # Get the column name
    col_name <- colnames(updated_data)[col_index]
    
    print(paste("Column name:", col_name))  # DEBUG
    
    # Convert value to appropriate type based on column
    new_value <- if (col_name %in% c("Latitude", "Longitude", "Total_Abundance")) {
      as.numeric(info$value)
    } else {
      as.character(info$value)
    }
    
    print(paste("New value:", new_value, "Type:", class(new_value)))  # DEBUG
    
    # Update the cell
    updated_data[row_index, col_index] <- new_value
    
    print(paste("Updated cell. New table preview:"))  # DEBUG
    print(head(updated_data))  # DEBUG
    
    pa_table_with_metadata(updated_data)
  })
  
  # Download P/A table
  output$download_pa_table <- downloadHandler(
    filename = function() {
      paste0("SoI_presence_absence_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(pa_table_with_metadata())
      write.csv(pa_table_with_metadata(), file, row.names = FALSE)
    }
  )
  
  
  observeEvent(input$add_to_historic, {
    req(pa_table_with_metadata())
    
    showModal(modalDialog(
      title = "Confirm Addition",
      "Are you sure you want to add this data to the historic dataset? This will update the existing historic.csv file.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_historic", "Confirm", class = "btn-success")
      )
    ))
  })
  
  
  # Confirm adding to historic data - USE SAME DATA AS DOWNLOAD
  observeEvent(input$confirm_add_historic, {
    req(pa_table_with_metadata())
    
    tryCatch({
      # Use the exact same data that would be downloaded
      new_data <- pa_table_with_metadata()
      
      print("=== Data being added to historic ===")
      print(new_data)
      
      new_data$DateAdded <- as.character(Sys.Date())
      
      # Ensure Year is character for consistency
      new_data$Year <- as.character(new_data$Year)
      new_data$Methodology <- as.character(new_data$Methodology)
      
      # Get existing historic data
      current_historic <- historic_data()
      
      # Combine datasets
      if (nrow(current_historic) > 0) {
        all_cols <- union(colnames(current_historic), colnames(new_data))
        
        # Ensure Year is character in existing data too
        if ("Year" %in% colnames(current_historic)) {
          current_historic$Year <- as.character(current_historic$Year)
        }
        if ("Methodology" %in% colnames(current_historic)) {
          current_historic$Methodology <- as.character(current_historic$Methodology)
        }
        if ("DateAdded" %in% colnames(current_historic)) {
          current_historic$DateAdded <- as.character(current_historic$DateAdded)
        }
        
        # Add missing columns with NA
        for (col in all_cols) {
          if (!col %in% colnames(current_historic)) {
            current_historic[[col]] <- NA_character_
          }
          if (!col %in% colnames(new_data)) {
            new_data[[col]] <- NA_character_
          }
        }
        
        updated_historic <- bind_rows(current_historic, new_data)
      } else {
        updated_historic <- new_data
      }
      
      # Remove duplicates 
      updated_historic <- updated_historic %>%
        arrange(desc(DateAdded)) %>%
        distinct(Species, SampleID, Site, Region, Year, Methodology, .keep_all = TRUE)  # Removed Year from distinct
      
      if (!dir.exists("data")) {
        dir.create("data")
      }
      
      write.csv(updated_historic, "data/historic.csv", row.names = FALSE)
      historic_data(updated_historic)
      
      removeModal()
      
      showModal(modalDialog(
        title = "Success",
        paste("Successfully added records to historic data."),
        footer = modalButton("OK")
      ))
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        paste("Failed to update historic data:", e$message),
        footer = modalButton("OK")
      ))
    })
  })
  
#####################
####Long Term DATA####  
#####################  
  
  # Dynamic UI for historic data filters
  output$historic_species_select <- renderUI({
    data <- historic_data()
    
    if (nrow(data) == 0) {
      return(selectInput("species", "Select Species:", choices = c("No data available" = "")))
    }
    
    selectInput(
      inputId = "species",
      label = "Select Species:",
      choices = c("Select species" = "", unique(data$Species)),
      selected = ""
    )
  })
  
  
  output$historic_gene_select <- renderUI({
    data <- historic_data()
    
    if (nrow(data) == 0) {
      return(selectInput("gene", "Select Gene:", choices = c("No data available" = "")))
    }
    
    selectInput(
      inputId = "gene",
      label = "Select Gene:",
      choices = c("Select Gene" = "", unique(data$Gene)),
      selected = ""
    )
  })
  
  
  output$historic_method_select <- renderUI({
    data <- historic_data()
    
    if (nrow(data) == 0 || !"Methodology" %in% colnames(data)) {  # Changed Method to Methodology
      return(selectInput("samplemethod", "Select Sampling Method:", choices = c("No data available" = "")))
    }
    
    selectInput(
      inputId = "samplemethod",
      label = "Select Sampling Method:",
      choices = c("Select Method" = "", "All", unique(data$Methodology)),  # Changed Method to Methodology
      selected = ""
    )
  })
  
  output$historic_year_slider <- renderUI({
    data <- historic_data()
    
    if (nrow(data) == 0 || !"Year" %in% colnames(data)) {
      return(p("No year data available", style = "color: white;"))
    }
    
    min_year <- min(data$Year, na.rm = TRUE)
    max_year <- max(data$Year, na.rm = TRUE)
    
    sliderInput(
      "year_range",
      "Select Year Range:",
      min = min_year,
      max = max_year,
      value = c(min_year, max_year),
      step = 1,  # Changed from calculation to 1
      sep = ""
    )
  })
  
  # Reactive filter for user selections
historic_filtered_data <- reactive({
  req(input$species, input$gene, input$year_range, input$samplemethod)
  
  data <- historic_data()
  
  # Simple filter without pivot if Presence column already exists
  if ("Presence" %in% colnames(data)) {
    result <- data %>%
      filter(
        Species == input$species,
        as.numeric(Year) >= input$year_range[1], 
        as.numeric(Year) <= input$year_range[2],
        if (input$samplemethod != "All") Methodology == input$samplemethod else TRUE
      )
    return(result)
  }
  
  # If no Presence column, just return filtered data
  result <- data %>%
    filter(
      as.numeric(Year) >= input$year_range[1], 
      as.numeric(Year) <= input$year_range[2],
      if (input$samplemethod != "All") Methodology == input$samplemethod else TRUE
    )
  return(result)
})



  
  # Render Leaflet Map
  output$historic_map <- renderLeaflet({
    data <- historic_data()  # Call the reactive function
    
    if (nrow(data) == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 0, zoom = 2)
    } else {
      leaflet() %>%
        addTiles() %>%
        setView(lng = mean(data$Longitude, na.rm = TRUE), 
                lat = mean(data$Latitude, na.rm = TRUE), 
                zoom = 6)
    }
  })
  
  # Add markers dynamically
  observe({
    filtered_data <- historic_filtered_data()
    
    if (nrow(filtered_data) == 0 || is.null(filtered_data)) {
      print("No data available for the selected species.")
      return()
    }
    
    filtered_data_map <- filtered_data %>% 
      filter(Presence == "Present" & Species == input$species & Gene == input$gene)  # Changed "Yes" to "Present"
    
    print("Filtered data:")
    print(filtered_data_map)
    
    
    if (nrow(filtered_data_map) > 0) {
      center_lon <- mean(filtered_data_map$Longitude, na.rm = TRUE)
      center_lat <- mean(filtered_data_map$Latitude, na.rm = TRUE)
      
      lon_range <- diff(range(filtered_data_map$Longitude, na.rm = TRUE))
      lat_range <- diff(range(filtered_data_map$Latitude, na.rm = TRUE))
      spread <- max(lon_range, lat_range)
      
      zoom_level <- case_when(
        spread > 30 ~ 3,
        spread > 10 ~ 4,
        spread > 2  ~ 5,
        TRUE        ~ 6
      )
      
      leafletProxy("historic_map", data = filtered_data_map) %>%
        clearMarkers() %>%
        addCircleMarkers(
          lng = ~Longitude, lat = ~Latitude,
          radius = 6, weight = 1.5,
          color = "#716fa8",
          fillColor = "#716fa8",
          fillOpacity = 1,
          layerId = ~as.character(Site)
        ) %>%
        setView(lng = center_lon, lat = center_lat, zoom = zoom_level)
    }
  })
  
  # Generate heatmap when a marker is clicked
  observeEvent(input$historic_map_marker_click, {
    print("Marker Clicked:")
    print(input$historic_map_marker_click)
    
    if (!is.null(input$historic_map_marker_click)) {
      clicked_site <- input$historic_map_marker_click$id
      print(paste("Clicked site:", clicked_site))
      
      site_data <- historic_filtered_data() %>%
        filter(
          Site == clicked_site,
          Species == input$species,
          input$samplemethod == "All" | Methodology == input$samplemethod
        ) %>%
        group_by(Year, Site) %>%
        summarize(Presence = ifelse("Present" %in% Presence, "Present", "Absent"), .groups = "drop")
      
      print(site_data)
      
      if (nrow(site_data) > 0) {
        heatmap_plot <- ggplot(site_data, aes(x = as.factor(Year), y = Site, fill = Presence)) +
          geom_tile(color = "white") +
          scale_fill_manual(values = c("Absent" = "#d2d2d2", "Present" = "#716fa8")) +
          theme_minimal() +
          labs(title = paste("Species of Interest Presence at", clicked_site), x = "Year", y = "Site")
        
        showModal(modalDialog(
          title = paste("Heatmap for", clicked_site),
          plotOutput("historic_heatmap"),
          easyClose = TRUE
        ))
        
        output$historic_heatmap <- renderPlot({ heatmap_plot })
      } else {
        print("No data for this site!")
      }
    } else {
      print("Click event is NULL!")
    }
  })
  
  output$historic_plot <- renderPlot({
    req(input$species, input$gene, input$year_range)
    
    data <- historic_data()
    
    print(" data:")
    print(data)
    
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    # Filter data for selected species and date range
    filtered_data <- data %>%
      filter(
        Species == input$species,
        Gene == input$gene,
        as.numeric(Year) >= input$year_range[1], 
        as.numeric(Year) <= input$year_range[2],
        if (input$samplemethod != "All") Methodology == input$samplemethod else TRUE
      )
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    
    # Standardize Presence values FIRST
    filtered_data <- filtered_data %>%
      mutate(Presence = case_when(
        Presence %in% c("Yes", "Present", "TRUE", "1", TRUE) ~ "Present",
        Presence %in% c("No", "Absent", "FALSE", "0", FALSE) ~ "Absent",
        TRUE ~ as.character(Presence)
      ))
    
    # Count Present vs Absent by Year
    plot_bar_data <- filtered_data %>%
      group_by(Year, Presence) %>%
      summarise(Count = n(), .groups = "drop")
    
    
    # Create the bar plot
    ggplot(plot_bar_data, aes(x = factor(Year), y = Count, fill = factor(Presence))) +
      geom_bar(stat = "identity", position = "stack") +
      labs(x = "Year", y = "Site Count", fill = "Presence") +
      scale_fill_manual(values = c("Absent" = "#d2d2d2", "Present" = "#716fa8")) +
      scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::number_format(accuracy = 1)) +
      theme(legend.position = "top") +
      ggtitle(bquote(Presence/Absence~of~italic(.(input$species))~by~Year)) +
      theme(
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA), 
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.margin = margin(0, 0, 0, 0),
        axis.title = element_text(size = 18, colour = "white"),
        axis.text = element_text(size = 16, colour = "white"),
        plot.title = element_text(size = 20, colour = "white"),
        legend.text = element_text(size = 16, colour = "white"),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()
      )
  }, bg = "transparent")
  
  
####################
####OUTPUT FILES####
####################

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
  
  #############################  
  ####DATABASE CONSTRUCTION####
  ############################# 
  
  queries <- reactiveVal(list())
  show_results <- reactiveVal(FALSE)
  
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
  
#Polygon for WKT coordinates
  polygon_wkt <- reactiveVal(NULL)
  
  # Observe when a shape is drawn
  observeEvent(input$map_draw_new_feature, {
    req(input$map_draw_new_feature)
    print(input$map_draw_new_feature)
    
    # Convert the drawn feature to WKT
    feature <- input$map_draw_new_feature
    geojson <- feature$geometry  # GeoJSON format of the drawn feature
    
    # Flatten the coordinates and ensure it's numeric
    coords <- unlist(geojson$coordinates)  # Flatten the coordinates
    
    # Convert to matrix or array for st_polygon
    coords_matrix <- matrix(coords, ncol = 2, byrow = TRUE)  # Create a matrix with two columns (lat, lon)
    
    # Convert the matrix to an sf polygon
    polygon_sf <- st_polygon(list(coords_matrix)) %>%
      st_sfc(crs = 4326)  # Assign the correct CRS
    
    # Convert to WKT using wk
    wkt <- wk::as_wkt(polygon_sf)
    
    
    polygon_wkt(wkt)
    
    # Display WKT output
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
  
  
  output$wkt_output <- renderText({
    paste("WKT coordinates: ", selected_wkt())
  })
  
  # Clear drawn features
  observeEvent(input$clear, {
    leafletProxy("database_map") %>% clearShapes()
    output$wkt_output <- renderText("")
  })
  
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
 
################################# 
####DATABASE CREATION OPTIONS####
################################# 
  
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
  
  
  
  ##########################################      
  ####Presence/Absence in BLAST database####  
  ##########################################
  
  species_presence <- reactive({
    req(input$species_file)
    req(species_dataframe_rv())   
    
    species_list <- readLines(input$species_file$datapath)
    species_list <- trimws(species_list)
    
    
    cleaned_data <- read.delim("intermediate/merged_combined_output.tax.cleaned.final.tsv", header = F, sep = "\t")
    species_column <- cleaned_data[, 10]   # species column
    
    df <- species_dataframe_rv()
    
    presence_df <- df %>%
      dplyr::mutate(
        Present = canonicalName %in% species_column
      ) %>%
      dplyr::group_by(SOI) %>%
      dplyr::summarise(
        Presence = ifelse(any(Present), "Yes", "No")
      )
    
    colnames(presence_df) <- c("Species", "Presence")
    percentage_present <- (sum(presence_df$Presence == "Yes") / nrow(presence_df)) * 100
    
    list(presence_df = presence_df, percentage_present = percentage_present)
  })
  
  output$presence_table <- renderTable({
    req(species_presence())
    species_presence()$presence_df
  })
  
  #Downlaod table
  output$download_presence <- downloadHandler(
    filename = function() paste0(db_name(), ".species_presence.csv" ),
    content = function(file) {
      write.csv(species_presence()$presence_df, file, row.names = FALSE)
    }
  )
  
  output$dynamic_heatmap <- renderUI({
    req(species_presence())
    
    species_count <- nrow(species_presence()$presence_df)
    plot_height <- max(300, species_count * 30)  # Ensure a minimum height of 300px
    
    plotOutput("heatmap", height = paste0(plot_height, "px"))
  })
  
  
  # Render Heat Map
  output$heatmap <- renderPlot({
    req(species_presence())
    
    heatmap_data <- species_presence()$presence_df
    print(heatmap_data)
    
    heatmap_data <- as.data.frame(heatmap_data)
    
    # Create heatmap
    library(ggplot2)
    ggplot(heatmap_data, aes(x = "Presence", y = Species, fill = factor(Presence))) +
      geom_tile(colour = "grey80", linewidth = 1) +
      scale_fill_manual(values = c("No" = "#d2d2d2", "Yes" = "#716fa8"), name = "Presence") +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14)) +
      theme(legend.position = "") +
      labs(
        x = "",
        y = "Species of Interest"
      )+ 
      theme(panel.background = element_rect(fill = "transparent", color = NA),  # Transparent background
            plot.background = element_rect(fill = "transparent", color = NA), 
            legend.background = element_rect(fill = "transparent", color = NA), # Transparent background
            plot.margin = margin(0, 0, 0, 0),
            axis.title = element_text(size = 18, colour = "white"),
            axis.text = element_text(size = 16, colour = "white"),
            plot.title = element_text(size = 20, colour = "white"),
            legend.text = element_text(size= 16, colour = "white"),
            legend.title = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            strip.text = element_blank())
  }, bg = "transparent")
  
  
  output$species_summary <- renderText({
    species_info <- species_presence()
    sprintf("Percentage of species from list present in the database: %.2f%%", species_info$percentage_present)
  })
  
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
  
observeEvent(input$generate_run, {
  
  req(input$species_file)
  
  showModal(modalDialog(
    title = "Processing",
    "Running Database Construction... Please wait.",
    footer = NULL
  ))
  
  ######################## 
  ####CREATE TAXA LIST####
  ######################## 
  
  species_list <- readLines(input$species_file$datapath) %>% trimws()
  genera <- unique(sapply(species_list, function(x) strsplit(x, " ")[[1]][1]))
  
  #Get family taxon IDs
  get_family_taxon_id <- function(species) {
    result <- name_backbone(name = species)
    if (!is.null(result$familyKey)) result$familyKey else NA
  }
  
  family_taxon_ids <- unique(na.omit(sapply(species_list, get_family_taxon_id)))
  # Special case adjustment
  if (any(family_taxon_ids %in% c(7336, 7334))) {
    family_taxon_ids <- unique(c(family_taxon_ids, 8596, 8597))
  }
  
  if (length(family_taxon_ids) == 0) {
    showNotification("No valid taxon IDs found!", type = "error")
    removeModal()
    return(NULL)
  }
  
  #Submit GBIF download based on geographic coordinates
  wkt_polygon <- selected_wkt()
  if (is.null(wkt_polygon) || nchar(wkt_polygon) < 10) {
    showNotification("No valid polygon selected. Draw a region first!", type = "error")
    removeModal()
    return(NULL)
  }
  
  # Create cache directory if it doesn't exist
  cache_dir <- file.path(getwd(), "gbif_cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  # Create cache key from taxon IDs and WKT polygon
  cache_key <- digest::digest(list(family_taxon_ids, wkt_polygon), algo = "md5")
  cache_file_path <- file.path(cache_dir, paste0(cache_key, ".rds"))  # Renamed to avoid confusion
  
  # Check if cache exists and is less than 30 days old
  use_cache <- FALSE
  if (file.exists(cache_file_path)) {
    cache_age <- difftime(Sys.time(), file.info(cache_file_path)$mtime, units = "days")
    if (cache_age < 30) {
      use_cache <- TRUE
      cached_data <- readRDS(cache_file_path)
      gbif_results(cached_data)
      showNotification("Using cached GBIF data", type = "message", duration = 5)
    }
  }
  
  if (!use_cache) {
    req_download <- occ_download(
      pred_and(pred_in("taxonKey", family_taxon_ids),
               pred("geometry", wkt_polygon)),
      format = "SIMPLE_CSV",
      user = Sys.getenv("GBIF_USER"),
      pwd = Sys.getenv("GBIF_PWD"),
      email = Sys.getenv("GBIF_EMAIL")
    )
    gbif_request_id(req_download)
    showNotification("GBIF download started…", type = "message", duration = 5)
  }
  
  #Get information from GBIF
  observe({
    req(gbif_request_id())
    invalidateLater(5000)
    
    meta <- tryCatch(
      occ_download_meta(gbif_request_id()),
      error = function(e) return(NULL)
    )
    
    if (!is.null(meta)) {
      if (meta$status == "SUCCEEDED") {
        zipfile <- occ_download_get(gbif_request_id())
        zip_contents <- unzip(zipfile, list = TRUE)$Name
        occ_file <- zip_contents[grepl("\\.csv$", zip_contents)][1]
        if (!is.na(occ_file)) {
          occ <- read.delim(unz(zipfile, occ_file), sep = "\t", stringsAsFactors = FALSE)
          gbif_results(occ)
          # Save to cache with explicit error handling
          tryCatch({
            saveRDS(occ, cache_file_path)
            showNotification(paste("Cached to:", cache_file_path), type = "message", duration = 3)
          }, error = function(e) {
            showNotification(paste("Cache save failed:", e$message), type = "warning", duration = 5)
          })
        }
        gbif_request_id(NULL)
      }
    }
  })
  
  observeEvent(gbif_results(), {
    occ <- gbif_results()
    if (!is.null(occ) && "species" %in% colnames(occ)) {
      additional_species_rv(unique(occ$species[!is.na(occ$species) & nzchar(occ$species)]))
    }
  })
    
    #Get synonyms
    observeEvent(species_list, {
      df <- do.call(
        rbind,
        lapply(species_list, function(sp) {
          Sys.sleep(0.2)
          get_synonyms_gbif(sp)
        })
      )
      species_dataframe_rv(df)
      
      write.table(df, paste0("databases/", db_name(), "_synonyms.txt"),
                  sep = "\t", row.names = FALSE, quote = FALSE)
    })
    
    #Compute taxa list
    taxa_ready <- reactive({
      req(genera, additional_species_rv(), species_dataframe_rv())
      unique(c(genera, additional_species_rv(), species_dataframe_rv()$canonicalName))
    })
  
    observeEvent(taxa_ready(), {
      taxa_vector <- taxa_ready()
      
      query_mode <- input$query_mode
      selected_sources <- input$source
      
#####################
####NCBI DOWNLOAD####
#####################
      
      query_commands <- if (query_mode == "add") {
        lapply(queries(), function(q) paste0(q$query, "[", q$field_type, "]"))
      } else {
        lapply(seq_along(queries()), function(i) {
          paste0(input[[paste0("query_", i)]], "[", input[[paste0("field_type_", i)]], "]")
        })
      }
      final_query_str <- paste(query_commands, collapse = " OR ")
      output$query_output <- renderText({ final_query_str })
      
      all_exec_output <- NULL
      
      if ("ncbi" %in% selected_sources) {
        chunk_size <- 25
        chunks <- split(taxa_vector, ceiling(seq_along(taxa_vector)/chunk_size))
        for (i in seq_along(chunks)) {
          taxa_chunk <- chunks[[i]]
          taxa_chunk_query <- paste0(taxa_chunk, "[ORGN]", collapse = " OR ")
          final_chunk_query <- paste(taxa_chunk_query, "AND (", final_query_str, ") AND (200 [SLEN] :50000[SLEN])")
          ncbi_command <- paste(crabs_path,
                                "--download-ncbi",
                                "--database nucleotide",
                                "--query", shQuote(final_chunk_query),
                                "--output", paste0("intermediate/ncbi_", i, ".fasta"),
                                "--email", ncbi_email,
                                "--batchsize 5000")
          system(ncbi_command, wait = TRUE)
        }
        system("cat intermediate/ncbi_*.fasta > intermediate/merged_ncbi_output.fasta")
        system("awk '{if (/>.*/) {print} else { sub(/^N*/, \"\"); sub(/N*$/, \"\"); print}}' intermediate/merged_ncbi_output.fasta > intermediate/merged_ncbi_output_cleaned.fasta")
        system(paste(crabs_path, "--import --import-format ncbi --input intermediate/merged_ncbi_output_cleaned.fasta --output intermediate/merged_ncbi_output.tax.tsv --acc2tax data/nucl_gb.accession2taxid --nodes data/nodes.dmp --names data/names.dmp --ranks 'superkingdom;phylum;class;order;family;genus;species'"))
      }
  
#####################
####BOLD DOWNLOAD####
#####################
      
      genera_vector <- unique(sub(" .*", "", taxa_vector)) #To minimise the chances of the API crashing 
      
      if ("bold" %in% selected_sources) {
        all_exec_output <- character()
        
        for (i in seq_along(genera_vector)) {
          
          taxon <- genera_vector[i]
          
          print(paste("Downloading genus:", taxon))
          
          bold_command <- paste(
            crabs_path,
            "--download-bold", #uses BOLD v5
            "--taxon", shQuote(taxon),
            "--output", shQuote(paste0("intermediate/bold_", taxon, ".tsv")),
            "--marker", shQuote(input$bold_marker)
          )
          
          bold_exec_output <- tryCatch({
            system(bold_command, intern = TRUE, wait = TRUE)
          }, error = function(e) {
            paste("Error executing command for genus", taxon, ":", e$message)
          })
          
          all_exec_output <- c(all_exec_output, bold_exec_output)
          
          if (i < length(genera_vector)) {
            Sys.sleep(runif(1, min = 3, max = 8))  # Wait 2 seconds between requests
          }
        }
        
        # Merge and clean the downloaded BOLD sequences
        all_bold_files <- list.files(pattern = "intermediate/^bold_.*\\.tsv$", full.names = TRUE)
        if (length(all_bold_files) > 0) {
          # Read all files and combine, filling missing columns with NA
          combined_data <- lapply(all_bold_files, function(file) {
            df <- read.table(file, header = TRUE, sep = "\t", quote = "", 
                             fill = TRUE, stringsAsFactors = FALSE, 
                             colClasses = "character")  
            return(df)
          }) %>%
            bind_rows()
        #filter for species/genera in taxa_vector after having to download using the genera  
          combined_data <- combined_data[
            combined_data$genus %in% taxa_vector | 
              combined_data$species %in% taxa_vector,
          ]
          
          write.table(combined_data, "intermediate/bold_combined.tsv", sep = "\t", quote = FALSE, row.names = FALSE)
        } 
        
        # Run CRABS import
        system(paste(crabs_path,
                     "--import --import-format BOLDV5",
                     "--input intermediate/bold_combined.tsv",
                     "--output intermediate/merged_bold_output.tax.tsv",
                     "--acc2tax data/nucl_gb.accession2taxid",
                     "--nodes data/nodes.dmp",
                     "--names data/names.dmp",
                     "--ranks 'superkingdom;phylum;class;order;family;genus;species'"))
      }
      
###################### 
####SILVA DOWNLOAD####      
######################
      
      if ("silva" %in% selected_sources) {
        
        print("Downloading SILVA database...")
        
        system(paste(
          crabs_path,
          "--download-silva",
          "--output intermediate/silva_138.2_subset.fasta",
          "--gene", shQuote(input$silva_marker),
          "--db-type subset",
          "--db-version 138.2"
        ))
        
        # Check if download succeeded
        if (file.exists("intermediate/silva_138.2_subset.fasta")) {
          
          print("Importing SILVA data...")
          
          # Run CRABS import
          system(paste(crabs_path,
                       "--import --import-format silva",
                       "--input intermediate/silva_138.2_subset.fasta",
                       "--output intermediate/silva_output.tax.tsv",
                       "--acc2tax data/nucl_gb.accession2taxid",
                       "--nodes data/nodes.dmp",
                       "--names data/names.dmp",
                       "--ranks 'superkingdom;phylum;class;order;family;genus;species'"))
          
          # Filter the imported SILVA file for species in the list
          if (file.exists("intermediate/silva_output.tax.tsv")) {
            silva_data <- read.table("intermediate/silva_output.tax.tsv", header = TRUE, sep = "\t", 
                                     quote = "", comment.char = "",
                                     fill = TRUE, stringsAsFactors = FALSE, 
                                     colClasses = "character")
            
            silva_data <- silva_data[
              silva_data[, 9] %in% taxa_vector | 
                silva_data[, 10] %in% taxa_vector,
            ]
            
            write.table(silva_data, "intermediate/merged_silva_output.tax.tsv", sep = "\t", 
                        quote = FALSE, row.names = FALSE, col.names = FALSE)
            
            print(paste("Filtered SILVA data:", nrow(silva_data), "sequences"))
          }
        } else {
          print("SILVA download failed")
        }
      }
      
######################### 
####CUSTOM FASTA FILE####     
#########################
      
      if (!is.null(input$custom_fasta)) {
        custom_fasta_path <- input$custom_fasta$datapath
        
        system(paste(crabs_path,
                     "--import --import-format BOLDV3",
                     "--input", shQuote(custom_fasta_path),
                     "--output intermediate/custom_fasta_output.tax.tsv",
                     "--acc2tax data/nucl_gb.accession2taxid",
                     "--nodes data/nodes.dmp",
                     "--names data/names.dmp",
                     "--ranks 'superkingdom;phylum;class;order;family;genus;species'"))
      }
      
###################     
####MERGE FILES####
###################  
      
      combined_output <- "intermediate/merged_combined_output.tax.tsv"
      
      # Build list of files to merge based on selected sources
      files_to_merge <- character()
      
      if ("ncbi" %in% selected_sources && file.exists("intermediate/merged_ncbi_output.tax.tsv")) {
        files_to_merge <- c(files_to_merge, "intermediate/merged_ncbi_output.tax.tsv")
      }
      
      if ("bold" %in% selected_sources && file.exists("intermediate/merged_bold_output.tax.tsv")) {
        files_to_merge <- c(files_to_merge, "intermediate/merged_bold_output.tax.tsv")
      }
      
      if ("midori" %in% selected_sources && file.exists("intermediate/merged_midori_output.tax.tsv")) {
        files_to_merge <- c(files_to_merge, "intermediate/merged_midori_output.tax.tsv")
      }
      
      if ("silva" %in% selected_sources && file.exists("intermediate/merged_silva_output.tax.tsv")) {
        files_to_merge <- c(files_to_merge, "intermediate/merged_silva_output.tax.tsv")
      }
      
      if (!is.null(input$custom_fasta) && file.exists("intermediate/custom_fasta_output.tax.tsv")) {
        files_to_merge <- c(files_to_merge, "intermediate/custom_fasta_output.tax.tsv")
      }
      
      if (length(files_to_merge) > 1) {
        # Multiple files - merge them
        system(paste(crabs_path,
                     "--merge --input", shQuote(paste(files_to_merge, collapse = ";")),
                     "--uniq --output", combined_output))
      } else if (length(files_to_merge) == 1) {
        # Single file - just use it directly
        combined_output <- files_to_merge[1]
      } else {
        stop("No data sources selected or available")
      }
      
###########################      
####DATABASE PROCESSING####      
###########################
      
      #Dereplicate sequences and clean output
      system(paste(crabs_path, "--dereplicate --input", combined_output, "--output intermediate/merged_combined_output.filtered.tsv --dereplication-method \'unique_species\'"))
      
      system("awk -F'\t' 'NF>=1 && length($NF)>0' intermediate/merged_combined_output.filtered.tsv > intermediate/merged_combined_output.filtered.cleaned.tsv")
      system("awk -F'\t' 'NF>=1 && length($NF)>0 {gsub(/ /,\"-\",$1); print}' OFS='\t' intermediate/merged_combined_output.filtered.cleaned.tsv > intermediate/merged_combined_output.filtered.cleaned.final.tsv")
      
      # Insilico PCR using selected primers
      system(paste(crabs_path, "--in-silico-pcr --input intermediate/merged_combined_output.filtered.cleaned.final.tsv --output intermediate/insilico_relaxed.txt --forward", shQuote(input$forward_primer), "--reverse", shQuote(input$reverse_primer), "--relaxed"))
      
      #Pairwise global alignment to find any sequences with no primers but correct region
      system(paste(crabs_path, "--pairwise-global-alignment --input intermediate/merged_combined_output.filtered.cleaned.final.tsv --amplicons intermediate/insilico_relaxed.txt --output intermediate/insilico_aligned.txt --forward", shQuote(input$forward_primer), "--reverse", shQuote(input$reverse_primer), "--size-select 10000 --percent-identity 0.95 --coverage 95"))
      
      #Further dereplication after insilico PCR
      system(paste(crabs_path, "--dereplicate --input intermediate/insilico_aligned.txt --output intermediate/merged_combined_output.tax.cleaned.tsv --dereplication-method \'unique_species\'"))
      
      #Filtering of results - possibly requires minimum length to be changed. 
      system(paste(crabs_path, "--filter", 
                   "--input intermediate/merged_combined_output.tax.cleaned.tsv", 
                   "--output intermediate/merged_combined_output.tax.cleaned.final.tsv", 
                   "--minimum-length 150",
                   "--maximum-n 5",
                   "--environmental",
                   "--no-species-id",
                   "--rank-na 2"))
      
      #Sort out issues with Sequence ID names to enable BLAST database to be constructed. 
      dt <- fread("intermediate/merged_combined_output.tax.cleaned.final.tsv", sep = "\t")
      
      dt[[1]] <- gsub(":", "-", dt[[1]])
      
      # Case-insensitive key
      key <- tolower(dt[[1]])
      
      # Count duplicates by lowercase key
      dup_index <- ave(seq_along(key), key, FUN = function(x) seq_along(x) - 1)
      
      # Build new names
      dt[[1]] <- ifelse(dup_index == 0,
                        dt[[1]],
                        paste0(dt[[1]], "_", dup_index))
      
      fwrite(dt,
             "intermediate/merged_combined_output.tax.cleaned.final.tsv",
             sep = "\t",
             quote = FALSE,
             col.names = FALSE)
      
      #Export into fasta format
      system(paste(crabs_path, "--export --input intermediate/merged_combined_output.tax.cleaned.final.tsv --output intermediate/combined.db.fasta --export-format rdp"))
      system(paste("awk '/^>/ {print $1; next}1'", shQuote("intermediate/combined.db.fasta"), ">", shQuote(file.path("databases", paste0(db_name(), ".fasta")))))
      
      #Create taxonomy file
      system("awk -F\"\\t\" '{ print $1,$3 }' intermediate/merged_combined_output.tax.cleaned.final.tsv > intermediate/taxonomy.dictionary.combined.v1.txt")
      system(paste("grep -v 'seqID'", shQuote("intermediate/taxonomy.dictionary.combined.v1.txt"), ">", shQuote(file.path("databases", paste0(db_name(), ".txt")))))
      
      #Make BLAST database
      system(paste(
        "makeblastdb",
        "-in", shQuote(file.path("databases", paste0(db_name(), ".fasta"))),
        "-parse_seqids",
        "-blastdb_version", "5",
        "-dbtype", "nucl",
        "-taxid_map", shQuote(file.path("databases", paste0(db_name(), ".txt"))),
        "-title", shQuote(file.path("databases", paste0(db_name(), ".blast.db"))),
        "-out", shQuote(file.path("databases", paste0(db_name(), ".blast.db")))
      ))


#################################     
####DELETE INTERMEDIATE FILES####
#################################
      
      
      if (.Platform$OS.type == "windows") {
        
      } else {
        
      }

      
      
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
          system("rm intermediate/insilico_*.txt")
          system("rm intermediate/*.zip")
          system("rm intermediate/ncbi_*.fasta")
          system("rm intermediate/bold_*.tsv")
          system("rm intermediate/merged_combined_output.*")
          system("rm intermediate/silva_*")
          system("rm intermediate/merged_silva_output.tax.tsv")
          system("rm intermediate/*_cleaned.fasta")  
          system("rm intermediate/merged_ncbi_output*")
          system("rm intermediate/merged_bold_output*")
          system("rm intermediate/combined.db*")
          system("rm intermediate/taxonomy.dictionary.*")
          system("rm intermediate/custom_fasta_output.*")
        }
      })      
      
      
      
      # Display results
      output$execution_output <- renderText({ paste(all_exec_output, collapse = "\n") })
      show_results(TRUE)
      removeModal()
      
      
      updateTabsetPanel(session, "main_tabs", selected = "Results")
    })
  })
  
}
shinyApp(ui, server)
